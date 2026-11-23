// The selected track's sound, as the engine will play it. Read-only until
// sound editing exists; every value shown is the engine's.
import { useEffect, useRef } from "react";
import type { Effect, Envelope, Patch, TrackView } from "./protocol";
import { shorten } from "./score";
import { describeSource } from "./TrackList";

const WAVES: Record<string, string> = { sine: "Sine", triangle: "Triangle", saw: "Saw", square: "Square", whiteNoise: "White noise", pinkNoise: "Pink noise" };
const MODES: Record<string, string> = { lowpass: "Low-pass", highpass: "High-pass", bandpass: "Band-pass", notch: "Notch" };

const hz = (f: number) => (f >= 1000 ? `${(f / 1000).toFixed(f >= 10000 ? 0 : 1)} kHz` : `${Math.round(f)} Hz`);
const ms = (s: number) => (s >= 1 ? `${s.toFixed(2)} s` : `${Math.round(s * 1000)} ms`);
const pct = (x: number) => `${Math.round(x * 100)}%`;
const signed = (x: number, unit: string) => `${x > 0 ? "+" : x < 0 ? "−" : ""}${Math.abs(x)} ${unit}`;

function effectText(e: Effect): [string, string] {
  switch (e.type) {
    case "delay":
      return ["Delay", `${ms(e.time)}, feedback ${pct(e.feedback)}, mix ${pct(e.mix)}`];
    case "reverb":
      return ["Reverb", `room ${pct(e.roomSize)}, mix ${pct(e.mix)}`];
    case "chorus":
      return ["Chorus", `${e.rate} Hz, depth ${pct(e.depth)}, mix ${pct(e.mix)}`];
    case "distortion":
      return ["Distortion", `drive ${e.drive}`];
    case "tremolo":
      return ["Tremolo", `${e.rate} Hz, depth ${pct(e.depth)}`];
  }
}

function useCanvas(draw: (g: CanvasRenderingContext2D, w: number, h: number, css: (n: string) => string) => void, deps: unknown[]) {
  const ref = useRef<HTMLCanvasElement>(null);
  useEffect(() => {
    const el = ref.current;
    if (!el) return;
    const paint = () => {
      const g = el.getContext("2d");
      if (!g) return;
      const r = window.devicePixelRatio || 1;
      el.width = Math.round(el.clientWidth * r);
      el.height = Math.round(el.clientHeight * r);
      g.scale(r, r);
      const style = getComputedStyle(el);
      draw(g, el.clientWidth, el.clientHeight, (n) => style.getPropertyValue(n).trim());
    };
    paint();
    const resize = new ResizeObserver(paint);
    resize.observe(el);
    const look = window.matchMedia("(prefers-color-scheme: dark)");
    look.addEventListener("change", paint);
    return () => {
      resize.disconnect();
      look.removeEventListener("change", paint);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps);
  return ref;
}

/** Magnitude response of the engine's two-pole state-variable filter. */
function response(p: Patch, f: number): number {
  const r = f / p.filter.cutoff;
  const q = p.filter.resonance;
  const lp = 1 / Math.sqrt((1 - r * r) ** 2 + (r / q) ** 2);
  switch (p.filter.mode) {
    case "lowpass":
      return lp;
    case "highpass":
      return lp * r * r;
    case "bandpass":
      return (lp * r) / q;
    case "notch":
      return lp * Math.abs(1 - r * r);
  }
}

function FilterCurve({ patch }: { patch: Patch }) {
  const ref = useCanvas(
    (g, w, h, css) => {
      g.font = "11px " + css("--sans");
      g.strokeStyle = css("--line");
      g.fillStyle = css("--ink-2");
      for (const f of [100, 1000, 10000]) {
        const x = (Math.log(f / 20) / Math.log(1000)) * w;
        g.beginPath();
        g.moveTo(x, 0);
        g.lineTo(x, h);
        g.stroke();
        const label = hz(f);
        const tw = g.measureText(label).width;
        g.fillText(label, x + 4 + tw > w ? x - 4 - tw : x + 4, h - 4);
      }
      g.strokeStyle = css("--tc") || css("--sel");
      g.lineWidth = 2;
      g.beginPath();
      for (let x = 0; x <= w; x++) {
        const db = Math.max(-36, 20 * Math.log10(response(patch, 20 * Math.pow(1000, x / w)) || 1e-9));
        const y = h * (0.3 - db / 54);
        if (x) g.lineTo(x, y);
        else g.moveTo(x, y);
      }
      g.stroke();
    },
    [patch],
  );
  return <canvas ref={ref} className="curve" aria-hidden="true" />;
}

function EnvelopeCurve({ env }: { env: Envelope }) {
  const ref = useCanvas(
    (g, w, h, css) => {
      const hold = Math.max(0.2, (env.attack + env.decay + env.release) * 0.4);
      const total = env.attack + env.decay + hold + env.release || 1;
      const pts: [number, number][] = [[0, 0], [env.attack, 1], [env.attack + env.decay, env.sustain], [env.attack + env.decay + hold, env.sustain], [total, 0]];
      const at = ([t, v]: [number, number]): [number, number] => [(t / total) * (w - 4) + 2, h - 3 - v * (h - 8)];
      g.beginPath();
      pts.map(at).forEach(([x, y], i) => (i ? g.lineTo(x, y) : g.moveTo(x, y)));
      const colour = css("--tc") || css("--sel");
      g.globalAlpha = 0.16;
      g.fillStyle = colour;
      g.fill();
      g.globalAlpha = 1;
      g.strokeStyle = colour;
      g.lineWidth = 2;
      g.lineJoin = "round";
      g.stroke();
    },
    [env],
  );
  return <canvas ref={ref} className="curve" aria-hidden="true" />;
}

const Row = ({ label, value }: { label: string; value: string }) => (
  <div className="value-row">
    <dt>{label}</dt>
    <dd>{value}</dd>
  </div>
);

function EnvelopeValues({ env }: { env: Envelope }) {
  return (
    <dl className="values">
      <Row label="Attack" value={ms(env.attack)} />
      <Row label="Decay" value={ms(env.decay)} />
      <Row label="Sustain" value={pct(env.sustain)} />
      <Row label="Release" value={ms(env.release)} />
    </dl>
  );
}

export function SoundView({ track, colour }: { track: TrackView | null; colour: string }) {
  if (!track) {
    return (
      <section className="sound" aria-label="Sound">
        <p className="empty">Select a track to see its sound.</p>
      </section>
    );
  }
  const p = track.patch;
  const order = [...p.effects].reverse();
  return (
    <section className="sound" aria-label={`Sound of ${track.name}`} style={{ "--tc": colour } as React.CSSProperties}>
      <header className="pane-head">
        <span className="swatch" />
        <h2 title={track.name}>{shorten(track.name, 40)}</h2>
        <span className="tag">{describeSource(track)}</span>
        <span className="grow" />
        <span className="sub">Read-only</span>
      </header>
      <div className="sound-grid">
        <section className="card wide">
          <h3>Oscillators</h3>
          <table className="osc">
            <thead>
              <tr>
                <th scope="col">Wave</th>
                <th scope="col">Level</th>
                <th scope="col">Pitch</th>
                <th scope="col">Detune</th>
              </tr>
            </thead>
            <tbody>
              {p.oscillators.map((o, i) => (
                <tr key={i}>
                  <td>{WAVES[o.waveform.type] + (o.waveform.type === "square" && o.waveform.duty !== 0.5 ? `, ${pct(o.waveform.duty)} duty` : "")}</td>
                  <td>
                    <span className="meter" style={{ "--v": o.level } as React.CSSProperties} aria-hidden="true" />
                    {pct(o.level)}
                  </td>
                  <td>{signed(o.semitones, "st")}</td>
                  <td>{signed(o.cents, "ct")}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </section>
        <section className="card">
          <h3>
            Filter <span className="sub">{MODES[p.filter.mode]}</span>
          </h3>
          <FilterCurve patch={p} />
          <dl className="values">
            <Row label="Cutoff" value={hz(p.filter.cutoff)} />
            <Row label="Resonance" value={p.filter.resonance.toFixed(1)} />
            <Row label="Sweep" value={`${p.filter.envelopeAmount >= 0 ? "+" : "−"}${hz(Math.abs(p.filter.envelopeAmount))}`} />
          </dl>
        </section>
        <section className="card">
          <h3>Loudness over a note</h3>
          <EnvelopeCurve env={p.amplitudeEnvelope} />
          <EnvelopeValues env={p.amplitudeEnvelope} />
        </section>
        <section className="card">
          <h3>Filter sweep over a note</h3>
          <EnvelopeValues env={p.filter.envelope} />
        </section>
        <section className="card">
          <h3>Effects</h3>
          {order.length === 0 ? (
            <p className="sub">None</p>
          ) : (
            <ol className="effects">
              {order.map((e, i) => {
                const [name, detail] = effectText(e);
                return (
                  <li key={i}>
                    <b>{name}</b> <span>{detail}</span>
                  </li>
                );
              })}
            </ol>
          )}
          {order.length > 1 ? <p className="sub">Applied top to bottom.</p> : null}
        </section>
      </div>
    </section>
  );
}
