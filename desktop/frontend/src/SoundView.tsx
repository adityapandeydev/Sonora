// The selected track's sound: edit it, hear single notes of it, and see
// whether the song plays it yet. While a control is dragged the change is
// only heard in the audition; letting go applies it to the track once.
import { useCallback, useEffect, useRef, useState, type KeyboardEvent as ReactKeyboardEvent } from "react";
import type { Effect, Envelope, Patch, TrackView } from "./protocol";
import { shorten } from "./score";
import {
  addEffect,
  addOscillator,
  EFFECT_TYPES,
  hz,
  isNoise,
  KEY_NOTES,
  MAX_EFFECTS,
  MAX_OSCILLATORS,
  moveEffect,
  newEffect,
  noteName,
  pct,
  processing,
  RANGES,
  removeEffect,
  removeOscillator,
  sameSound,
  seconds,
  signed,
  signedHz,
  waveform,
  withEffect,
  withEnvelope,
  withFilter,
  withOscillator,
} from "./sound";
import { describeSource } from "./TrackList";
import { DownIcon, RemoveIcon, UpIcon } from "./ui/icons";
import type { Range } from "./ui/range";
import { Slider } from "./ui/Slider";
import { useAudition } from "./useAudition";

const WAVES: Record<string, string> = { sine: "Sine", triangle: "Triangle", saw: "Saw", square: "Square", whiteNoise: "White noise", pinkNoise: "Pink noise" };
const MODES: Record<string, string> = { lowpass: "Low-pass", highpass: "High-pass", bandpass: "Band-pass", notch: "Notch" };
const EFFECT_NAMES: Record<Effect["type"], string> = { delay: "Delay", reverb: "Reverb", chorus: "Chorus", distortion: "Distortion", tremolo: "Tremolo" };

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
        const label = f >= 1000 ? `${f / 1000} kHz` : `${f} Hz`;
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
    [patch.filter],
  );
  return <canvas ref={ref} className="curve" aria-hidden="true" />;
}

function EnvelopeCurve({ env }: { env: Envelope }) {
  const ref = useCanvas(
    (g, w, h, css) => {
      const hold = Math.max(0.2, (env.attack + env.decay + env.release) * 0.4);
      const total = env.attack + env.decay + hold + env.release || 1;
      const steps = 120;
      const level = (t: number) => {
        const shape = (x: number) => (env.curve === "exponential" ? x * x : x);
        if (t < env.attack) return shape(t / (env.attack || 1));
        if (t < env.attack + env.decay) {
          const x = (t - env.attack) / (env.decay || 1);
          return 1 - (env.curve === "exponential" ? x * (2 - x) : x) * (1 - env.sustain);
        }
        const gate = env.attack + env.decay + hold;
        if (t < gate) return env.sustain;
        const x = Math.min(1, (t - gate) / (env.release || 1));
        return env.sustain * (env.curve === "exponential" ? (1 - x) * (1 - x) : 1 - x);
      };
      g.beginPath();
      for (let i = 0; i <= steps; i++) {
        const t = (i / steps) * total;
        const x = (t / total) * (w - 4) + 2;
        const y = h - 3 - level(t) * (h - 8);
        if (i) g.lineTo(x, y);
        else g.moveTo(x, y);
      }
      const colour = css("--tc") || css("--sel");
      g.strokeStyle = colour;
      g.lineWidth = 2;
      g.lineJoin = "round";
      g.stroke();
      g.lineTo(w - 2, h - 3);
      g.lineTo(2, h - 3);
      g.globalAlpha = 0.16;
      g.fillStyle = colour;
      g.fill();
      g.globalAlpha = 1;
    },
    [env],
  );
  return <canvas ref={ref} className="curve" aria-hidden="true" />;
}

/** Edits flow through here: `live` while dragging, `apply` once when done. */
type Edit = (next: Patch, apply: boolean) => void;

function Control(props: { label: string; value: number; range: Range; reset: number; format: (v: number) => string; origin?: number; disabled?: boolean; to: (v: number) => Patch; edit: Edit }) {
  return (
    <Slider
      label={props.label}
      value={props.value}
      range={props.range}
      defaultValue={props.reset}
      origin={props.origin}
      format={props.format}
      disabled={props.disabled}
      onInput={(v) => props.edit(props.to(v), false)}
      onCommit={(v) => props.edit(props.to(v), true)}
    />
  );
}

function Choice<T extends string>(props: { label: string; value: T; options: Record<string, string>; onChange: (v: T) => void }) {
  return (
    <label className="choice">
      <span>{props.label}</span>
      <select value={props.value} onChange={(e) => props.onChange(e.target.value as T)}>
        {Object.entries(props.options).map(([k, v]) => (
          <option key={k} value={k}>
            {v}
          </option>
        ))}
      </select>
    </label>
  );
}

function EnvelopeControls({ which, env, patch, edit }: { which: "amplitude" | "filter"; env: Envelope; patch: Patch; edit: Edit }) {
  const set = (f: (e: Envelope) => Envelope) => withEnvelope(patch, which, f);
  return (
    <div className="controls">
      <Control label="Attack" value={env.attack} range={RANGES.attack} reset={0.01} format={seconds} to={(v) => set((e) => ({ ...e, attack: v }))} edit={edit} />
      <Control label="Decay" value={env.decay} range={RANGES.decay} reset={0.1} format={seconds} to={(v) => set((e) => ({ ...e, decay: v }))} edit={edit} />
      <Control label="Sustain" value={env.sustain} range={RANGES.sustain} reset={0.7} format={pct} to={(v) => set((e) => ({ ...e, sustain: v }))} edit={edit} />
      <Control label="Release" value={env.release} range={RANGES.release} reset={0.2} format={seconds} to={(v) => set((e) => ({ ...e, release: v }))} edit={edit} />
      <Choice label="Curve" value={env.curve} options={{ linear: "Straight", exponential: "Curved" }} onChange={(c) => edit(set((e) => ({ ...e, curve: c })), true)} />
    </div>
  );
}

function EffectControls({ effect, at, patch, edit }: { effect: Effect; at: number; patch: Patch; edit: Edit }) {
  const set = (f: (e: Effect) => Effect) => withEffect(patch, at, f);
  const defaults = newEffect(effect.type);
  switch (effect.type) {
    case "delay": {
      const d = defaults as typeof effect;
      return (
        <>
          <Control label="Time" value={effect.time} range={RANGES.delayTime} reset={d.time} format={seconds} to={(v) => set(() => ({ ...effect, time: v }))} edit={edit} />
          <Control label="Feedback" value={effect.feedback} range={RANGES.feedback} reset={d.feedback} format={pct} to={(v) => set(() => ({ ...effect, feedback: v }))} edit={edit} />
          <Control label="Mix" value={effect.mix} range={RANGES.mix} reset={d.mix} format={pct} to={(v) => set(() => ({ ...effect, mix: v }))} edit={edit} />
        </>
      );
    }
    case "reverb": {
      const d = defaults as typeof effect;
      return (
        <>
          <Control label="Room" value={effect.roomSize} range={RANGES.roomSize} reset={d.roomSize} format={pct} to={(v) => set(() => ({ ...effect, roomSize: v }))} edit={edit} />
          <Control label="Mix" value={effect.mix} range={RANGES.mix} reset={d.mix} format={pct} to={(v) => set(() => ({ ...effect, mix: v }))} edit={edit} />
        </>
      );
    }
    case "chorus": {
      const d = defaults as typeof effect;
      return (
        <>
          <Control label="Rate" value={effect.rate} range={RANGES.chorusRate} reset={d.rate} format={(v) => `${v} Hz`} to={(v) => set(() => ({ ...effect, rate: v }))} edit={edit} />
          <Control label="Depth" value={effect.depth} range={RANGES.depth} reset={d.depth} format={pct} to={(v) => set(() => ({ ...effect, depth: v }))} edit={edit} />
          <Control label="Mix" value={effect.mix} range={RANGES.mix} reset={d.mix} format={pct} to={(v) => set(() => ({ ...effect, mix: v }))} edit={edit} />
        </>
      );
    }
    case "distortion": {
      const d = defaults as typeof effect;
      return <Control label="Drive" value={effect.drive} range={RANGES.drive} reset={d.drive} format={(v) => `${v}×`} to={(v) => set(() => ({ ...effect, drive: v }))} edit={edit} />;
    }
    case "tremolo": {
      const d = defaults as typeof effect;
      return (
        <>
          <Control label="Rate" value={effect.rate} range={RANGES.tremoloRate} reset={d.rate} format={(v) => `${v} Hz`} to={(v) => set(() => ({ ...effect, rate: v }))} edit={edit} />
          <Control label="Depth" value={effect.depth} range={RANGES.depth} reset={d.depth} format={pct} to={(v) => set(() => ({ ...effect, depth: v }))} edit={edit} />
        </>
      );
    }
  }
}

const BLACK = new Set([1, 3, 6, 8, 10]);

function Keys({ base, held, onDown, onUp, onOctave }: { base: number; held: number | null; onDown: (p: number) => void; onUp: (p: number) => void; onOctave: (by: -1 | 1) => void }) {
  const pitches = Array.from({ length: 13 }, (_, i) => base + i);
  const whites = pitches.filter((p) => !BLACK.has(p % 12));
  const keyProps = (p: number) => ({
    type: "button" as const,
    "aria-label": noteName(p),
    "aria-pressed": held === p,
    tabIndex: -1,
    onPointerDown: (e: React.PointerEvent<HTMLButtonElement>) => {
      if (e.button !== 0) return;
      try {
        // Keep the release on this key even if the pointer slides off it.
        e.currentTarget.setPointerCapture(e.pointerId);
      } catch {
        /* the pointer is already gone; the press still counts */
      }
      onDown(p);
    },
    onPointerUp: () => onUp(p),
    onPointerCancel: () => onUp(p),
  });
  return (
    <div className="keys-row">
      <button type="button" className="button small" onClick={() => onOctave(-1)} disabled={base <= 24} aria-label="Octave down" title="Octave down (Z)">
        −
      </button>
      <div className="keys" role="group" aria-label={`Play notes from ${noteName(base)}. Keys A to K play them; hold to sustain.`}>
        {whites.map((p) => (
          <button key={p} className="key white" {...keyProps(p)}>
            {p % 12 === 0 ? <span>{noteName(p)}</span> : null}
          </button>
        ))}
        {pitches
          .filter((p) => BLACK.has(p % 12))
          .map((p) => {
            const whiteBefore = whites.filter((w) => w < p).length;
            return <button key={p} className="key black" style={{ "--at": whiteBefore } as React.CSSProperties} {...keyProps(p)} />;
          })}
      </div>
      <button type="button" className="button small" onClick={() => onOctave(1)} disabled={base >= 96} aria-label="Octave up" title="Octave up (X)">
        +
      </button>
    </div>
  );
}

export type SoundStatus = "rendering" | "rendered" | "unrendered";

interface Props {
  track: TrackView | null;
  colour: string;
  /** Whether the song's latest render includes the project as it is. */
  songStatus: SoundStatus;
  onApply: (track: TrackView, patch: Patch) => Promise<unknown>;
  onReset: (track: TrackView) => Promise<unknown>;
}

export function SoundView({ track, colour, songStatus, onApply, onReset }: Props) {
  const [draft, setDraft] = useState<Patch | null>(null);
  const [applying, setApplying] = useState(0);
  const [problem, setProblem] = useState<string | null>(null);
  const [base, setBase] = useState(60);
  const audition = useAudition(setProblem);

  // Another track, another sound.
  const trackId = track?.id;
  useEffect(() => {
    setDraft(null);
    setProblem(null);
  }, [trackId]);

  const shown = draft ?? track?.patch ?? null;
  const shownRef = useRef(shown);
  shownRef.current = shown;

  const edit: Edit = useCallback(
    (next, apply) => {
      if (!track) return;
      setDraft(next);
      setProblem(null);
      audition.update(next);
      if (!apply) return;
      setApplying((n) => n + 1);
      onApply(track, next)
        .then(() => setDraft((d) => (d === next ? null : d)))
        .catch((e) => {
          setProblem(e instanceof Error ? e.message : String(e));
          setDraft((d) => (d === next ? null : d));
        })
        .finally(() => setApplying((n) => n - 1));
    },
    [track, onApply, audition],
  );

  const down = (pitch: number) => {
    if (shownRef.current) audition.start(shownRef.current, pitch);
  };

  // Computer keys play notes while focus is in the sound panel, outside
  // text fields and menus.
  const keys = (e: ReactKeyboardEvent) => {
    const t = e.target as HTMLElement;
    if (e.ctrlKey || e.metaKey || e.altKey || t.closest("select, input:not([type=range]), textarea")) return;
    const k = e.key.toLowerCase();
    const i = KEY_NOTES.indexOf(k);
    if (e.type === "keydown") {
      if (i >= 0) {
        e.preventDefault();
        if (!e.repeat) down(base + i);
      } else if (k === "z" || k === "x") {
        e.preventDefault();
        setBase((b) => Math.max(24, Math.min(96, b + (k === "z" ? -12 : 12))));
      }
    } else if (i >= 0) {
      audition.release(base + i);
    }
  };

  if (!track || !shown) {
    return (
      <section className="sound" aria-label="Sound">
        <p className="empty">Select a track to hear and shape its sound.</p>
      </section>
    );
  }

  const p = shown;
  const order = processing(p);
  const changed = draft !== null && !sameSound(draft, track.patch);
  const status =
    changed && applying === 0
      ? "Previewing, not applied yet"
      : applying > 0
        ? "Applying"
        : songStatus === "rendering"
          ? track.hasPatchOverride
            ? "Applied. The song is rendering with it"
            : "The song is rendering"
          : songStatus === "unrendered"
            ? track.hasPatchOverride
              ? "Applied. The song is not rendered with it yet"
              : "The song is not rendered yet"
            : "The song plays this sound";

  return (
    <section className="sound" aria-label={`Sound of ${track.name}`} style={{ "--tc": colour } as React.CSSProperties} onKeyDown={keys} onKeyUp={keys}>
      <header className="pane-head">
        <span className="swatch" />
        <h2 title={track.name}>{shorten(track.name, 32)}</h2>
        <span className="tag">{describeSource(track)}</span>
        <span className="grow" />
        <span className="sub sound-status" role="status">
          {status}
        </span>
        {track.hasPatchOverride ? (
          <button
            type="button"
            className="button small"
            title="Go back to the sound the score asks for"
            onClick={() => {
              setDraft(null);
              onReset(track).catch((e) => setProblem(e instanceof Error ? e.message : String(e)));
            }}
          >
            Use score sound
          </button>
        ) : null}
      </header>
      <Keys base={base} held={audition.held} onDown={down} onUp={audition.release} onOctave={(by) => setBase((b) => b + by * 12)} />
      {problem ? (
        <p className="field-problem sound-problem" role="alert">
          {problem}
        </p>
      ) : null}
      <div className="sound-grid">
        <section className="card wide">
          <h3>
            Oscillators <span className="sub">{p.oscillators.length} of {MAX_OSCILLATORS}</span>
          </h3>
          {p.oscillators.map((o, i) => {
            const set = (f: (x: typeof o) => typeof o) => withOscillator(p, i, f);
            const noise = isNoise(o.waveform);
            return (
              <div className="osc-row" key={i}>
                <div className="osc-head">
                  <Choice label={`Oscillator ${i + 1}`} value={o.waveform.type} options={WAVES} onChange={(t) => edit(set((x) => ({ ...x, waveform: waveform(t, x.waveform) })), true)} />
                  <button type="button" className="button small square" aria-label={`Remove oscillator ${i + 1}`} disabled={p.oscillators.length <= 1} onClick={() => edit(removeOscillator(p, i), true)}>
                    <RemoveIcon />
                  </button>
                </div>
                <div className="controls">
                  <Control label="Level" value={o.level} range={RANGES.level} reset={0.7} format={pct} to={(v) => set((x) => ({ ...x, level: v }))} edit={edit} />
                  <Control label="Pitch" value={o.semitones} range={RANGES.semitones} reset={0} origin={0} format={signed("st")} disabled={noise} to={(v) => set((x) => ({ ...x, semitones: v }))} edit={edit} />
                  <Control label="Detune" value={o.cents} range={RANGES.cents} reset={0} origin={0} format={signed("ct")} disabled={noise} to={(v) => set((x) => ({ ...x, cents: v }))} edit={edit} />
                  {o.waveform.type === "square" ? (
                    <Control label="Width" value={o.waveform.duty} range={RANGES.duty} reset={0.5} format={pct} to={(v) => set((x) => ({ ...x, waveform: { type: "square", duty: v } }))} edit={edit} />
                  ) : null}
                  {noise ? <p className="sub">Noise has no pitch.</p> : null}
                </div>
              </div>
            );
          })}
          <div>
            <button type="button" className="button small" disabled={p.oscillators.length >= MAX_OSCILLATORS} onClick={() => edit(addOscillator(p), true)}>
              Add oscillator
            </button>
          </div>
        </section>
        <section className="card">
          <h3>Filter</h3>
          <Choice label="Type" value={p.filter.mode} options={MODES} onChange={(m) => edit(withFilter(p, (f) => ({ ...f, mode: m })), true)} />
          <FilterCurve patch={p} />
          <div className="controls">
            <Control label="Cutoff" value={p.filter.cutoff} range={RANGES.cutoff} reset={2500} format={hz} to={(v) => withFilter(p, (f) => ({ ...f, cutoff: v }))} edit={edit} />
            <Control label="Resonance" value={p.filter.resonance} range={RANGES.resonance} reset={1} format={(v) => v.toFixed(1)} to={(v) => withFilter(p, (f) => ({ ...f, resonance: v }))} edit={edit} />
            <Control label="Sweep" value={p.filter.envelopeAmount} range={RANGES.sweep} reset={0} origin={0} format={signedHz} to={(v) => withFilter(p, (f) => ({ ...f, envelopeAmount: v }))} edit={edit} />
          </div>
        </section>
        <section className="card">
          <h3>Loudness over a note</h3>
          <EnvelopeCurve env={p.amplitudeEnvelope} />
          <EnvelopeControls which="amplitude" env={p.amplitudeEnvelope} patch={p} edit={edit} />
        </section>
        <section className="card">
          <h3>Filter sweep over a note</h3>
          <EnvelopeCurve env={p.filter.envelope} />
          <EnvelopeControls which="filter" env={p.filter.envelope} patch={p} edit={edit} />
          {p.filter.envelopeAmount === 0 ? <p className="sub">Sweep is 0, so this shape has no effect.</p> : null}
        </section>
        <section className="card">
          <h3>
            Effects <span className="sub">in the order they process the sound</span>
          </h3>
          {order.length === 0 ? <p className="sub">None</p> : null}
          <ol className="effect-list">
            {order.map((e, at) => (
              <li key={`${at}-${e.type}`}>
                <div className="osc-head">
                  <b>
                    {at + 1}. {EFFECT_NAMES[e.type]}
                  </b>
                  <span className="grow" />
                  <button type="button" className="button small square" aria-label={`Move ${EFFECT_NAMES[e.type]} earlier`} disabled={at === 0} onClick={() => edit(moveEffect(p, at, -1), true)}>
                    <UpIcon />
                  </button>
                  <button type="button" className="button small square" aria-label={`Move ${EFFECT_NAMES[e.type]} later`} disabled={at === order.length - 1} onClick={() => edit(moveEffect(p, at, 1), true)}>
                    <DownIcon />
                  </button>
                  <button type="button" className="button small square" aria-label={`Remove ${EFFECT_NAMES[e.type]}`} onClick={() => edit(removeEffect(p, at), true)}>
                    <RemoveIcon />
                  </button>
                </div>
                <div className="controls">
                  <EffectControls effect={e} at={at} patch={p} edit={edit} />
                </div>
              </li>
            ))}
          </ol>
          <label className="choice">
            <span>Add effect</span>
            <select
              value=""
              disabled={p.effects.length >= MAX_EFFECTS}
              onChange={(ev) => {
                if (ev.target.value) edit(addEffect(p, ev.target.value as Effect["type"]), true);
              }}
            >
              <option value="">{p.effects.length >= MAX_EFFECTS ? `At most ${MAX_EFFECTS}` : "Choose…"}</option>
              {EFFECT_TYPES.map((t) => (
                <option key={t} value={t}>
                  {EFFECT_NAMES[t]}
                </option>
              ))}
            </select>
          </label>
        </section>
      </div>
    </section>
  );
}
