// Interim transport: waveform, seek, loop, and volume. Functional only; the
// designed interface replaces it.
import { useEffect, useRef, useState } from "react";
import type { LoopRange, RenderPeaks, TransportView } from "./protocol";

/** Matches the engine's minimum (Sonora.Service.Session.minLoopSeconds). */
const MIN_LOOP_SECONDS = 0.05;

interface Props {
  transport: TransportView;
  peaks: RenderPeaks | null;
  duration: number;
  canPlay: boolean;
  onPlay: () => void;
  onPause: () => void;
  onStop: () => void;
  onSeek: (seconds: number) => void;
  onLoop: (loop: LoopRange | null) => void;
  onVolume: (volume: number) => void;
}

export const clock = (seconds: number) => {
  const tenths = Math.round(seconds * 10);
  const m = Math.floor(tenths / 600);
  const s = ((tenths % 600) / 10).toFixed(1).padStart(4, "0");
  return `${m}:${s}`;
};

/** The engine's position, carried forward between its updates while playing. */
function useSmoothPosition(transport: TransportView, duration: number): number {
  const [now, setNow] = useState(() => performance.now());
  const received = useRef({ transport, at: performance.now() });
  if (received.current.transport !== transport) received.current = { transport, at: performance.now() };

  useEffect(() => {
    if (!transport.playing) return;
    let frame = requestAnimationFrame(function tick() {
      setNow(performance.now());
      frame = requestAnimationFrame(tick);
    });
    return () => cancelAnimationFrame(frame);
  }, [transport.playing]);

  if (!transport.playing) return transport.positionSeconds;
  // Never run further ahead than a few missed updates.
  const ahead = Math.min(0.2, Math.max(0, (now - received.current.at) / 1000));
  const end = transport.loop && transport.loopActive && transport.positionSeconds < transport.loop.end ? transport.loop.end : duration;
  return Math.min(end, transport.positionSeconds + ahead);
}

function Waveform({ peaks, position, duration, loop }: { peaks: RenderPeaks | null; position: number; duration: number; loop: LoopRange | null }) {
  const canvas = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    const el = canvas.current;
    const g = el?.getContext("2d");
    if (!el || !g) return;
    const ratio = window.devicePixelRatio || 1;
    const w = el.clientWidth;
    const h = el.clientHeight;
    el.width = Math.round(w * ratio);
    el.height = Math.round(h * ratio);
    g.scale(ratio, ratio);
    const color = getComputedStyle(el).color;
    if (loop && duration > 0) {
      g.globalAlpha = 0.2;
      g.fillStyle = color;
      const x = (loop.start / duration) * w;
      g.fillRect(x, 0, (Math.min(loop.end, duration) / duration) * w - x, h);
      g.globalAlpha = 1;
    }
    g.fillStyle = color;
    const data = peaks?.peaks ?? [];
    data.forEach(([lo, hi], i) => {
      const x = (i / data.length) * w;
      const top = (1 - hi) * (h / 2);
      g.fillRect(x, top, Math.max(1, w / data.length), Math.max(1, (hi - lo) * (h / 2)));
    });
  }, [peaks, duration, loop]);

  return (
    <div className="waveform">
      <canvas ref={canvas} aria-hidden="true" />
      {duration > 0 ? <div className="playhead" style={{ left: `${Math.min(100, (position / duration) * 100)}%` }} /> : null}
    </div>
  );
}

export function Transport(props: Props) {
  const { transport, duration, canPlay } = props;
  const position = useSmoothPosition(transport, duration);
  const loop = transport.loop;
  const [from, setFrom] = useState(0);
  const loopEnd = Math.max(position, from + MIN_LOOP_SECONDS);

  return (
    <div className="transport">
      <Waveform peaks={props.peaks} position={position} duration={duration} loop={transport.loopActive ? loop : null} />
      <input
        type="range"
        aria-label="Position"
        aria-valuetext={`${clock(position)} of ${clock(duration)}`}
        min={0}
        max={duration}
        step={0.01}
        value={Math.min(position, duration)}
        disabled={!canPlay}
        onChange={(e) => props.onSeek(Number(e.target.value))}
      />
      <div className="actions">
        {transport.playing ? (
          <button type="button" onClick={props.onPause}>
            Pause
          </button>
        ) : (
          <button type="button" onClick={props.onPlay} disabled={!canPlay}>
            Play
          </button>
        )}
        <button type="button" onClick={props.onStop} disabled={!canPlay}>
          Stop
        </button>
        <span data-testid="clock">
          {clock(position)} / {clock(duration)}
        </span>
        <button type="button" disabled={!canPlay} onClick={() => setFrom(position)}>
          Loop from here
        </button>
        <button type="button" disabled={!canPlay} onClick={() => props.onLoop({ start: from, end: loopEnd })}>
          Loop to here
        </button>
        {loop ? (
          <button type="button" onClick={() => props.onLoop(null)}>
            Clear loop {clock(loop.start)}–{clock(loop.end)}
            {transport.loopActive ? "" : " (outside the song)"}
          </button>
        ) : null}
        <label>
          Volume{" "}
          <input
            type="range"
            min={0}
            max={1}
            step={0.01}
            value={transport.volume}
            onChange={(e) => props.onVolume(Number(e.target.value))}
          />
        </label>
      </div>
      {transport.deviceLost ? (
        <p className="error" role="alert">
          The audio device was disconnected. Press Play to continue on the current output.
        </p>
      ) : null}
    </div>
  );
}
