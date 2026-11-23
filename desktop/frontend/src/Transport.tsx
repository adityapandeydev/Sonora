// The timeline strip and transport bar at the bottom of the window.
import { useEffect, useRef, useState, type KeyboardEvent, type PointerEvent } from "react";
import type { LoopRange, RenderPeaks, TransportView } from "./protocol";
import { carryForward, clock, MIN_LOOP_SECONDS, moveLoop } from "./playback";
import { LoopIcon, PauseIcon, PlayIcon, StopIcon, VolumeIcon } from "./ui/icons";
import { Slider } from "./ui/Slider";

/** One track's row in the timeline. */
export interface Lane {
  id: string;
  /** A CSS colour, such as var(--track-0). */
  colour: string;
  /** [start s, end s, MIDI pitch, line] */
  notes: [number, number, number, number][];
  selected: boolean;
}

const LANE_HEIGHT = 12;
/** All lanes together never take more than this; rows get thinner instead. */
const LANES_MAX = 96;

function Lanes({ lanes, duration }: { lanes: Lane[]; duration: number }) {
  const canvas = useRef<HTMLCanvasElement>(null);
  useEffect(() => {
    const el = canvas.current;
    if (!el) return;
    const draw = () => {
      const g = el.getContext("2d");
      if (!g) return;
      const r = window.devicePixelRatio || 1;
      const w = el.clientWidth;
      el.width = Math.round(w * r);
      el.height = Math.round(el.clientHeight * r);
      g.scale(r, r);
      const style = getComputedStyle(el);
      const row = Math.min(LANE_HEIGHT, LANES_MAX / lanes.length);
      lanes.forEach((lane, i) => {
        const y = i * row;
        if (lane.selected) {
          g.fillStyle = style.getPropertyValue("--sel-soft");
          g.fillRect(0, y, w, row);
        }
        if (lane.notes.length === 0 || duration <= 0) return;
        const pitches = lane.notes.map((n) => n[2]);
        const lo = Math.min(...pitches);
        const hi = Math.max(...pitches);
        g.fillStyle = style.getPropertyValue(lane.colour.slice(4, -1)) || style.color;
        for (const [s, e, p] of lane.notes) {
          const x = (s / duration) * w;
          const pad = row >= 8 ? 2 : 1;
          const bar = row >= 8 ? 2 : 1;
          const yy = y + pad + (hi === lo ? (row - 2 * pad - bar) / 2 : ((hi - p) / (hi - lo)) * (row - 2 * pad - bar));
          g.fillRect(x, yy, Math.max(1.5, ((e - s) / duration) * w - 1), bar);
        }
      });
    };
    draw();
    const resize = new ResizeObserver(draw);
    resize.observe(el);
    const look = window.matchMedia("(prefers-color-scheme: dark)");
    look.addEventListener("change", draw);
    return () => {
      resize.disconnect();
      look.removeEventListener("change", draw);
    };
  }, [lanes, duration]);
  if (lanes.length === 0) return null;
  return <canvas ref={canvas} className="lanes" style={{ height: Math.min(lanes.length * LANE_HEIGHT, LANES_MAX) }} aria-hidden="true" />;
}

interface Props {
  transport: TransportView;
  peaks: RenderPeaks | null;
  lanes: Lane[];
  duration: number;
  canPlay: boolean;
  /** What the render is doing, in words. */
  status: string;
  onPlay: () => void;
  onPause: () => void;
  onStop: () => void;
  onSeek: (seconds: number) => void;
  onLoop: (loop: LoopRange | null) => void;
  onVolume: (volume: number) => void;
}

const VOLUME = { min: 0, max: 1, step: 0.01 };
const percent = (v: number) => `${Math.round(v * 100)}%`;

/** The engine's position, carried forward between its updates while playing. */
export function usePosition(transport: TransportView, duration: number): number {
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
  const elapsed = Math.min(0.2, (now - received.current.at) / 1000);
  return carryForward(transport.positionSeconds, elapsed, duration, transport.loopActive ? transport.loop : null);
}

function Waveform({ peaks }: { peaks: RenderPeaks | null }) {
  const canvas = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    const el = canvas.current;
    if (!el) return;
    const draw = () => {
      const g = el.getContext("2d");
      if (!g) return;
      const ratio = window.devicePixelRatio || 1;
      const w = el.clientWidth;
      const h = el.clientHeight;
      el.width = Math.round(w * ratio);
      el.height = Math.round(h * ratio);
      g.scale(ratio, ratio);
      g.fillStyle = getComputedStyle(el).color;
      const data = peaks?.peaks ?? [];
      if (data.length === 0) return;
      for (let x = 0; x < w; x++) {
        const from = Math.floor((x / w) * data.length);
        const to = Math.max(from + 1, Math.floor(((x + 1) / w) * data.length));
        let lo = 1;
        let hi = -1;
        for (let i = from; i < to; i++) {
          lo = Math.min(lo, data[i][0]);
          hi = Math.max(hi, data[i][1]);
        }
        g.fillRect(x, (1 - hi) * (h / 2), 1, Math.max(1, (hi - lo) * (h / 2)));
      }
    };
    draw();
    const resize = new ResizeObserver(draw);
    resize.observe(el);
    const look = window.matchMedia("(prefers-color-scheme: dark)");
    look.addEventListener("change", draw);
    return () => {
      resize.disconnect();
      look.removeEventListener("change", draw);
    };
  }, [peaks]);

  return <canvas ref={canvas} className="waveform" aria-hidden="true" />;
}

interface TimelineProps {
  lanes: Lane[];
  position: number;
  duration: number;
  peaks: RenderPeaks | null;
  loop: LoopRange | null;
  onSeek: (seconds: number) => void;
  onLoop: (loop: LoopRange) => void;
}

function Timeline({ lanes, position, duration, peaks, loop, onSeek, onLoop }: TimelineProps) {
  const strip = useRef<HTMLDivElement>(null);
  // The loop while one of its ends is being dragged. Also kept in a ref, as
  // the pointer can be released before the last move has been rendered.
  const [draft, showDraft] = useState<LoopRange | null>(null);
  const drafted = useRef<LoopRange | null>(null);
  const setDraft = (next: LoopRange | null) => {
    drafted.current = next;
    showDraft(next);
  };
  const grabbed = useRef<"seek" | "start" | "end" | null>(null);
  const shownLoop = draft ?? loop;
  const enabled = duration > 0;
  const at = (seconds: number) => `${(Math.min(Math.max(seconds, 0), duration) / (duration || 1)) * 100}%`;

  const secondsAt = (e: PointerEvent) => {
    const box = strip.current!.getBoundingClientRect();
    return Math.min(1, Math.max(0, (e.clientX - box.left) / box.width)) * duration;
  };

  const down = (what: "seek" | "start" | "end") => (e: PointerEvent) => {
    if (!enabled || e.button !== 0 || grabbed.current) return;
    e.stopPropagation();
    grabbed.current = what;
    strip.current!.setPointerCapture(e.pointerId);
    if (what === "seek") onSeek(secondsAt(e));
    else setDraft(loop);
  };
  const move = (e: PointerEvent) => {
    const what = grabbed.current;
    if (!what) return;
    if (what === "seek") onSeek(secondsAt(e));
    else if (drafted.current) setDraft(moveLoop(drafted.current, what, secondsAt(e), duration));
  };
  const up = () => {
    if (grabbed.current && grabbed.current !== "seek" && drafted.current) onLoop(drafted.current);
    grabbed.current = null;
    setDraft(null);
  };

  const keySeek = (e: KeyboardEvent) => {
    const target = {
      ArrowLeft: position - 1,
      ArrowRight: position + 1,
      PageDown: position - duration / 10,
      PageUp: position + duration / 10,
      Home: 0,
      End: duration,
    }[e.key];
    if (target === undefined) return;
    e.preventDefault();
    onSeek(Math.min(duration, Math.max(0, target)));
  };
  const keyLoop = (which: "start" | "end") => (e: KeyboardEvent) => {
    const step = { ArrowLeft: -0.05, ArrowRight: 0.05, PageDown: -1, PageUp: 1 }[e.key];
    if (step === undefined || !loop) return;
    e.preventDefault();
    e.stopPropagation();
    onLoop(moveLoop(loop, which, loop[which] + step, duration));
  };

  return (
    <div
      ref={strip}
      className="timeline"
      role="slider"
      tabIndex={enabled ? 0 : -1}
      aria-label="Position"
      aria-disabled={!enabled}
      aria-valuemin={0}
      aria-valuemax={Math.round(duration * 10) / 10}
      aria-valuenow={Math.round(position * 10) / 10}
      aria-valuetext={`${clock(position)} of ${clock(duration)}`}
      onPointerDown={down("seek")}
      onPointerMove={move}
      onPointerUp={up}
      onPointerCancel={up}
      onLostPointerCapture={up}
      onKeyDown={keySeek}
    >
      {shownLoop ? (
        <div className="loop-range" style={{ left: at(shownLoop.start), right: `calc(100% - ${at(shownLoop.end)})` }} />
      ) : null}
      <Lanes lanes={lanes} duration={duration} />
      <Waveform peaks={peaks} />
      {shownLoop
        ? (["start", "end"] as const).map((which) => (
            <div
              key={which}
              className={`loop-end ${which}`}
              style={{ left: at(shownLoop[which]) }}
              role="slider"
              tabIndex={0}
              aria-label={which === "start" ? "Loop start" : "Loop end"}
              aria-valuemin={0}
              aria-valuemax={Math.round(duration * 10) / 10}
              aria-valuenow={Math.round(shownLoop[which] * 100) / 100}
              aria-valuetext={clock(shownLoop[which])}
              onPointerDown={down(which)}
              onKeyDown={keyLoop(which)}
            />
          ))
        : null}
      {enabled ? <div className="playhead" style={{ left: at(position) }} /> : null}
    </div>
  );
}

export function Transport(props: Props) {
  const { transport, duration, canPlay } = props;
  const position = usePosition(transport, duration);

  return (
    <>
      <Timeline
        lanes={props.lanes}
        position={position}
        duration={duration}
        peaks={props.peaks}
        loop={transport.loopActive ? transport.loop : null}
        onSeek={props.onSeek}
        onLoop={props.onLoop}
      />
      <footer className="transport-bar">
        <button
          type="button"
          className="button primary"
          aria-label={transport.playing ? "Pause" : "Play"}
          title={`${transport.playing ? "Pause" : "Play"} (Space or Ctrl+Space)`}
          disabled={!canPlay}
          onClick={transport.playing ? props.onPause : props.onPlay}
        >
          {transport.playing ? <PauseIcon /> : <PlayIcon />}
        </button>
        <button type="button" className="button square" aria-label="Stop" title="Stop (Ctrl+.)" disabled={!canPlay} onClick={props.onStop}>
          <StopIcon />
        </button>
        <button
          type="button"
          className="button square"
          aria-label="Loop"
          title="Loop the whole song. Ctrl+L loops the bar at the caret."
          aria-pressed={transport.loopActive}
          disabled={!canPlay || duration < MIN_LOOP_SECONDS}
          onClick={() => props.onLoop(transport.loopActive ? null : { start: 0, end: duration })}
        >
          <LoopIcon />
        </button>
        <span className="clock" data-testid="clock">
          {clock(position)} <span>/ {clock(duration)}</span>
        </span>
        <span className="status" role="status">
          {transport.deviceLost ? (
            <span className="problem">Audio device disconnected. Press Play to use the current one.</span>
          ) : (
            props.status
          )}
        </span>
        <div className="volume">
          <Slider
            label="Volume"
            icon={<VolumeIcon />}
            value={transport.volume}
            range={VOLUME}
            defaultValue={1}
            format={percent}
            onInput={props.onVolume}
          />
        </div>
      </footer>
    </>
  );
}
