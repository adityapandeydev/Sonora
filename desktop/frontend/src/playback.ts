// Transport maths that needs no DOM.
import type { LoopRange } from "./protocol";

/** Matches the engine's minimum (Sonora.Service.Session.minLoopSeconds). */
export const MIN_LOOP_SECONDS = 0.05;

export const clock = (seconds: number): string => {
  const tenths = Math.round(seconds * 10);
  return `${Math.floor(tenths / 600)}:${((tenths % 600) / 10).toFixed(1).padStart(4, "0")}`;
};

/**
 * Where playback is now, carried forward from the engine's last update.
 * Inside an active loop it wraps; otherwise it stops at the end.
 */
export function carryForward(position: number, elapsed: number, duration: number, loop: LoopRange | null): number {
  const next = position + Math.max(0, elapsed);
  const end = loop ? Math.min(loop.end, duration) : duration;
  if (loop && position < end && next >= end && end > loop.start) {
    return loop.start + ((next - end) % (end - loop.start));
  }
  return Math.min(duration, next);
}

/** Move one end of a loop, keeping it inside the song and long enough. */
export function moveLoop(loop: LoopRange, which: "start" | "end", seconds: number, duration: number): LoopRange {
  const round = (x: number) => Math.round(x * 1000) / 1000;
  if (which === "start") {
    return { start: round(Math.max(0, Math.min(seconds, loop.end - MIN_LOOP_SECONDS))), end: loop.end };
  }
  return { start: loop.start, end: round(Math.min(Math.max(duration, loop.end), Math.max(seconds, loop.start + MIN_LOOP_SECONDS))) };
}
