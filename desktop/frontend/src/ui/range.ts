// Maths for sliders: mapping a value to a position along the track and back,
// and stepping it from the keyboard.

export interface Range {
  min: number;
  max: number;
  /** Keyboard step, and the precision values are rounded to. */
  step: number;
  /** "log" suits frequencies and times; it needs a minimum above zero. */
  scale?: "linear" | "log";
}

const clamp = (x: number, lo: number, hi: number) => Math.min(hi, Math.max(lo, x));

/** Where a value sits along the track, 0 to 1. */
export function toPosition(value: number, r: Range): number {
  const v = clamp(value, r.min, r.max);
  if (r.scale === "log") return Math.log(v / r.min) / Math.log(r.max / r.min);
  return (v - r.min) / (r.max - r.min);
}

/** Round to the step, staying inside the range and free of float noise. */
export function snap(value: number, r: Range): number {
  const steps = Math.round((value - r.min) / r.step);
  const decimals = Math.max(0, Math.ceil(-Math.log10(r.step)) + 1);
  return clamp(Number((r.min + steps * r.step).toFixed(decimals)), r.min, r.max);
}

/** The value at a position along the track. */
export function fromPosition(position: number, r: Range): number {
  const p = clamp(position, 0, 1);
  const value = r.scale === "log" ? r.min * Math.pow(r.max / r.min, p) : r.min + p * (r.max - r.min);
  return snap(value, r);
}

export type Nudge = "up" | "down" | "pageUp" | "pageDown" | "min" | "max";

/**
 * The value after a key press. Arrows move one step, or a tenth of the track
 * with a page key. On a log scale arrows move by a fixed ratio instead, and
 * always by at least one step.
 */
export function nudge(value: number, how: Nudge, r: Range): number {
  switch (how) {
    case "min":
      return r.min;
    case "max":
      return r.max;
    case "pageUp":
    case "pageDown":
      return fromPosition(toPosition(value, r) + (how === "pageUp" ? 0.1 : -0.1), r);
    default: {
      const direction = how === "up" ? 1 : -1;
      if (r.scale !== "log") return snap(value + direction * r.step, r);
      const moved = fromPosition(toPosition(value, r) + direction * 0.01, r);
      return moved === snap(value, r) ? snap(value + direction * r.step, r) : moved;
    }
  }
}
