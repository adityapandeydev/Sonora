// Generator settings in the UI: defaults, limits, labels and field errors.
// The engine validates and generates; these limits only shape the inputs.
import type { GenSettings, ScoreVersion } from "./protocol";

export const LIMITS = {
  seed: { min: 0, max: 4294967295 },
  bpm: { min: 40, max: 240 },
  bars: { min: 1, max: 64 },
  /** The engine refuses songs longer than this. */
  seconds: 180,
};

export type Field = keyof GenSettings;

const SCALE_LABELS: Record<string, string> = {
  major: "Major",
  naturalMinor: "Minor",
  harmonicMinor: "Harmonic minor",
  melodicMinor: "Melodic minor",
  dorian: "Dorian",
  phrygian: "Phrygian",
  lydian: "Lydian",
  mixolydian: "Mixolydian",
  locrian: "Locrian",
  majorPentatonic: "Major pentatonic",
  minorPentatonic: "Minor pentatonic",
  blues: "Blues",
};

/** A readable scale name; unknown names are split at capitals. */
export const scaleLabel = (name: string): string =>
  SCALE_LABELS[name] ?? name.replace(/([A-Z])/g, " $1").replace(/^./, (c) => c.toUpperCase());

/** Song length in seconds (the generator writes 4/4). */
export const lengthOf = (s: Pick<GenSettings, "bpm" | "bars">): number => (s.bars * 4 * 60) / s.bpm;

export function randomSeed(random: () => number = Math.random): number {
  return Math.floor(random() * 1_000_000);
}

/** Start from the newest generated settings, so regenerating is one change away. */
export function initialSettings(versions: ScoreVersion[], random: () => number = Math.random): GenSettings {
  const last = versions.find((v) => v.settings !== null)?.settings;
  return last ? { ...last } : { seed: randomSeed(random), root: "C", scale: "major", bpm: 110, bars: 8 };
}

/** Problems the form can show before asking the engine; the engine has the final word. */
export function localProblems(s: GenSettings): Partial<Record<Field, string>> {
  const out: Partial<Record<Field, string>> = {};
  const whole = (x: number) => Number.isFinite(x) && Math.floor(x) === x;
  if (!whole(s.seed) || s.seed < LIMITS.seed.min || s.seed > LIMITS.seed.max) out.seed = `Use a whole number from 0 to ${LIMITS.seed.max}`;
  if (!Number.isFinite(s.bpm) || s.bpm < LIMITS.bpm.min || s.bpm > LIMITS.bpm.max) out.bpm = `Use ${LIMITS.bpm.min} to ${LIMITS.bpm.max} bpm`;
  if (!whole(s.bars) || s.bars < LIMITS.bars.min || s.bars > LIMITS.bars.max) out.bars = `Use ${LIMITS.bars.min} to ${LIMITS.bars.max} bars`;
  else if (!out.bpm && lengthOf(s) > LIMITS.seconds) out.bars = "Over three minutes; use fewer bars or a faster tempo";
  return out;
}

/** Field errors from an engine invalid_value reply: `{params: [{path, message}]}`. */
export function engineProblems(data: unknown): Partial<Record<Field, string>> {
  const out: Partial<Record<Field, string>> = {};
  const params = (data as { params?: unknown } | null)?.params;
  if (!Array.isArray(params)) return out;
  for (const p of params) {
    const path = (p as { path?: unknown }).path;
    const message = (p as { message?: unknown }).message;
    if (typeof path === "string" && typeof message === "string" && !(path in out)) out[path as Field] = message;
  }
  return out;
}

export function describeSettings(s: GenSettings): string {
  return `${s.root} ${scaleLabel(s.scale).toLowerCase()}, ${s.bpm} bpm, ${s.bars} bar${s.bars === 1 ? "" : "s"}, seed ${s.seed}`;
}
