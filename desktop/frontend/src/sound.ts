// Sound editing without the DOM: control ranges (matching the engine's
// validation), immutable updates, and effect order.
//
// Effect order: the engine applies a patch's effect list last to first. The
// editor shows effects in the order they process the sound, top first, so
// the shown list is the engine's list reversed. Only these helpers convert.
import type { Effect, Envelope, Patch, Waveform } from "./protocol";
import type { Range } from "./ui/range";

export type Oscillator = Patch["oscillators"][number];

export const MAX_OSCILLATORS = 8;
export const MAX_EFFECTS = 8;

const time: Range = { min: 0.001, max: 20, step: 0.001, scale: "log" };

export const RANGES = {
  level: { min: 0, max: 1, step: 0.01 },
  semitones: { min: -48, max: 48, step: 1 },
  cents: { min: -100, max: 100, step: 1 },
  duty: { min: 0.01, max: 0.99, step: 0.01 },
  cutoff: { min: 20, max: 20000, step: 1, scale: "log" },
  resonance: { min: 0.1, max: 20, step: 0.1, scale: "log" },
  sweep: { min: -20000, max: 20000, step: 10 },
  attack: time,
  decay: time,
  sustain: { min: 0, max: 1, step: 0.01 },
  release: time,
  delayTime: { min: 0.001, max: 4, step: 0.001, scale: "log" },
  feedback: { min: 0, max: 0.95, step: 0.01 },
  mix: { min: 0, max: 1, step: 0.01 },
  roomSize: { min: 0, max: 1, step: 0.01 },
  chorusRate: { min: 0.01, max: 10, step: 0.01, scale: "log" },
  tremoloRate: { min: 0.01, max: 20, step: 0.01, scale: "log" },
  depth: { min: 0, max: 1, step: 0.01 },
  drive: { min: 1, max: 50, step: 0.1, scale: "log" },
} satisfies Record<string, Range>;

// Formats
export const hz = (f: number) => (f >= 1000 ? `${(f / 1000).toFixed(f >= 10000 ? 1 : 2)} kHz` : `${Math.round(f)} Hz`);
export const seconds = (s: number) => (s >= 1 ? `${s.toFixed(2)} s` : `${s >= 0.1 ? Math.round(s * 1000) : (s * 1000).toFixed(1)} ms`);
export const pct = (x: number) => `${Math.round(x * 100)}%`;
export const signedHz = (x: number) => `${x > 0 ? "+" : x < 0 ? "−" : ""}${hz(Math.abs(x))}`;
export const signed = (unit: string) => (x: number) => `${x > 0 ? "+" : x < 0 ? "−" : ""}${Math.abs(x)} ${unit}`;

export const isNoise = (w: Waveform) => w.type === "whiteNoise" || w.type === "pinkNoise";

export function waveform(type: Waveform["type"], previous?: Waveform): Waveform {
  if (type === "square") return { type, duty: previous?.type === "square" ? previous.duty : 0.5 };
  return { type };
}

// Updates

export const withOscillator = (p: Patch, i: number, f: (o: Oscillator) => Oscillator): Patch => ({
  ...p,
  oscillators: p.oscillators.map((o, k) => (k === i ? f(o) : o)),
});

export const withFilter = (p: Patch, f: (x: Patch["filter"]) => Patch["filter"]): Patch => ({ ...p, filter: f(p.filter) });

export const withEnvelope = (p: Patch, which: "amplitude" | "filter", f: (e: Envelope) => Envelope): Patch =>
  which === "amplitude" ? { ...p, amplitudeEnvelope: f(p.amplitudeEnvelope) } : { ...p, filter: { ...p.filter, envelope: f(p.filter.envelope) } };

export function addOscillator(p: Patch): Patch {
  if (p.oscillators.length >= MAX_OSCILLATORS) return p;
  return { ...p, oscillators: [...p.oscillators, { waveform: { type: "sine" }, level: 0.5, semitones: 0, cents: 0, phase: 0 }] };
}

export function removeOscillator(p: Patch, i: number): Patch {
  if (p.oscillators.length <= 1) return p;
  return { ...p, oscillators: p.oscillators.filter((_, k) => k !== i) };
}

// Effects, in processing order

/** Effects in the order they process the sound, first first. */
export const processing = (p: Patch): Effect[] => [...p.effects].reverse();

const fromProcessing = (p: Patch, list: Effect[]): Patch => ({ ...p, effects: [...list].reverse() });

export const EFFECT_TYPES: Effect["type"][] = ["distortion", "chorus", "tremolo", "delay", "reverb"];

export function newEffect(type: Effect["type"]): Effect {
  switch (type) {
    case "delay":
      return { type, time: 0.25, feedback: 0.3, mix: 0.25 };
    case "reverb":
      return { type, roomSize: 0.5, mix: 0.25 };
    case "chorus":
      return { type, rate: 1, depth: 0.4, mix: 0.3 };
    case "distortion":
      return { type, drive: 2 };
    case "tremolo":
      return { type, rate: 5, depth: 0.4 };
  }
}

/** Add an effect at the end of the chain: it processes the sound last. */
export function addEffect(p: Patch, type: Effect["type"]): Patch {
  if (p.effects.length >= MAX_EFFECTS) return p;
  return fromProcessing(p, [...processing(p), newEffect(type)]);
}

/** Change the effect at a position in processing order. */
export function withEffect(p: Patch, at: number, f: (e: Effect) => Effect): Patch {
  return fromProcessing(p, processing(p).map((e, k) => (k === at ? f(e) : e)));
}

export function removeEffect(p: Patch, at: number): Patch {
  return fromProcessing(p, processing(p).filter((_, k) => k !== at));
}

/** Move an effect earlier (-1) or later (+1) in the chain. */
export function moveEffect(p: Patch, at: number, by: -1 | 1): Patch {
  const list = processing(p);
  const to = at + by;
  if (to < 0 || to >= list.length) return p;
  [list[at], list[to]] = [list[to], list[at]];
  return fromProcessing(p, list);
}

export const sameSound = (a: Patch, b: Patch) => JSON.stringify(a) === JSON.stringify(b);

// Audition keys: a piano row on the computer keyboard, from C.
export const KEY_NOTES = ["a", "w", "s", "e", "d", "f", "t", "g", "y", "h", "u", "j", "k"];

export const noteName = (midi: number) => `${["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"][midi % 12]}${Math.floor(midi / 12) - 1}`;
