// Types for the engine's application methods and events. They mirror the
// JSON produced by the Haskell service (Sonora.Service.Session).

export interface Diagnostic {
  severity: "error" | "warning";
  line: number | null;
  column: number | null;
  message: string;
}

export interface TrackMix {
  gain: number;
  pan: number;
  mute: boolean;
  solo: boolean;
}

export interface TrackView {
  id: string;
  name: string;
  instrument: string;
  patchSource: "override" | "score" | "preset" | "default";
  hasPatchOverride: boolean;
  /** The sound the track plays with, after overrides and fallbacks. */
  patch: Patch;
  mix: TrackMix;
}

export type Waveform = { type: "sine" | "triangle" | "saw" | "whiteNoise" | "pinkNoise" } | { type: "square"; duty: number };

export interface Envelope {
  attack: number;
  decay: number;
  sustain: number;
  release: number;
  curve: "linear" | "exponential";
}

export type Effect =
  | { type: "delay"; time: number; feedback: number; mix: number }
  | { type: "reverb"; roomSize: number; mix: number }
  | { type: "chorus"; rate: number; depth: number; mix: number }
  | { type: "distortion"; drive: number }
  | { type: "tremolo"; rate: number; depth: number };

export interface Patch {
  name: string;
  oscillators: { waveform: Waveform; level: number; semitones: number; cents: number; phase: number }[];
  filter: { mode: "lowpass" | "highpass" | "bandpass" | "notch"; cutoff: number; resonance: number; envelopeAmount: number; envelope: Envelope };
  amplitudeEnvelope: Envelope;
  /** The engine applies these last to first. */
  effects: Effect[];
}

export interface SongInfo {
  title: string;
  bpm: number;
  timeSignature: [number, number];
  beatsPerBar: number;
}

/** Track blocks in the current text (1-based lines, inclusive). */
export interface Outline {
  lines: number;
  tracks: { name: string; header: number; last: number }[];
}

/** Per track: [line, start s, end s, start beat] and [start s, end s, MIDI pitch, line]. */
export interface TrackTiming {
  track: string;
  lines: [number, number, number, number][];
  notes: [number, number, number, number][];
}

export interface ScoreTiming {
  version: number;
  tracks: TrackTiming[];
}

export interface FinishedRender {
  jobId: number;
  revision: number;
  durationSeconds: number;
  renderMillis: number;
  current: boolean;
}

export interface RenderView {
  state: "idle" | "rendering" | "ready" | "outdated" | "failed";
  running: { jobId: number; revision: number } | null;
  latest: FinishedRender | null;
  failure: { jobId: number; revision: number; message: string } | null;
}

export interface LoopRange {
  start: number;
  end: number;
}

export interface TransportView {
  playing: boolean;
  positionSeconds: number;
  durationSeconds: number;
  jobId: number | null;
  revision: number | null;
  current: boolean;
  loop: LoopRange | null;
  loopActive: boolean;
  volume: number;
  deviceLost: boolean;
}

export interface ProjectView {
  revision: number;
  scoreText: string;
  scoreIsCurrent: boolean;
  diagnostics: Diagnostic[];
  tracks: TrackView[];
  /** From the last valid score. */
  song: SongInfo | null;
  outline: Outline;
  /** Changes when the last valid score changes; matches ScoreTiming.version. */
  timingVersion: number;
  master: { gain: number };
  render: RenderView;
  transport: TransportView;
  /** Kept scores, newest first. */
  versions: ScoreVersion[];
  /** What the generator accepts. */
  generator: { roots: string[]; scales: string[] };
  /** What undo and redo would do next, e.g. "typing". */
  history: { undo: string | null; redo: string | null };
  /** Where the project lives, and whether it has unsaved changes. */
  file: { path: string | null; name: string | null; modified: boolean };
}

/** Unsaved work from a session that ended without saving. */
export interface RecoveryCheck {
  available: boolean;
  file?: string | null;
  name?: string | null;
  savedAt?: string;
  title?: string | null;
  /** An autosave that could not be read, and where it was kept. */
  problem?: string;
}

/** Settings for project.generate. */
export interface GenSettings {
  seed: number;
  root: string;
  scale: string;
  bpm: number;
  bars: number;
}

export interface ScoreVersion {
  id: number;
  label: string;
  /** Project revision when the version was kept. */
  revision: number;
  /** Present for generated scores. */
  settings: GenSettings | null;
  /** The version's text is the current score text. */
  current: boolean;
  lines: number;
}

/** Waveform overview: `[min, max]` per equal span of the render. */
export interface RenderPeaks {
  jobId: number;
  durationSeconds: number;
  peaks: [number, number][];
}

export interface RenderProgress {
  jobId: number;
  revision: number;
  completed: number;
  total: number;
}
