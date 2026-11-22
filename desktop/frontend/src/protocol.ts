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
  mix: TrackMix;
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

export interface TransportView {
  playing: boolean;
  positionSeconds: number;
  durationSeconds: number;
  jobId: number | null;
  revision: number | null;
  current: boolean;
  deviceLost: boolean;
}

export interface ProjectView {
  revision: number;
  scoreText: string;
  scoreIsCurrent: boolean;
  diagnostics: Diagnostic[];
  tracks: TrackView[];
  master: { gain: number };
  render: RenderView;
  transport: TransportView;
}

export interface RenderProgress {
  jobId: number;
  revision: number;
  completed: number;
  total: number;
}
