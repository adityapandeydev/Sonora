// Interim workspace for the edit → render → play slice. Functional only;
// the designed interface replaces it.
import { useRef } from "react";
import type { Diagnostic, LoopRange, ProjectView, RenderPeaks, RenderProgress, TransportView } from "./protocol";
import { clock, Transport } from "./Transport";

interface Props {
  project: ProjectView;
  text: string;
  progress: RenderProgress | null;
  transport: TransportView | null;
  peaks: RenderPeaks | null;
  error: string | null;
  onEdit: (text: string) => void;
  onPlay: () => void;
  onPause: () => void;
  onStop: () => void;
  onSeek: (seconds: number) => void;
  onLoop: (loop: LoopRange | null) => void;
  onVolume: (volume: number) => void;
  onRetryRender: () => void;
}

function renderStatus(project: ProjectView, progress: RenderProgress | null): string {
  const r = project.render;
  switch (r.state) {
    case "rendering": {
      const p = progress && r.running && progress.jobId === r.running.jobId ? progress : null;
      return p ? `Rendering… ${p.completed}/${p.total}` : "Rendering…";
    }
    case "ready":
      return r.latest ? `Rendered (${clock(r.latest.durationSeconds)}, ${r.latest.renderMillis} ms)` : "Rendered";
    case "outdated":
      return "Render is out of date";
    case "failed":
      return `Render failed: ${r.failure?.message ?? "unknown error"}`;
    default:
      return "Not rendered yet";
  }
}

export function Workspace(props: Props) {
  const { project, text, progress, transport, error } = props;
  const editor = useRef<HTMLTextAreaElement>(null);

  // Diagnostics describe the text the engine last saw.
  const checked = project.scoreText === text;
  const canPlay = project.render.latest !== null;

  const jumpTo = (d: Diagnostic) => {
    const el = editor.current;
    if (!el || d.line === null) return;
    const lines = text.split("\n");
    let offset = 0;
    for (let i = 0; i < d.line - 1 && i < lines.length; i++) offset += lines[i].length + 1;
    offset += Math.max(0, (d.column ?? 1) - 1);
    el.focus();
    el.setSelectionRange(offset, offset);
  };

  return (
    <section className="workspace">
      <label htmlFor="score">Score</label>
      <textarea
        id="score"
        ref={editor}
        value={text}
        spellCheck={false}
        rows={18}
        onChange={(e) => props.onEdit(e.target.value)}
      />

      <div role="status" aria-live="polite" className="status-lines">
        <p>
          {!checked
            ? "Checking score…"
            : project.scoreIsCurrent
              ? "Score is valid"
              : "Score has errors; the last valid version is used for playback"}
        </p>
        <p>
          {renderStatus(project, progress)}
          {project.render.state === "failed" ? (
            <button type="button" onClick={props.onRetryRender}>
              Retry
            </button>
          ) : null}
        </p>
        {transport && transport.jobId !== null && !transport.current ? (
          <p>Playing an earlier render; the newest one is swapped in when ready.</p>
        ) : null}
        {error ? <p className="error">{error}</p> : null}
      </div>

      {checked && project.diagnostics.length > 0 ? (
        <ul className="diagnostics">
          {project.diagnostics.map((d, i) => (
            <li key={i}>
              <button type="button" onClick={() => jumpTo(d)} disabled={d.line === null}>
                {d.severity}
                {d.line !== null ? ` line ${d.line}${d.column !== null ? `:${d.column}` : ""}` : ""}
              </button>{" "}
              <span className="detail">{d.message}</span>
            </li>
          ))}
        </ul>
      ) : null}

      <Transport
        transport={transport ?? project.transport}
        peaks={props.peaks}
        duration={project.render.latest?.durationSeconds ?? 0}
        canPlay={canPlay}
        onPlay={props.onPlay}
        onPause={props.onPause}
        onStop={props.onStop}
        onSeek={props.onSeek}
        onLoop={props.onLoop}
        onVolume={props.onVolume}
      />
    </section>
  );
}
