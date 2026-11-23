// The open project: score, problems, timeline, and transport. The middle of
// the window is interim until the sound-led workspace.
import { useRef } from "react";
import type { Diagnostic, LoopRange, ProjectView, RenderPeaks, RenderProgress, TransportView } from "./protocol";
import { Transport } from "./Transport";

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
      return p ? `Rendering ${p.completed} of ${p.total}` : "Rendering";
    }
    case "ready":
      return r.latest ? `Rendered in ${(r.latest.renderMillis / 1000).toFixed(1)} s` : "Rendered";
    case "outdated":
      return "Playing an older render";
    case "failed":
      return "Render failed";
    default:
      return "Not rendered yet";
  }
}

const place = (d: Diagnostic) =>
  d.line === null ? (d.severity === "error" ? "Problem" : "Warning") : `Line ${d.line}${d.column === null ? "" : `, col ${d.column}`}`;

export function scoreState(project: ProjectView, text: string): { tone: "good" | "bad" | ""; words: string } {
  // Diagnostics describe the text the engine last saw.
  if (project.scoreText !== text) return { tone: "", words: "Checking the score" };
  if (project.scoreIsCurrent) return { tone: "good", words: "Score is valid" };
  const n = project.diagnostics.filter((d) => d.severity === "error").length;
  return { tone: "bad", words: `${n} problem${n === 1 ? "" : "s"}. Playing the last valid version` };
}

export function Workspace(props: Props) {
  const { project, text, progress, transport, error } = props;
  const editor = useRef<HTMLTextAreaElement>(null);
  const checked = project.scoreText === text;

  const jumpTo = (d: Diagnostic) => {
    const el = editor.current;
    if (!el || d.line === null) return;
    const lines = text.split("\n");
    let offset = 0;
    for (let i = 0; i < d.line - 1 && i < lines.length; i++) offset += lines[i].length + 1;
    offset += Math.max(0, (d.column ?? 1) - 1);
    el.focus();
    el.setSelectionRange(offset, offset);
    // Moving the caret does not scroll a textarea; lines do not wrap.
    const lineHeight = parseFloat(getComputedStyle(el).lineHeight);
    el.scrollTop = Math.max(0, (d.line - 1) * lineHeight - el.clientHeight / 2);
  };

  return (
    <>
      <section className="score-pane" aria-label="Score">
        {project.render.state === "failed" ? (
          <div className="notice" role="alert">
            <span>The song could not be rendered: {project.render.failure?.message ?? "unknown error"}</span>
            <button type="button" className="button" onClick={props.onRetryRender}>
              Try again
            </button>
          </div>
        ) : null}
        {error ? (
          <div className="notice" role="alert">
            {error}
          </div>
        ) : null}
        <textarea
          id="score"
          aria-label="Score"
          ref={editor}
          value={text}
          spellCheck={false}
          wrap="off"
          onChange={(e) => props.onEdit(e.target.value)}
        />
        {checked && project.diagnostics.length > 0 ? (
          <div className="problems" aria-label="Problems">
            <ul>
              {project.diagnostics.map((d, i) => (
                <li key={i} className={d.severity}>
                  <button type="button" onClick={() => jumpTo(d)} disabled={d.line === null}>
                    <b>{place(d)}</b>
                    <span>{d.message}</span>
                  </button>
                </li>
              ))}
            </ul>
          </div>
        ) : null}
      </section>
      <Transport
        transport={transport ?? project.transport}
        peaks={props.peaks}
        duration={project.render.latest?.durationSeconds ?? 0}
        canPlay={project.render.latest !== null}
        status={renderStatus(project, progress)}
        onPlay={props.onPlay}
        onPause={props.onPause}
        onStop={props.onStop}
        onSeek={props.onSeek}
        onLoop={props.onLoop}
        onVolume={props.onVolume}
      />
    </>
  );
}
