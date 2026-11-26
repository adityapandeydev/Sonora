// Export the song to a WAV file. The engine renders the project as it is when
// the export starts and writes the file only once it is complete; this
// dialog picks the file and format and follows the engine's progress.
import { Dialogs } from "@wailsio/runtime";
import { useCallback, useEffect, useId, useRef, useState } from "react";
import { EngineError, onEngineEvent, request } from "./engine";
import { baseName, bytes, EXPORT_DEPTHS, EXPORT_RATES, fileNameFor, readFormat, withWav, type ExportFormat } from "./mix";
import type { ProjectView } from "./protocol";
import { Modal } from "./ui/Modal";

const FORMAT_KEY = "sonora.export.v1";

type Phase =
  | { kind: "idle" }
  | { kind: "starting" }
  | { kind: "running"; id: number; revision: number; path: string; done: number; total: number }
  | { kind: "done"; path: string; seconds: number; size: number; format: ExportFormat; revision: number }
  | { kind: "failed"; message: string }
  | { kind: "cancelled" };

/** Export state lives above the dialog, so closing it does not lose a running export. */
export function useExport() {
  const [phase, setPhase] = useState<Phase>({ kind: "idle" });
  const running = useRef<number | null>(null);

  useEffect(
    () =>
      onEngineEvent((name, data) => {
        const d = data as Record<string, unknown>;
        if (!name.startsWith("export.") || d.exportId !== running.current) return;
        if (name === "export.progress") setPhase((p) => (p.kind === "running" ? { ...p, done: d.completed as number, total: d.total as number } : p));
        else if (name === "export.finished") {
          running.current = null;
          setPhase({
            kind: "done",
            path: d.path as string,
            seconds: d.seconds as number,
            size: d.bytes as number,
            format: { sampleRate: d.sampleRate as number, bitDepth: d.bitDepth as number },
            revision: d.revision as number,
          });
        } else if (name === "export.failed") {
          running.current = null;
          setPhase({ kind: "failed", message: d.message as string });
        } else if (name === "export.cancelled") {
          running.current = null;
          setPhase({ kind: "cancelled" });
        }
      }),
    [],
  );

  const start = useCallback(async (path: string, format: ExportFormat) => {
    setPhase({ kind: "starting" });
    try {
      const r = await request<{ exportId: number; revision: number }>("export.start", { path, ...format });
      running.current = r.exportId;
      setPhase({ kind: "running", id: r.exportId, revision: r.revision, path, done: 0, total: 1 });
    } catch (e) {
      const params = e instanceof EngineError ? (e.data as { params?: { message: string }[] } | undefined)?.params : undefined;
      setPhase({ kind: "failed", message: params?.length ? params.map((p) => p.message).join(". ") : e instanceof Error ? e.message : String(e) });
    }
  }, []);

  const cancel = useCallback(() => {
    request("export.cancel").catch(() => {});
  }, []);

  const reset = useCallback(() => setPhase((p) => (p.kind === "running" || p.kind === "starting" ? p : { kind: "idle" })), []);

  return { phase, start, cancel, reset };
}

const minutes = (s: number) => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;

interface Props {
  open: boolean;
  project: ProjectView;
  exporter: ReturnType<typeof useExport>;
  onClose: () => void;
}

export function ExportDialog({ open, project, exporter, onClose }: Props) {
  const { phase } = exporter;
  const [format, setFormat] = useState<ExportFormat>(() => {
    try {
      return readFormat(localStorage.getItem(FORMAT_KEY));
    } catch {
      return readFormat(null);
    }
  });
  const ids = { rate: useId(), depth: useId() };
  const busy = phase.kind === "running" || phase.kind === "starting";

  const choose = (f: ExportFormat) => {
    setFormat(f);
    try {
      localStorage.setItem(FORMAT_KEY, JSON.stringify(f));
    } catch {
      /* not remembered */
    }
  };

  const pick = async () => {
    let path = "";
    try {
      path = await Dialogs.SaveFile({
        Title: "Export song",
        Filename: fileNameFor(project.song?.title ?? "Song"),
        Filters: [{ DisplayName: "WAV audio", Pattern: "*.wav" }],
        ButtonText: "Export",
        CanCreateDirectories: true,
      });
    } catch {
      path = "";
    }
    if (path) exporter.start(withWav(path), format);
  };

  const close = () => {
    exporter.reset();
    onClose();
  };

  // Only a render of the project as it is now tells its length.
  const duration = project.render.latest?.current ? project.render.latest.durationSeconds : null;
  const playable = project.song !== null;

  return (
    <Modal open={open} title="Export song" onClose={close}>
      <p className="sub">
        {playable ? (
          <>
            Saves {project.song?.title ? <b>{project.song.title}</b> : "the song"} as it sounds now, with its mix
            {duration ? `, about ${minutes(duration)} long` : ""}. Later edits do not change the file.
            {!project.scoreIsCurrent ? " The score has problems, so its last valid version is exported." : ""}
          </>
        ) : (
          "There is nothing to export until the score has a valid track."
        )}
      </p>
      <div className="field-row">
        <label className="field grow" htmlFor={ids.rate}>
          <span>Sample rate</span>
          <select id={ids.rate} value={format.sampleRate} disabled={busy} onChange={(e) => choose({ ...format, sampleRate: Number(e.target.value) })}>
            {EXPORT_RATES.map((r) => (
              <option key={r.value} value={r.value}>
                {r.label}
              </option>
            ))}
          </select>
        </label>
        <label className="field" htmlFor={ids.depth}>
          <span>Bit depth</span>
          <select id={ids.depth} value={format.bitDepth} disabled={busy} onChange={(e) => choose({ ...format, bitDepth: Number(e.target.value) })}>
            {EXPORT_DEPTHS.map((d) => (
              <option key={d.value} value={d.value}>
                {d.label}
              </option>
            ))}
          </select>
        </label>
      </div>
      {phase.kind === "running" || phase.kind === "starting" ? (
        <div className="export-progress" role="status">
          <span>{phase.kind === "running" ? `Exporting ${baseName(phase.path)}` : "Starting the export"}</span>
          <progress max={phase.kind === "running" ? phase.total : 1} value={phase.kind === "running" ? phase.done : 0} />
        </div>
      ) : null}
      {phase.kind === "done" ? (
        <p className="export-done" role="status">
          Saved <b>{baseName(phase.path)}</b>: {minutes(phase.seconds)}, {phase.format.sampleRate / 1000} kHz, {phase.format.bitDepth === 32 ? "32-bit float" : `${phase.format.bitDepth}-bit`}, {bytes(phase.size)}.
          <span className="sub export-path">{phase.path}</span>
        </p>
      ) : null}
      {phase.kind === "failed" ? (
        <p className="field-problem" role="alert">
          {phase.message}
        </p>
      ) : null}
      {phase.kind === "cancelled" ? <p className="sub">The export was cancelled; no file was written.</p> : null}
      <div className="modal-actions">
        {busy ? (
          <button type="button" className="button" onClick={exporter.cancel} disabled={phase.kind === "starting"}>
            Cancel export
          </button>
        ) : (
          <>
            <button type="button" className="button" onClick={close}>
              {phase.kind === "done" ? "Close" : "Cancel"}
            </button>
            <button type="button" className="button accent" onClick={pick} disabled={!playable}>
              {phase.kind === "done" || phase.kind === "failed" ? "Export again…" : "Choose file and export…"}
            </button>
          </>
        )}
      </div>
    </Modal>
  );
}
