import { useEffect, useState } from "react";
import { ExportDialog, useExport } from "./ExportDialog";
import { FileMenu, UnsavedDialog, useFiles } from "./FileMenu";
import { GenerateDialog, VersionsDialog } from "./GenerateDialog";
import { engineStatus, onEngineStatus, restartEngine, State, type Status } from "./engine";
import { useProject } from "./useProject";
import { scoreState, Workspace } from "./Workspace";

const describe: Record<string, string> = {
  [State.StateStopped]: "Engine stopped",
  [State.StateStarting]: "Starting the engine",
  [State.StateReady]: "Opening the project",
  [State.StateCrashed]: "The engine stopped unexpectedly",
  [State.StateIncompatible]: "The engine does not match this version of Sonora",
  [State.StateFailed]: "The engine could not start",
};

export default function App() {
  const [status, setStatus] = useState<Status | null>(null);
  const [restarting, setRestarting] = useState(false);
  const [dialog, setDialog] = useState<"generate" | "versions" | "export" | null>(null);
  const exporter = useExport();

  useEffect(() => {
    const off = onEngineStatus(setStatus);
    engineStatus().then(setStatus);
    return off;
  }, []);

  const state = status?.state ?? State.StateStarting;
  const session = useProject(state === State.StateReady);
  const broken = state === State.StateCrashed || state === State.StateFailed || state === State.StateIncompatible;

  const restart = async () => {
    setRestarting(true);
    try {
      setStatus(await restartEngine());
    } finally {
      setRestarting(false);
    }
  };

  const { project, text } = session;
  const files = useFiles(session, project);
  const ready = state === State.StateReady && project !== null;

  // File shortcuts, outside dialogs.
  useEffect(() => {
    if (!ready) return;
    const keys = (e: KeyboardEvent) => {
      if (!(e.ctrlKey || e.metaKey) || e.altKey || document.querySelector("dialog[open]")) return;
      const k = e.key.toLowerCase();
      const run = k === "s" ? (e.shiftKey ? files.saveAs : files.save) : !e.shiftKey && k === "o" ? files.open : !e.shiftKey && k === "n" ? files.create : null;
      if (!run) return;
      e.preventDefault();
      run();
    };
    window.addEventListener("keydown", keys);
    return () => window.removeEventListener("keydown", keys);
  });

  const fileName = project?.file.name ?? "Untitled";
  // A score without a title is named by its file.
  const songTitle = ready && project.song?.title && project.song.title !== "Untitled" ? project.song.title : null;
  const heading = ready ? (songTitle ?? fileName) : "Sonora";
  const edited = project?.file.modified ?? false;
  useEffect(() => {
    document.title = ready ? `${fileName}${edited ? " (edited)" : ""} - Sonora` : "Sonora";
  }, [ready, fileName, edited]);
  const open = state === State.StateReady && project !== null && text !== null;
  const banner = open ? scoreState(project, text) : { tone: broken ? "bad" : "", words: describe[state] ?? state };

  return (
    <div className={open ? "app ready" : "app"}>
      <header className="top">
        <h1 title={heading}>{heading}</h1>
        {open && project.song ? (
          <span className="meta">
            {Math.round(project.song.bpm * 10) / 10} bpm, {project.song.timeSignature.join("/")}
          </span>
        ) : null}
        {open ? (
          <span className="file-name" title={project.file.path ?? "Not saved yet"}>
            {heading !== fileName ? fileName : null}
            {edited ? <span className="edited">Edited</span> : null}
          </span>
        ) : null}
        <span className={`state ${banner.tone}`} role="status">
          {banner.words}
        </span>
        {open ? (
          <div className="top-actions">
            <FileMenu files={files} />
            <button type="button" className="button on-shell" onClick={() => setDialog("versions")} disabled={project.versions.length === 0} title="Scores replaced by generating or restoring">
              Versions{project.versions.length > 0 ? ` (${project.versions.length})` : ""}
            </button>
            <button type="button" className="button on-shell" onClick={() => setDialog("generate")}>
              Generate
            </button>
            <button type="button" className="button on-shell" onClick={() => setDialog("export")} title="Save the song as a WAV file">
              {exporter.phase.kind === "running" ? "Exporting…" : "Export"}
            </button>
          </div>
        ) : null}
      </header>
      {open ? (
        <>
          <GenerateDialog
            open={dialog === "generate"}
            roots={project.generator.roots}
            scales={project.generator.scales}
            versions={project.versions}
            onClose={() => setDialog(null)}
            onGenerate={session.generate}
          />
          <VersionsDialog open={dialog === "versions"} versions={project.versions} onClose={() => setDialog(null)} onRestore={session.restoreVersion} />
          <ExportDialog open={dialog === "export"} project={project} exporter={exporter} onClose={() => setDialog(null)} />
          <UnsavedDialog files={files} project={project} />
        </>
      ) : null}
      {open ? (
        <Workspace
          project={project}
          text={text}
          progress={session.progress}
          transport={session.transport}
          peaks={session.peaks}
          timing={session.timing}
          error={files.problem ?? session.error}
          onDismissError={files.problem ? files.clearProblem : undefined}
          onEdit={session.edit}
          onPlay={session.play}
          onPause={session.pause}
          onStop={session.stop}
          onSeek={session.seek}
          onLoop={session.setLoop}
          onVolume={session.setVolume}
          onRetryRender={session.retryRender}
          onCancelRender={session.cancelRender}
          onCommand={session.command}
          onAct={session.act}
        />
      ) : (
        <main className="message">
          <p>{banner.words}.</p>
          {status?.detail ? <p className="mono">{status.detail}</p> : null}
          {session.error ? <p className="mono">{session.error}</p> : null}
          {broken ? (
            <button type="button" className="button" onClick={restart} disabled={restarting}>
              {restarting ? "Restarting" : "Restart engine"}
            </button>
          ) : null}
        </main>
      )}
    </div>
  );
}
