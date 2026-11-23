import { useEffect, useState } from "react";
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
  const open = state === State.StateReady && project !== null && text !== null;
  const banner = open ? scoreState(project, text) : { tone: broken ? "bad" : "", words: describe[state] ?? state };

  return (
    <div className={open ? "app ready" : "app"}>
      <header className="top">
        <h1 title={open && project.song ? project.song.title : "Sonora"}>{open && project.song ? project.song.title : "Sonora"}</h1>
        {open && project.song ? (
          <span className="meta">
            {Math.round(project.song.bpm * 10) / 10} bpm, {project.song.timeSignature.join("/")}
          </span>
        ) : null}
        <span className={`state ${banner.tone}`} role="status">
          {banner.words}
        </span>
      </header>
      {open ? (
        <Workspace
          project={project}
          text={text}
          progress={session.progress}
          transport={session.transport}
          peaks={session.peaks}
          timing={session.timing}
          error={session.error}
          onEdit={session.edit}
          onPlay={session.play}
          onPause={session.pause}
          onStop={session.stop}
          onSeek={session.seek}
          onLoop={session.setLoop}
          onVolume={session.setVolume}
          onRetryRender={session.retryRender}
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
