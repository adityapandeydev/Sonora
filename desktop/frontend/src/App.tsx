// Placeholder shell: engine connection plus the interim workspace. The
// designed product interface replaces it.
import { useEffect, useState } from "react";
import { engineStatus, onEngineStatus, restartEngine, State, type Status } from "./engine";
import { useProject } from "./useProject";
import { Workspace } from "./Workspace";

const describe: Record<string, string> = {
  [State.StateStopped]: "Engine stopped",
  [State.StateStarting]: "Starting engine…",
  [State.StateReady]: "Engine ready",
  [State.StateCrashed]: "Engine stopped unexpectedly",
  [State.StateIncompatible]: "Engine version is incompatible",
  [State.StateFailed]: "Engine could not start",
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
  const ready = state === State.StateReady;
  const session = useProject(ready);

  const restart = async () => {
    setRestarting(true);
    try {
      setStatus(await restartEngine());
    } finally {
      setRestarting(false);
    }
  };

  const canRetry = state === State.StateCrashed || state === State.StateFailed || state === State.StateIncompatible;

  return (
    <main>
      <h1>Sonora</h1>
      {!ready ? (
        <>
          <p role="status" aria-live="polite">
            {describe[state] ?? state}
          </p>
          {status?.detail ? <p className="detail">{status.detail}</p> : null}
          {canRetry ? (
            <button type="button" onClick={restart} disabled={restarting}>
              {restarting ? "Restarting…" : "Restart engine"}
            </button>
          ) : null}
        </>
      ) : session.project && session.text !== null ? (
        <Workspace
          project={session.project}
          text={session.text}
          progress={session.progress}
          transport={session.transport}
          error={session.error}
          onEdit={session.edit}
          onPlay={session.play}
          onPause={session.pause}
          onStop={session.stop}
          onRetryRender={session.retryRender}
        />
      ) : (
        <p role="status">Loading project…</p>
      )}
    </main>
  );
}
