// Placeholder shell: proves the host and engine connection. The product
// interface replaces this.
import { useEffect, useState } from "react";
import { callEngine, engineStatus, onEngineStatus, restartEngine, State, type Status } from "./engine";

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
  const [ping, setPing] = useState<string>("");
  const [restarting, setRestarting] = useState(false);

  useEffect(() => {
    const off = onEngineStatus(setStatus);
    engineStatus().then(setStatus);
    return off;
  }, []);

  const sendPing = async () => {
    const t0 = performance.now();
    const reply = await callEngine("ping");
    const ms = (performance.now() - t0).toFixed(2);
    setPing(reply.ok ? `Round trip ${ms} ms` : `${reply.error?.code}: ${reply.error?.message}`);
  };

  const restart = async () => {
    setRestarting(true);
    try {
      setStatus(await restartEngine());
    } finally {
      setRestarting(false);
    }
  };

  const state = status?.state ?? State.StateStarting;
  const canRetry = state === State.StateCrashed || state === State.StateFailed || state === State.StateIncompatible;

  return (
    <main>
      <h1>Sonora</h1>
      <p role="status" aria-live="polite">
        {describe[state] ?? state}
        {state === State.StateReady && status ? ` (version ${status.serviceVersion}, protocol ${status.protocol})` : ""}
      </p>
      {status?.detail ? <p className="detail">{status.detail}</p> : null}
      <div className="actions">
        <button type="button" onClick={sendPing} disabled={state !== State.StateReady}>
          Ping engine
        </button>
        {canRetry ? (
          <button type="button" onClick={restart} disabled={restarting}>
            {restarting ? "Restarting…" : "Restart engine"}
          </button>
        ) : null}
      </div>
      {ping ? <p>{ping}</p> : null}
    </main>
  );
}
