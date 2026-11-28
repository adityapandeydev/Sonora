// Offers work kept from a session that ended without saving.
import { useState } from "react";
import type { RecoveryCheck } from "./protocol";
import { Modal } from "./ui/Modal";

function when(iso: string | undefined): string {
  if (!iso) return "";
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return "";
  const today = new Date().toDateString() === d.toDateString();
  return today ? `today at ${d.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}` : d.toLocaleString([], { dateStyle: "medium", timeStyle: "short" });
}

interface Props {
  found: RecoveryCheck | null;
  onRestore: () => Promise<unknown>;
  onDiscard: () => Promise<unknown>;
  onDone: () => void;
}

export function RecoveryDialog({ found, onRestore, onDiscard, onDone }: Props) {
  const [busy, setBusy] = useState(false);
  const [problem, setProblem] = useState<string | null>(null);
  const open = found?.available === true;
  const name = found?.name ?? (found?.title ? `"${found.title}"` : "an untitled project");
  const at = when(found?.savedAt);

  const act = async (f: () => Promise<unknown>) => {
    setBusy(true);
    setProblem(null);
    try {
      await f();
      onDone();
    } catch (e) {
      setProblem(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <Modal open={open} title="Recover unsaved work?" onClose={() => {}}>
      <p className="sub">
        Sonora stopped before changes to <b>{name}</b> were saved. They were kept{at ? ` ${at}` : ""}. Recovering opens them as unsaved work; save to keep them.
      </p>
      {problem ? (
        <p className="field-problem" role="alert">
          {problem}
        </p>
      ) : null}
      <div className="modal-actions">
        <button type="button" className="button" disabled={busy} onClick={() => act(onDiscard)}>
          Discard
        </button>
        {/* Recovering is the safe default: Enter must never throw work away. */}
        <button type="button" className="button accent" disabled={busy} data-autofocus onClick={() => act(onRestore)}>
          Recover
        </button>
      </div>
    </Modal>
  );
}
