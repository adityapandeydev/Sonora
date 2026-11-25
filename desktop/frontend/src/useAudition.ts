// Auditioning notes of a sound. The engine renders and plays them; this hook
// only tracks which note is ours, which keys are held, and sends sound
// changes to the sounding note one at a time, always the latest.
import { useCallback, useEffect, useRef, useState } from "react";
import { request } from "./engine";
import type { Patch } from "./protocol";

/** How long after the last start or release a note may still be sounding. */
const SOUNDING_MS = 6000;

interface Note {
  pitch: number;
  /** Known once the engine answers. */
  id: number | null;
  /** Released before the engine answered. */
  releaseWhenKnown: boolean;
  until: number;
}

export function useAudition(onError: (message: string) => void) {
  const note = useRef<Note | null>(null);
  const [held, setHeld] = useState<number | null>(null);
  const sending = useRef(false);
  const queued = useRef<Patch | null>(null);
  const report = useRef(onError);
  report.current = onError;

  const fail = (e: unknown) => {
    if (!(e instanceof Error && e.name === "CancelError")) report.current(e instanceof Error ? e.message : String(e));
  };

  const release = useCallback((pitch?: number) => {
    const n = note.current;
    if (!n || (pitch !== undefined && n.pitch !== pitch)) return;
    setHeld(null);
    n.until = Date.now() + SOUNDING_MS;
    if (n.id === null) n.releaseWhenKnown = true;
    else request("audition.release", { auditionId: n.id }).catch(fail);
  }, []);

  const start = useCallback((patch: Patch, pitch: number) => {
    const n: Note = { pitch, id: null, releaseWhenKnown: false, until: Date.now() + SOUNDING_MS };
    note.current = n;
    queued.current = null;
    setHeld(pitch);
    request<{ auditionId: number }>("audition.start", { patch, pitch })
      .then((r) => {
        n.id = r.auditionId;
        if (n.releaseWhenKnown && note.current === n) request("audition.release", { auditionId: r.auditionId }).catch(fail);
      })
      .catch((e) => {
        if (note.current === n) {
          note.current = null;
          setHeld(null);
        }
        fail(e);
      });
  }, []);

  // One change in flight at a time; while it is, keep only the newest.
  const update = useCallback((patch: Patch) => {
    const n = note.current;
    if (!n || n.id === null || Date.now() > n.until) return;
    if (sending.current) {
      queued.current = patch;
      return;
    }
    sending.current = true;
    request<{ active: boolean }>("audition.update", { auditionId: n.id, patch })
      .then((r) => {
        if (!r.active && note.current === n) note.current = null;
      })
      .catch(fail)
      .finally(() => {
        sending.current = false;
        const next = queued.current;
        queued.current = null;
        if (next) update(next);
      });
  }, []);

  const stop = useCallback(() => {
    note.current = null;
    setHeld(null);
    request("audition.stop").catch(fail);
  }, []);

  // Let go of a held note if the window loses focus mid-press.
  useEffect(() => {
    const blur = () => release();
    window.addEventListener("blur", blur);
    return () => window.removeEventListener("blur", blur);
  }, [release]);

  return { held, start, release, update, stop };
}
