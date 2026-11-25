// Connects the editor and transport to the engine's project session.
// The engine applies edits in arrival order and owns all project state;
// this hook only keeps the latest view of it and the local editor text.
import { useCallback, useEffect, useRef, useState } from "react";
import { EngineError, onEngineEvent, request } from "./engine";
import type { GenSettings, LoopRange, ProjectView, RenderPeaks, RenderProgress, ScoreTiming, TransportView } from "./protocol";

/** Idle time after typing before the engine validates the score. */
export const VALIDATE_AFTER_MS = 250;

const PEAK_BUCKETS = 1024;

const describeError = (e: unknown): string =>
  e instanceof EngineError ? e.message : e instanceof Error ? e.message : String(e);

const isCancel = (e: unknown) => e instanceof Error && e.name === "CancelError";

export function useProject(ready: boolean) {
  const [project, setProject] = useState<ProjectView | null>(null);
  const [text, setText] = useState<string | null>(null);
  const [progress, setProgress] = useState<RenderProgress | null>(null);
  const [transport, setTransport] = useState<TransportView | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [peaks, setPeaks] = useState<RenderPeaks | null>(null);
  const [timing, setTiming] = useState<ScoreTiming | null>(null);
  const shownRevision = useRef(-1);
  const timer = useRef<number | undefined>(undefined);
  /** Text typed but not yet sent to the engine. */
  const pending = useRef<string | null>(null);

  const report = useCallback((e: unknown) => {
    if (!isCancel(e)) setError(describeError(e));
  }, []);

  // Views can arrive out of order (an edit response and a refresh); never
  // replace a newer revision with an older one.
  const accept = useCallback((v: ProjectView) => {
    if (v.revision < shownRevision.current) return;
    shownRevision.current = v.revision;
    setProject(v);
    setTransport(v.transport);
  }, []);

  const refresh = useCallback(() => {
    request<ProjectView>("project.get").then(accept).catch(report);
  }, [accept, report]);

  useEffect(() => {
    if (!ready) {
      shownRevision.current = -1;
      setProject(null);
      setProgress(null);
      setTransport(null);
      setPeaks(null);
      setTiming(null);
      return;
    }
    const load = request<ProjectView>("project.get");
    load
      .then((v) => {
        accept(v);
        setText(v.scoreText);
        return request("render.start");
      })
      .then(refresh)
      .catch(report);
    return () => {
      load.cancel();
    };
  }, [ready, accept, refresh, report]);

  useEffect(
    () =>
      onEngineEvent((name, data) => {
        switch (name) {
          case "render.progress":
            setProgress(data as RenderProgress);
            break;
          case "render.started":
          case "render.finished":
          case "render.failed":
          case "render.cancelled":
            setProgress(null);
            refresh();
            break;
          case "transport":
            setTransport(data as TransportView);
            break;
        }
      }),
    [refresh],
  );

  const edit = useCallback(
    (next: string) => {
      setText(next);
      setError(null);
      pending.current = next;
      window.clearTimeout(timer.current);
      timer.current = window.setTimeout(() => {
        pending.current = null;
        request<ProjectView>("project.setScore", { text: next }).then(accept).catch(report);
      }, VALIDATE_AFTER_MS);
    },
    [accept, report],
  );

  const transportCommand = useCallback(
    (method: string, params: unknown = {}) => {
      setError(null);
      request<TransportView>(method, params).then(setTransport).catch(report);
    },
    [report],
  );

  // Fetch the waveform of each finished render once.
  const latestJob = project?.render.latest?.jobId ?? null;
  useEffect(() => {
    if (latestJob === null) return;
    const load = request<RenderPeaks>("render.peaks", { buckets: PEAK_BUCKETS });
    load.then(setPeaks).catch(report);
    return () => {
      load.cancel();
    };
  }, [latestJob, report]);

  // Fetch line timing whenever the playable score changes.
  const timingVersion = project?.timingVersion ?? null;
  useEffect(() => {
    if (timingVersion === null) return;
    const load = request<ScoreTiming>("project.timing");
    load.then(setTiming).catch(report);
    return () => {
      load.cancel();
    };
  }, [timingVersion, report]);

  // Dragging a slider produces many values: send one at a time and only the
  // newest of those that arrived meanwhile.
  const dragging = useRef(new Map<string, { busy: boolean; next: unknown }>());
  const dragCommand = useCallback(
    (method: string, params: unknown) => {
      const slots = dragging.current;
      const slot = slots.get(method) ?? { busy: false, next: null };
      slots.set(method, slot);
      slot.next = params;
      if (slot.busy) return;
      const send = () => {
        const sending = slot.next;
        slot.next = null;
        slot.busy = true;
        request<TransportView>(method, sending)
          .then((v) => {
            if (slot.next === null) setTransport(v);
          })
          .catch(report)
          .finally(() => {
            slot.busy = false;
            if (slot.next !== null) send();
          });
      };
      send();
    },
    [report],
  );

  useEffect(() => () => window.clearTimeout(timer.current), []);

  const command = useCallback(
    (cmd: unknown) =>
      request<ProjectView>("project.apply", { command: cmd }).then((v) => {
        accept(v);
        return v;
      }),
    [accept],
  );

  // Replace the score in the engine. Typing not yet sent goes first, so the
  // engine keeps it as the version being replaced. Errors go to the caller.
  const replace = useCallback(
    async (method: string, params: unknown) => {
      setError(null);
      window.clearTimeout(timer.current);
      const unsent = pending.current;
      pending.current = null;
      if (unsent !== null) accept(await request<ProjectView>("project.setScore", { text: unsent }));
      const v = await request<ProjectView>(method, params);
      accept(v);
      setText(v.scoreText);
    },
    [accept],
  );

  return {
    project,
    text,
    progress,
    transport,
    peaks,
    timing,
    error,
    edit,
    play: () => transportCommand("transport.play"),
    pause: () => transportCommand("transport.pause"),
    stop: () => transportCommand("transport.stop"),
    seek: (seconds: number) => dragCommand("transport.seek", { seconds }),
    setLoop: (loop: LoopRange | null) => transportCommand("transport.setLoop", { loop }),
    setVolume: (volume: number) => dragCommand("transport.setVolume", { volume }),
    retryRender: () => request("render.start").then(refresh).catch(report),
    command,
    cancelRender: () => request("render.cancel").then(refresh).catch(report),
    generate: (settings: GenSettings) => replace("project.generate", { settings }),
    restoreVersion: (id: number) => replace("project.restoreVersion", { id }),
  };
}
