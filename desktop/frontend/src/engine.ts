// Thin client for the Sonora engine, reached through the Go host. The host
// forwards calls and events unchanged; all behavior lives in the engine.
import { CancellablePromise, Events } from "@wailsio/runtime";
import { EngineService } from "../bindings/github.com/adityapandeydev/Sonora/desktop";
import type { Reply } from "../bindings/github.com/adityapandeydev/Sonora/desktop/models";
import { State, type Status } from "../bindings/github.com/adityapandeydev/Sonora/desktop/engine/models";
import { EngineEvent, EngineStatus } from "../bindings/events";

export { State };
export type { Reply, Status };

/** A structured error returned by the engine (or `engine_unavailable`). */
export class EngineError extends Error {
  constructor(
    readonly code: string,
    message: string,
    readonly data?: unknown,
  ) {
    super(message);
    this.name = "EngineError";
  }
}

export const engineStatus = (): Promise<Status> => EngineService.Status();

export const restartEngine = (): Promise<Status> => EngineService.Restart();

/** Send a request to the engine. Cancel the returned promise to cancel it there. */
export const callEngine = (method: string, params: unknown = {}) =>
  EngineService.Call(method, params);

/**
 * Send a request and resolve with its result, or reject with an
 * EngineError. Cancelling the promise cancels the request in the engine.
 */
export function request<T>(method: string, params: unknown = {}): CancellablePromise<T> {
  return EngineService.Call(method, params).then((reply) => {
    if (reply.ok) return reply.result as T;
    const err = reply.error;
    throw new EngineError(err?.code ?? "unknown", err?.message ?? "Unknown engine error", err?.data);
  });
}

/** Subscribe to engine status changes. Returns an unsubscribe function. */
export const onEngineStatus = (handler: (status: Status) => void): (() => void) =>
  Events.On(EngineStatus, (ev) => handler(ev.data));

/** Subscribe to events emitted by the engine. Returns an unsubscribe function. */
export const onEngineEvent = (handler: (name: string, data: unknown) => void): (() => void) =>
  Events.On(EngineEvent, (ev) => handler(ev.data.name, ev.data.data));
