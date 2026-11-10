// Thin client for the Sonora engine, reached through the Go host. The host
// forwards calls and events unchanged; all behavior lives in the engine.
import { Events } from "@wailsio/runtime";
import { EngineService } from "../bindings/github.com/adityapandeydev/Sonora/desktop";
import type { Reply } from "../bindings/github.com/adityapandeydev/Sonora/desktop/models";
import { State, type Status } from "../bindings/github.com/adityapandeydev/Sonora/desktop/engine/models";
import { EngineEvent, EngineStatus } from "../bindings/events";

export { State };
export type { Reply, Status };

export const engineStatus = (): Promise<Status> => EngineService.Status();

export const restartEngine = (): Promise<Status> => EngineService.Restart();

/** Send a request to the engine. Cancel the returned promise to cancel it there. */
export const callEngine = (method: string, params: unknown = {}) =>
  EngineService.Call(method, params);

/** Subscribe to engine status changes. Returns an unsubscribe function. */
export const onEngineStatus = (handler: (status: Status) => void): (() => void) =>
  Events.On(EngineStatus, (ev) => handler(ev.data));

/** Subscribe to events emitted by the engine. Returns an unsubscribe function. */
export const onEngineEvent = (handler: (name: string, data: unknown) => void): (() => void) =>
  Events.On(EngineEvent, (ev) => handler(ev.data.name, ev.data.data));
