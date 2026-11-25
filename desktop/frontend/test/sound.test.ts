import assert from "node:assert/strict";
import { test } from "node:test";
import type { Patch } from "../src/protocol.ts";
import {
  addEffect,
  addOscillator,
  hz,
  MAX_OSCILLATORS,
  moveEffect,
  noteName,
  processing,
  RANGES,
  removeEffect,
  removeOscillator,
  seconds,
  waveform,
  withEffect,
  withEnvelope,
  withOscillator,
} from "../src/sound.ts";

const env = { attack: 0.01, decay: 0.1, sustain: 0.7, release: 0.2, curve: "linear" as const };
const patch: Patch = {
  name: "lead",
  oscillators: [{ waveform: { type: "saw" }, level: 0.7, semitones: 0, cents: 0, phase: 0 }],
  filter: { mode: "lowpass", cutoff: 3000, resonance: 1.5, envelopeAmount: 2000, envelope: env },
  amplitudeEnvelope: env,
  // engine order: reverb is applied first
  effects: [
    { type: "delay", time: 0.25, feedback: 0.3, mix: 0.25 },
    { type: "reverb", roomSize: 0.5, mix: 0.2 },
  ],
};

test("effects show in processing order, the reverse of the engine's list", () => {
  assert.deepEqual(processing(patch).map((e) => e.type), ["reverb", "delay"]);
  const added = addEffect(patch, "distortion");
  assert.deepEqual(processing(added).map((e) => e.type), ["reverb", "delay", "distortion"]);
  assert.equal(added.effects[0].type, "distortion", "processed last, so first in the engine's list");
  const moved = moveEffect(patch, 0, 1);
  assert.deepEqual(processing(moved).map((e) => e.type), ["delay", "reverb"]);
  assert.equal(moveEffect(patch, 0, -1), patch);
  assert.deepEqual(processing(removeEffect(patch, 0)).map((e) => e.type), ["delay"]);
  const changed = withEffect(patch, 0, (e) => (e.type === "reverb" ? { ...e, mix: 0.9 } : e));
  assert.deepEqual(changed.effects[1], { type: "reverb", roomSize: 0.5, mix: 0.9 });
  assert.deepEqual(patch.effects[1], { type: "reverb", roomSize: 0.5, mix: 0.2 }, "the original is untouched");
});

test("oscillators stay between one and eight", () => {
  assert.equal(removeOscillator(patch, 0), patch);
  let p = patch;
  for (let i = 0; i < 10; i++) p = addOscillator(p);
  assert.equal(p.oscillators.length, MAX_OSCILLATORS);
  assert.equal(removeOscillator(p, 3).oscillators.length, MAX_OSCILLATORS - 1);
  const sq = withOscillator(patch, 0, (o) => ({ ...o, waveform: waveform("square", o.waveform) }));
  assert.deepEqual(sq.oscillators[0].waveform, { type: "square", duty: 0.5 });
  assert.deepEqual(waveform("square", { type: "square", duty: 0.2 }), { type: "square", duty: 0.2 });
  assert.equal(withEnvelope(patch, "filter", (e) => ({ ...e, attack: 1 })).amplitudeEnvelope.attack, 0.01);
});

test("ranges match the engine's limits", () => {
  assert.deepEqual([RANGES.cutoff.min, RANGES.cutoff.max], [20, 20000]);
  assert.deepEqual([RANGES.resonance.min, RANGES.resonance.max], [0.1, 20]);
  assert.deepEqual([RANGES.feedback.max, RANGES.drive.min, RANGES.drive.max], [0.95, 1, 50]);
  assert.deepEqual([RANGES.semitones.min, RANGES.cents.max, RANGES.duty.min], [-48, 100, 0.01]);
});

test("values read naturally", () => {
  assert.equal(hz(440), "440 Hz");
  assert.equal(hz(2500), "2.50 kHz");
  assert.equal(seconds(0.005), "5.0 ms");
  assert.equal(seconds(0.25), "250 ms");
  assert.equal(seconds(1.5), "1.50 s");
  assert.equal(noteName(60), "C4");
  assert.equal(noteName(69), "A4");
});
