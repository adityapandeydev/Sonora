import assert from "node:assert/strict";
import { test } from "node:test";
import { describeSettings, engineProblems, initialSettings, lengthOf, localProblems, randomSeed, scaleLabel } from "../src/generate.ts";
import type { GenSettings, ScoreVersion } from "../src/protocol.ts";

const base: GenSettings = { seed: 42, root: "D", scale: "dorian", bpm: 98, bars: 8 };
const version = (id: number, settings: GenSettings | null): ScoreVersion => ({ id, label: "", revision: id, settings, current: false, lines: 1 });

test("settings start from the newest generated version", () => {
  assert.deepEqual(initialSettings([version(3, null), version(2, base), version(1, { ...base, seed: 1 })]), base);
  const fresh = initialSettings([version(1, null)], () => 0.5);
  assert.deepEqual(fresh, { seed: 500000, root: "C", scale: "major", bpm: 110, bars: 8 });
});

test("the form catches what the engine would refuse", () => {
  assert.deepEqual(localProblems(base), {});
  assert.deepEqual(Object.keys(localProblems({ ...base, seed: -1, bpm: 300, bars: 0.5 })).sort(), ["bars", "bpm", "seed"]);
  assert.match(localProblems({ ...base, bpm: 40, bars: 64 }).bars!, /three minutes/);
  assert.equal(localProblems({ ...base, bpm: NaN }).bars, undefined);
  assert.equal(lengthOf({ bpm: 120, bars: 8 }), 16);
});

test("engine field errors map onto fields, first message wins", () => {
  const data = { params: [{ path: "root", message: "Unknown root H" }, { path: "root", message: "second" }, { path: "bpm", message: "Tempo" }, { nope: 1 }] };
  assert.deepEqual(engineProblems(data), { root: "Unknown root H", bpm: "Tempo" });
  assert.deepEqual(engineProblems(undefined), {});
  assert.deepEqual(engineProblems({ params: "x" }), {});
});

test("labels read well", () => {
  assert.equal(scaleLabel("naturalMinor"), "Minor");
  assert.equal(scaleLabel("someNewMode"), "Some New Mode");
  assert.equal(describeSettings(base), "D dorian, 98 bpm, 8 bars, seed 42");
  assert.ok(Number.isInteger(randomSeed()) && randomSeed(() => 0.999999) < 1_000_000);
});
