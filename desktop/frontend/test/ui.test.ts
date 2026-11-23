import assert from "node:assert/strict";
import { test } from "node:test";
import { carryForward, clock, MIN_LOOP_SECONDS, moveLoop } from "../src/playback.ts";
import { fromPosition, nudge, snap, toPosition, type Range } from "../src/ui/range.ts";

const percent: Range = { min: 0, max: 1, step: 0.01 };
const pan: Range = { min: -1, max: 1, step: 0.01 };
const cutoff: Range = { min: 20, max: 20000, step: 1, scale: "log" };
const ranges = [percent, pan, cutoff];

test("positions and values round-trip within a step", () => {
  for (const r of ranges)
    for (let i = 0; i <= 200; i++) {
      const v = fromPosition(i / 200, r);
      assert.ok(v >= r.min && v <= r.max);
      assert.ok(Math.abs(fromPosition(toPosition(v, r), r) - v) <= r.step + 1e-9, `${v}`);
    }
});

test("ends map exactly and out-of-range input is clamped", () => {
  for (const r of ranges) {
    assert.equal(fromPosition(0, r), r.min);
    assert.equal(fromPosition(1, r), r.max);
    assert.equal(fromPosition(7, r), r.max);
    assert.equal(toPosition(r.min - 5, r), 0);
    assert.equal(toPosition(r.max + 5, r), 1);
  }
});

test("values carry no float noise", () => {
  assert.equal(snap(0.1 + 0.2, percent), 0.3);
  assert.equal(String(fromPosition(0.07, percent)), "0.07");
  assert.equal(fromPosition(0.5, pan), 0);
});

test("a log slider gives each octave the same room", () => {
  const octave = (f: number) => toPosition(2 * f, cutoff) - toPosition(f, cutoff);
  assert.ok(Math.abs(octave(100) - octave(5000)) < 1e-9);
});

test("every arrow press moves, from any value, until the end", () => {
  for (const r of ranges) {
    let v = r.min;
    let presses = 0;
    while (v < r.max) {
      const next = nudge(v, "up", r);
      assert.ok(next > v, `stuck at ${v}`);
      v = next;
      assert.ok(++presses < 5000);
    }
    assert.equal(nudge(r.max, "up", r), r.max);
    assert.equal(nudge(r.min, "down", r), r.min);
    assert.ok(nudge(r.max, "down", r) < r.max);
  }
  assert.equal(nudge(0.5, "pageUp", percent), 0.6);
  assert.equal(nudge(0.5, "min", percent), 0);
  assert.equal(nudge(0.5, "max", percent), 1);
});

test("playback position carries forward, wraps in a loop, and stops at the end", () => {
  assert.equal(carryForward(1, 0.5, 10, null), 1.5);
  assert.equal(carryForward(9.9, 0.5, 10, null), 10);
  assert.equal(carryForward(1, -3, 10, null), 1);
  const loop = { start: 2, end: 4 };
  assert.ok(Math.abs(carryForward(3.9, 0.3, 10, loop) - 2.2) < 1e-9);
  assert.ok(Math.abs(carryForward(3.9, 4.3, 10, loop) - 2.2) < 1e-9);
  assert.equal(carryForward(5, 0.5, 10, loop), 5.5); // past the loop: plays on
  assert.ok(Math.abs(carryForward(9.5, 1, 10, { start: 9, end: 12 }) - 9.5) < 1e-9); // loop cut short by the song
});

test("loop ends stay inside the song and apart", () => {
  const loop = { start: 2, end: 4 };
  assert.deepEqual(moveLoop(loop, "start", -1, 10), { start: 0, end: 4 });
  assert.deepEqual(moveLoop(loop, "start", 9, 10), { start: 4 - MIN_LOOP_SECONDS, end: 4 });
  assert.deepEqual(moveLoop(loop, "end", 99, 10), { start: 2, end: 10 });
  assert.deepEqual(moveLoop(loop, "end", 0, 10), { start: 2, end: 2 + MIN_LOOP_SECONDS });
});

test("the clock rounds without showing sixty seconds", () => {
  assert.equal(clock(0), "0:00.0");
  assert.equal(clock(59.97), "1:00.0");
  assert.equal(clock(125.24), "2:05.2");
});
