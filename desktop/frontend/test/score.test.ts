import assert from "node:assert/strict";
import { test } from "node:test";
import type { Diagnostic, Outline, TrackTiming } from "../src/protocol.ts";
import { barAround, barLabels, clampSize, shorten, lineAt, nextProblem, offsetOfLine, replaceSlice, sliceOf, sliceText, soundingLines } from "../src/score.ts";

const text = ["title \"T\"", "", "track a (lead):", "    C4 quarter", "    E4 half", "", "track b (bass):", "    C2 whole", ""].join("\n");
const outline: Outline = { lines: 9, tracks: [{ name: "a", header: 3, last: 6 }, { name: "b", header: 7, last: 9 }] };

test("a track's slice is its block of lines", () => {
  assert.deepEqual(sliceOf(outline, "a"), { first: 3, last: 6 });
  assert.equal(sliceOf(outline, "nope"), null);
  assert.equal(sliceText(text, { first: 3, last: 6 }), "track a (lead):\n    C4 quarter\n    E4 half\n");
});

test("editing a slice keeps everything else byte for byte", () => {
  const s = sliceOf(outline, "a")!;
  const same = replaceSlice(text, s, sliceText(text, s));
  assert.equal(same.text, text);
  assert.deepEqual(same.slice, s);
  const grown = replaceSlice(text, s, "track a (lead):\n    C4 quarter\n    D4 quarter\n    E4 half\n");
  assert.equal(grown.text, text.replace("    C4 quarter\n", "    C4 quarter\n    D4 quarter\n"));
  assert.deepEqual(grown.slice, { first: 3, last: 7 });
  const emptied = replaceSlice(text, s, "");
  assert.equal(emptied.text, ["title \"T\"", "", "", "track b (bass):", "    C2 whole", ""].join("\n"));
  const crlf = "a\r\nb\r\nc";
  assert.equal(replaceSlice(crlf, { first: 2, last: 2 }, "b\r").text, crlf);
});

test("offsets and lines convert both ways", () => {
  for (let line = 1; line <= 9; line++) assert.equal(lineAt(text, offsetOfLine(text, line)), line);
  assert.equal(offsetOfLine(text, 3), 11);
});

const timing: TrackTiming[] = [
  { track: "t1", lines: [[4, 0, 0.5, 0], [5, 0.5, 1.5, 1]], notes: [] },
  { track: "t2", lines: [[8, 0, 2, 0]], notes: [] },
];

test("sounding lines include every track, half-open in time", () => {
  assert.deepEqual([...soundingLines(timing, 0)].sort(), [4, 8]);
  assert.deepEqual([...soundingLines(timing, 0.5)].sort(), [5, 8]);
  assert.deepEqual([...soundingLines(timing, 2)], []);
});

test("bars label the first line in each bar", () => {
  const t: TrackTiming = { track: "t", lines: [[1, 0, 0, 0], [2, 0, 0, 2], [3, 0, 0, 3], [4, 0, 0, 5.5], [5, 0, 0, 6]], notes: [] };
  assert.deepEqual([...barLabels(t, 3)], [[1, 1], [3, 2], [5, 3]]);
  assert.deepEqual(barAround(t, 4, 3, 120), [1.5, 3]);
  assert.equal(barAround(t, 99, 3, 120), null);
});

test("next problem wraps in both directions and skips unplaced ones", () => {
  const d = (line: number | null): Diagnostic => ({ severity: "error", line, column: null, message: "" });
  const all = [d(30), d(null), d(10), d(20)];
  assert.equal(nextProblem(all, 10)!.line, 20);
  assert.equal(nextProblem(all, 30)!.line, 10);
  assert.equal(nextProblem(all, 10, true)!.line, 30);
  assert.equal(nextProblem(all, 25, true)!.line, 20);
  assert.equal(nextProblem([d(null)], 1), null);
});

test("long names keep their distinct ends", () => {
  assert.equal(shorten("bass"), "bass");
  const a = shorten("track_with_a_rather_long_descriptive_014");
  const b = shorten("track_with_a_rather_long_descriptive_015");
  assert.equal(a.length, 24);
  assert.notEqual(a, b);
  assert.ok(a.endsWith("_014") && a.startsWith("track_with"));
});

test("panel sizes stay within limits", () => {
  assert.equal(clampSize(10, 160, 320), 160);
  assert.equal(clampSize(999, 160, 320), 320);
  assert.equal(clampSize(200.6, 160, 320), 201);
});
