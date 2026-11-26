import assert from "node:assert/strict";
import { test } from "node:test";
import { baseName, bytes, decibels, DEFAULT_FORMAT, fileNameFor, GAIN, panLabel, readFormat, withWav } from "../src/mix.ts";

test("gain reads in decibels, pan in words", () => {
  assert.equal(decibels(1), "0.0 dB");
  assert.equal(decibels(0.5), "−6.0 dB");
  assert.equal(decibels(2), "+6.0 dB");
  assert.equal(decibels(0), "Off");
  assert.equal(panLabel(0), "Centre");
  assert.equal(panLabel(-0.25), "25% left");
  assert.equal(panLabel(1), "100% right");
  assert.deepEqual([GAIN.min, GAIN.max], [0, 2], "the engine's limits");
});

test("the export format falls back to the default for anything unknown", () => {
  assert.deepEqual(readFormat(JSON.stringify({ sampleRate: 48000, bitDepth: 16 })), { sampleRate: 48000, bitDepth: 16 });
  assert.deepEqual(readFormat(JSON.stringify({ sampleRate: 22050, bitDepth: 16 })), DEFAULT_FORMAT);
  assert.deepEqual(readFormat("not json"), DEFAULT_FORMAT);
  assert.deepEqual(readFormat(null), DEFAULT_FORMAT);
});

test("file names are safe and always .wav", () => {
  assert.equal(fileNameFor("Generated: D dorian, 98 bpm"), "Generated D dorian, 98 bpm.wav");
  assert.equal(fileNameFor('a/b\\c*?"<>|'), "a b c.wav");
  assert.equal(fileNameFor("  ...  "), "Song.wav");
  assert.equal(fileNameFor("Trailing dot."), "Trailing dot.wav");
  assert.equal(withWav("C:\\x\\song"), "C:\\x\\song.wav");
  assert.equal(withWav("C:\\x\\song.WAV"), "C:\\x\\song.WAV");
  assert.equal(baseName("C:\\Users\\me\\song.wav"), "song.wav");
  assert.equal(bytes(2048), "2 KB");
  assert.equal(bytes(5 * 1024 * 1024), "5.0 MB");
});
