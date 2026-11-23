// Score maths that needs no DOM: slices of the text, what sounds when, bar
// labels, and problems. The engine supplies the outline and timing; these
// only map them onto the text being edited.
import type { Diagnostic, Outline, TrackTiming } from "./protocol";

export interface Slice {
  /** First line of the slice (1-based). */
  first: number;
  /** Last line of the slice (1-based, inclusive). */
  last: number;
}

export const lines = (text: string): string[] => text.split("\n");

/** The lines of a track block, from its header to the line before the next. */
export function sliceOf(outline: Outline, name: string): Slice | null {
  const t = outline.tracks.find((x) => x.name === name);
  return t ? { first: t.header, last: t.last } : null;
}

export const sliceText = (text: string, s: Slice): string => lines(text).slice(s.first - 1, s.last).join("\n");

/**
 * Put an edited slice back into the whole text. Everything outside the slice
 * is kept exactly, and the returned slice covers the edited lines.
 */
export function replaceSlice(text: string, s: Slice, edited: string): { text: string; slice: Slice } {
  const all = lines(text);
  const inner = lines(edited);
  const next = [...all.slice(0, s.first - 1), ...inner, ...all.slice(s.last)];
  return { text: next.join("\n"), slice: { first: s.first, last: s.first + inner.length - 1 } };
}

/** Character offset of the start of a 1-based line, within a text. */
export function offsetOfLine(text: string, line: number): number {
  let offset = 0;
  const ls = lines(text);
  for (let i = 0; i < line - 1 && i < ls.length; i++) offset += ls[i].length + 1;
  return offset;
}

/** 1-based line of a character offset. */
export const lineAt = (text: string, offset: number): number => lines(text.slice(0, offset)).length;

/** Lines sounding at a moment, across all tracks. */
export function soundingLines(timing: TrackTiming[], seconds: number): Set<number> {
  const out = new Set<number>();
  for (const t of timing) for (const [line, start, end] of t.lines) if (seconds >= start && seconds < end) out.add(line);
  return out;
}

/** The first line of each bar in a track, labelled with the bar number. */
export function barLabels(t: TrackTiming, beatsPerBar: number): Map<number, number> {
  const out = new Map<number, number>();
  let previous = -1;
  for (const [line, , , beat] of t.lines) {
    const bar = Math.floor(beat / beatsPerBar + 1e-9) + 1;
    if (bar !== previous) out.set(line, bar);
    previous = bar;
  }
  return out;
}

/** Start and end, in seconds, of the bar a line falls in. */
export function barAround(t: TrackTiming, line: number, beatsPerBar: number, bpm: number): [number, number] | null {
  const hit = t.lines.find(([l]) => l === line);
  if (!hit) return null;
  const beatSeconds = 60 / bpm;
  const bar = Math.floor(hit[3] / beatsPerBar + 1e-9);
  return [bar * beatsPerBar * beatSeconds, (bar + 1) * beatsPerBar * beatSeconds];
}

/** The next (or previous) problem after a line, wrapping around. */
export function nextProblem(diagnostics: Diagnostic[], line: number, backwards = false): Diagnostic | null {
  const located = diagnostics.filter((d) => d.line !== null).sort((a, b) => a.line! - b.line!);
  if (located.length === 0) return null;
  if (backwards) return [...located].reverse().find((d) => d.line! < line) ?? located[located.length - 1];
  return located.find((d) => d.line! > line) ?? located[0];
}

/** Shorten a long name in the middle, where names that share a prefix still differ. */
export function shorten(name: string, max = 24): string {
  if (name.length <= max) return name;
  const tail = Math.floor((max - 1) / 2);
  return name.slice(0, max - 1 - tail) + "…" + name.slice(name.length - tail);
}

/** A panel size kept within its limits. */
export const clampSize = (size: number, min: number, max: number): number => Math.round(Math.min(max, Math.max(min, size)));
