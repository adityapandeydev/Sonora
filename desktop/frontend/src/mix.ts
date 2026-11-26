// Mix and export values for display. The engine mixes and exports; these
// only describe and bound what the controls send.
import type { Range } from "./ui/range";

/** Linear gain, as the engine takes it: 1 leaves a track as rendered. */
export const GAIN: Range = { min: 0, max: 2, step: 0.01 };
export const PAN: Range = { min: -1, max: 1, step: 0.01 };

export function decibels(gain: number): string {
  if (gain <= 0) return "Off";
  const db = 20 * Math.log10(gain);
  const r = Math.round(db * 10) / 10;
  return `${r > 0 ? "+" : r < 0 ? "−" : ""}${Math.abs(r).toFixed(1)} dB`;
}

export function panLabel(pan: number): string {
  const p = Math.round(pan * 100);
  return p === 0 ? "Centre" : p < 0 ? `${-p}% left` : `${p}% right`;
}

export const EXPORT_RATES = [
  { value: 44100, label: "44.1 kHz (CD, streaming)" },
  { value: 48000, label: "48 kHz (video)" },
  { value: 96000, label: "96 kHz (studio)" },
];

export const EXPORT_DEPTHS = [
  { value: 16, label: "16-bit" },
  { value: 24, label: "24-bit" },
  { value: 32, label: "32-bit float" },
];

export interface ExportFormat {
  sampleRate: number;
  bitDepth: number;
}

export const DEFAULT_FORMAT: ExportFormat = { sampleRate: 44100, bitDepth: 24 };

/** A saved choice, or the default when it is missing or no longer offered. */
export function readFormat(saved: string | null): ExportFormat {
  try {
    const f = JSON.parse(saved ?? "");
    if (EXPORT_RATES.some((r) => r.value === f.sampleRate) && EXPORT_DEPTHS.some((d) => d.value === f.bitDepth)) return { sampleRate: f.sampleRate, bitDepth: f.bitDepth };
  } catch {
    /* default */
  }
  return DEFAULT_FORMAT;
}

/** A file name from a song title, safe on Windows. */
export function fileNameFor(title: string): string {
  const base = title.replace(/[<>:"/\\|?*\u0000-\u001f]/g, " ").replace(/\s+/g, " ").trim().replace(/[. ]+$/, "");
  return `${base || "Song"}.wav`;
}

/** Make sure a chosen path ends in .wav. */
export const withWav = (path: string) => (/\.wav$/i.test(path) ? path : `${path}.wav`);

export function bytes(n: number): string {
  if (n < 1024 * 1024) return `${Math.max(1, Math.round(n / 1024))} KB`;
  return `${(n / (1024 * 1024)).toFixed(1)} MB`;
}

export const baseName = (path: string) => path.split(/[\\/]/).pop() ?? path;
