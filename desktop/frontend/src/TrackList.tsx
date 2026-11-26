// Tracks and their mix. Each row selects a track and holds its mute and solo;
// the selected row also shows its level and pan. The master level at the
// bottom is part of the mix and the export (the transport's volume is not).
import { useEffect, useRef, type KeyboardEvent } from "react";
import { decibels, GAIN, PAN, panLabel } from "./mix";
import type { TrackMix, TrackView } from "./protocol";
import { shorten } from "./score";
import { Slider } from "./ui/Slider";

export type MixChange = Partial<TrackMix>;

interface Props {
  tracks: TrackView[];
  selected: string | null;
  /** Problems per track name. */
  problems: Map<string, number>;
  master: number;
  onSelect: (id: string) => void;
  onMix: (track: TrackView, change: MixChange) => void;
  onMaster: (gain: number) => void;
}

export const trackColour = (index: number) => `var(--track-${index % 6})`;

export function describeSource(t: TrackView): string {
  switch (t.patchSource) {
    case "override":
      return "Project sound";
    case "score":
      return `Score instrument ${t.instrument}`;
    case "preset":
      return `Built-in ${t.instrument}`;
    default:
      return `Default sound, no instrument named ${t.instrument}`;
  }
}

export function TrackList({ tracks, selected, problems, master, onSelect, onMix, onMaster }: Props) {
  const list = useRef<HTMLUListElement>(null);
  const soloing = tracks.some((t) => t.mix.solo);

  // Keep the selected track in view, however it was selected.
  useEffect(() => {
    if (selected) document.getElementById(`track-${selected}`)?.scrollIntoView({ block: "nearest" });
  }, [selected]);

  // Arrow keys move between tracks; M and S mute and solo the selected one.
  const keys = (e: KeyboardEvent) => {
    const t = e.target as HTMLElement;
    if (!t.classList.contains("track-select")) return;
    const i = tracks.findIndex((x) => x.id === selected);
    const to = { ArrowDown: i + 1, ArrowUp: i - 1, Home: 0, End: tracks.length - 1 }[e.key];
    if (to !== undefined && tracks.length > 0) {
      e.preventDefault();
      const next = tracks[Math.max(0, Math.min(tracks.length - 1, to))];
      onSelect(next.id);
      requestAnimationFrame(() => document.querySelector<HTMLButtonElement>(`#track-${next.id} .track-select`)?.focus());
      return;
    }
    const current = tracks[i];
    if (!current || e.ctrlKey || e.metaKey || e.altKey) return;
    if (e.key === "m" || e.key === "M") onMix(current, { mute: !current.mix.mute });
    else if (e.key === "s" || e.key === "S") onMix(current, { solo: !current.mix.solo });
  };

  return (
    <section className="tracks" aria-label="Tracks">
      <header className="pane-head">
        <h2>Tracks</h2>
        <span className="sub">{tracks.length}</span>
      </header>
      {tracks.length === 0 ? (
        <p className="empty">Tracks appear here once the score has a valid track.</p>
      ) : (
        <ul ref={list} className="track-list" onKeyDown={keys}>
          {tracks.map((t, i) => {
            const n = problems.get(t.name) ?? 0;
            const on = t.id === selected;
            const silent = t.mix.mute || (soloing && !t.mix.solo);
            return (
              <li
                key={t.id}
                id={`track-${t.id}`}
                className={`track${on ? " on" : ""}${silent ? " silent" : ""}`}
                style={{ "--tc": trackColour(i) } as React.CSSProperties}
                onClick={() => onSelect(t.id)}
              >
                <button type="button" className="track-select" aria-current={on ? "true" : undefined} tabIndex={on || (!selected && i === 0) ? 0 : -1} title={`${t.name}. ${describeSource(t)}`}>
                  <span className="swatch" />
                  <b>{shorten(t.name)}</b>
                  {n > 0 ? (
                    <span className="count" aria-label={`${n} problem${n === 1 ? "" : "s"}`}>
                      {n}
                    </span>
                  ) : null}
                  <small>{silent ? (t.mix.mute ? "Muted" : "Not soloed") : describeSource(t)}</small>
                </button>
                <span className="mix-toggles" onClick={(e) => e.stopPropagation()}>
                  <button type="button" className="toggle" aria-pressed={t.mix.mute} aria-label={`Mute ${t.name}`} title="Mute (M)" onClick={() => onMix(t, { mute: !t.mix.mute })}>
                    M
                  </button>
                  <button type="button" className="toggle solo" aria-pressed={t.mix.solo} aria-label={`Solo ${t.name}`} title="Solo (S)" onClick={() => onMix(t, { solo: !t.mix.solo })}>
                    S
                  </button>
                </span>
                {on ? (
                  <div className="track-mix" onClick={(e) => e.stopPropagation()}>
                    <Slider label="Level" value={t.mix.gain} range={GAIN} defaultValue={1} format={decibels} onInput={() => {}} onCommit={(v) => onMix(t, { gain: v })} />
                    <Slider label="Pan" value={t.mix.pan} range={PAN} defaultValue={0} origin={0} format={panLabel} onInput={() => {}} onCommit={(v) => onMix(t, { pan: v })} />
                  </div>
                ) : null}
              </li>
            );
          })}
        </ul>
      )}
      <footer className="master" title="Part of the mix and of exported files. The volume by the transport only changes what you hear.">
        <Slider label="Master" value={master} range={GAIN} defaultValue={1} format={decibels} onInput={() => {}} onCommit={onMaster} />
      </footer>
    </section>
  );
}
