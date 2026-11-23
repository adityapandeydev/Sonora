import { useEffect, useRef, type KeyboardEvent } from "react";
import { shorten } from "./score";
import type { TrackView } from "./protocol";

interface Props {
  tracks: TrackView[];
  selected: string | null;
  /** Problems per track name. */
  problems: Map<string, number>;
  onSelect: (id: string) => void;
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

export function TrackList({ tracks, selected, problems, onSelect }: Props) {
  const list = useRef<HTMLUListElement>(null);

  // Keep the selected track in view, however it was selected.
  useEffect(() => {
    if (selected) document.getElementById(`track-${selected}`)?.scrollIntoView({ block: "nearest" });
  }, [selected]);

  const move = (e: KeyboardEvent) => {
    const i = tracks.findIndex((t) => t.id === selected);
    const to = { ArrowDown: i + 1, ArrowUp: i - 1, Home: 0, End: tracks.length - 1 }[e.key];
    if (to === undefined || tracks.length === 0) return;
    e.preventDefault();
    onSelect(tracks[Math.max(0, Math.min(tracks.length - 1, to))].id);
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
        <ul
          ref={list}
          className="track-list"
          role="listbox"
          aria-label="Tracks"
          tabIndex={0}
          aria-activedescendant={selected ? `track-${selected}` : undefined}
          onKeyDown={move}
        >
          {tracks.map((t, i) => {
            const n = problems.get(t.name) ?? 0;
            return (
              <li
                key={t.id}
                id={`track-${t.id}`}
                role="option"
                aria-selected={t.id === selected}
                className={t.id === selected ? "track on" : "track"}
                style={{ "--tc": trackColour(i) } as React.CSSProperties}
                onClick={() => {
                  onSelect(t.id);
                  list.current?.focus();
                }}
              >
                <span className="swatch" />
                <b title={t.name}>{shorten(t.name)}</b>
                {n > 0 ? (
                  <span className="count" aria-label={`${n} problem${n === 1 ? "" : "s"}`}>
                    {n}
                  </span>
                ) : null}
                <small title={describeSource(t)}>{describeSource(t)}</small>
              </li>
            );
          })}
        </ul>
      )}
    </section>
  );
}
