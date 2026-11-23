// The sound-led workspace: tracks, the selected track's sound, its part of
// the score, the timeline, and the transport.
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import type { Diagnostic, LoopRange, ProjectView, RenderPeaks, RenderProgress, ScoreTiming, TransportView } from "./protocol";
import { ScoreEditor, type EditorHandle } from "./ScoreEditor";
import { barAround, barLabels, nextProblem, replaceSlice, sliceOf, sliceText, type Slice } from "./score";
import { SoundView } from "./SoundView";
import { Splitter } from "./Splitter";
import { trackColour, TrackList } from "./TrackList";
import { Transport, type Lane } from "./Transport";

interface Props {
  project: ProjectView;
  text: string;
  progress: RenderProgress | null;
  transport: TransportView | null;
  peaks: RenderPeaks | null;
  timing: ScoreTiming | null;
  error: string | null;
  onEdit: (text: string) => void;
  onPlay: () => void;
  onPause: () => void;
  onStop: () => void;
  onSeek: (seconds: number) => void;
  onLoop: (loop: LoopRange | null) => void;
  onVolume: (volume: number) => void;
  onRetryRender: () => void;
}

function renderStatus(project: ProjectView, progress: RenderProgress | null): string {
  const r = project.render;
  switch (r.state) {
    case "rendering": {
      const p = progress && r.running && progress.jobId === r.running.jobId ? progress : null;
      return p ? `Rendering ${p.completed} of ${p.total}` : "Rendering";
    }
    case "ready":
      return r.latest ? `Rendered revision ${r.latest.revision} in ${(r.latest.renderMillis / 1000).toFixed(1)} s` : "Rendered";
    case "outdated":
      return r.latest ? `Playing revision ${r.latest.revision}; the latest changes are not rendered` : "Out of date";
    case "failed":
      return "Render failed";
    default:
      return "Not rendered yet";
  }
}

export function scoreState(project: ProjectView, text: string): { tone: "good" | "bad" | ""; words: string } {
  if (project.scoreText !== text) return { tone: "", words: "Checking the score" };
  if (project.scoreIsCurrent) return { tone: "good", words: "Score is valid" };
  const n = project.diagnostics.filter((d) => d.severity === "error").length;
  return { tone: "bad", words: `${n} problem${n === 1 ? "" : "s"}. Playing the last valid version` };
}

// Layout sizes are a per-person convenience; storage may be unavailable.
const SIZES_KEY = "sonora.layout.v1";
function loadSizes(): { tracks: number; score: number } {
  try {
    const s = JSON.parse(localStorage.getItem(SIZES_KEY) ?? "");
    if (typeof s.tracks === "number" && typeof s.score === "number") return s;
  } catch {
    /* defaults */
  }
  return { tracks: 216, score: 380 };
}

const typing = (el: EventTarget | null) =>
  el instanceof HTMLElement && (el.closest("textarea, input, select, button, [role=slider], [role=separator], [role=listbox]") !== null || el.isContentEditable);

export function Workspace(props: Props) {
  const { project, text, transport: liveTransport } = props;
  const transport = liveTransport ?? project.transport;
  const tracks = project.tracks;
  const duration = project.render.latest?.durationSeconds ?? 0;

  const [selectedId, setSelectedId] = useState<string | null>(tracks[0]?.id ?? null);
  const selected = tracks.find((t) => t.id === selectedId) ?? tracks[0] ?? null;
  const selectedIndex = selected ? tracks.indexOf(selected) : 0;
  useEffect(() => {
    if (selected && selected.id !== selectedId) setSelectedId(selected.id);
  }, [selected, selectedId]);

  const [whole, setWhole] = useState(false);
  const [sizes, setSizes] = useState(loadSizes);
  const [width, setWidth] = useState(window.innerWidth);
  const [tab, setTab] = useState<"sound" | "score">("score");
  const compact = width < 1080;
  useEffect(() => {
    const resize = () => setWidth(window.innerWidth);
    window.addEventListener("resize", resize);
    return () => window.removeEventListener("resize", resize);
  }, []);
  useEffect(() => {
    try {
      localStorage.setItem(SIZES_KEY, JSON.stringify(sizes));
    } catch {
      /* not remembered */
    }
  }, [sizes]);

  // The slice of the text being edited. It comes from the engine's outline
  // when the engine has seen this text, and is carried along by our own
  // edits in between.
  const settled = project.scoreText === text;
  const [carried, setCarried] = useState<{ name: string; slice: Slice } | null>(null);
  const outlined = selected ? sliceOf(project.outline, selected.name) : null;
  const slice = settled ? outlined : carried && carried.name === selected?.name ? carried.slice : outlined;
  const showWhole = whole || !slice || !selected;
  const shown = showWhole ? { first: 1, last: text.split("\n").length } : slice!;
  const shownText = showWhole ? text : sliceText(text, slice!);

  const edit = (next: string) => {
    if (showWhole || !selected) return props.onEdit(next);
    const result = replaceSlice(text, slice!, next);
    setCarried({ name: selected.name, slice: result.slice });
    props.onEdit(result.text);
  };

  // Timing applies only while the editor shows exactly the valid score.
  const timing = props.timing && settled && project.scoreIsCurrent && props.timing.version === project.timingVersion ? props.timing.tracks : null;
  const song = project.song;
  const selectedTiming = timing?.find((t) => t.track === selected?.id) ?? null;
  const onSeek = props.onSeek;
  const bars = useMemo(() => {
    const labels = new Map<number, number>();
    const starts = new Map<number, number>();
    if (!timing || !song) return { labels, starts };
    for (const t of showWhole ? timing : selectedTiming ? [selectedTiming] : []) {
      for (const [line, bar] of barLabels(t, song.beatsPerBar)) {
        labels.set(line, bar);
        starts.set(line, ((bar - 1) * song.beatsPerBar * 60) / song.bpm);
      }
    }
    return { labels, starts };
  }, [timing, song, showWhole, selectedTiming]);

  // Problems: those on shown lines are marked; all are listed, by track.
  const diagnostics = settled ? project.diagnostics : [];
  const owner = useCallback(
    (d: Diagnostic) => (d.line === null ? null : project.outline.tracks.find((t) => d.line! >= t.header && d.line! <= t.last)?.name ?? null),
    [project.outline],
  );
  const perTrack = useMemo(() => {
    const m = new Map<string, number>();
    for (const d of diagnostics) {
      const n = d.severity === "error" ? owner(d) : null;
      if (n) m.set(n, (m.get(n) ?? 0) + 1);
    }
    return m;
  }, [diagnostics, owner]);
  const onShown = diagnostics.filter((d) => d.line !== null && d.line >= shown.first && d.line <= shown.last);

  const editor = useRef<EditorHandle>(null);
  const scrolls = useRef(new Map<string, number>()).current;
  const caretLine = useRef(1);
  const [pendingReveal, setPendingReveal] = useState<Diagnostic | null>(null);

  // Show a problem: switch to its track (or the whole score), then reveal it.
  const reveal = (d: Diagnostic) => {
    if (d.line === null) return;
    if (compact) setTab("score");
    const name = owner(d);
    const t = tracks.find((x) => x.name === name);
    if (!whole && t && t.id !== selected?.id) setSelectedId(t.id);
    else if (!whole && !t) setWhole(true);
    setPendingReveal(d);
  };
  useEffect(() => {
    if (!pendingReveal) return;
    editor.current?.reveal(pendingReveal.line!, pendingReveal.column);
    setPendingReveal(null);
  }, [pendingReveal, shown.first]);

  // In the whole score, the sound follows the track the caret is in.
  const onCaretLine = (line: number) => {
    caretLine.current = line;
    if (!showWhole) return;
    const name = project.outline.tracks.find((t) => line >= t.header && line <= t.last)?.name;
    const t = tracks.find((x) => x.name === name);
    if (t && t.id !== selected?.id) setSelectedId(t.id);
  };

  const canPlay = project.render.latest !== null;
  const loopBar = () => {
    if (!canPlay) return;
    const line = editor.current?.caretLine() ?? caretLine.current;
    const t = timing?.find((x) => x.lines.some(([l]) => l === line));
    const around = t && song ? barAround(t, line, song.beatsPerBar, song.bpm) : null;
    if (around) props.onLoop({ start: around[0], end: Math.min(around[1], duration) });
    else props.onLoop(transport.loopActive ? null : { start: 0, end: duration });
  };

  useEffect(() => {
    const keys = (e: KeyboardEvent) => {
      const ctrl = e.ctrlKey || e.metaKey;
      const playPause = () => (transport.playing ? props.onPause() : canPlay && props.onPlay());
      if (e.key === " " && ((ctrl && !e.shiftKey) || (!ctrl && !e.altKey && !typing(e.target)))) {
        e.preventDefault();
        playPause();
      } else if (ctrl && e.key === ".") {
        e.preventDefault();
        props.onStop();
      } else if (ctrl && (e.key === "l" || e.key === "L")) {
        e.preventDefault();
        loopBar();
      } else if (e.key === "F8") {
        e.preventDefault();
        const d = nextProblem(diagnostics, editor.current?.caretLine() ?? caretLine.current, e.shiftKey);
        if (d) reveal(d);
      } else if (ctrl && /^[1-9]$/.test(e.key) && !e.shiftKey) {
        const t = tracks[Number(e.key) - 1];
        if (t) {
          e.preventDefault();
          setSelectedId(t.id);
        }
      } else if (ctrl && (e.key === "e" || e.key === "E")) {
        e.preventDefault();
        if (compact) setTab("score");
        setTimeout(() => editor.current?.focus());
      }
    };
    window.addEventListener("keydown", keys);
    return () => window.removeEventListener("keydown", keys);
  });

  const lanes: Lane[] = (timing ?? []).map((t) => {
    const i = tracks.findIndex((x) => x.id === t.track);
    return { id: t.track, colour: trackColour(Math.max(0, i)), notes: t.notes, selected: t.track === selected?.id };
  });

  const scoreLabel = showWhole ? "Score" : `Score of ${selected!.name}`;
  const scorePane = (
    <section className="score-pane" aria-label={scoreLabel}>
      <header className="pane-head">
        <h2>{showWhole ? "Whole score" : `Score: ${selected!.name}`}</h2>
        <span className="grow" />
        {selected ? (
          <button type="button" className="button" aria-pressed={whole} onClick={() => setWhole((w) => !w)} title="Show the whole score">
            Whole score
          </button>
        ) : null}
      </header>
      {!whole && selected && !slice ? <p className="note">The header of track {selected.name} is not in the text; showing the whole score.</p> : null}
      <ScoreEditor
        ref={editor}
        text={shownText}
        firstLine={shown.first}
        label={scoreLabel}
        problems={onShown}
        bars={bars.labels}
        barStarts={bars.starts}
        timing={timing}
        transport={transport}
        duration={duration}
        scrollKey={showWhole ? "whole" : `track:${selected!.id}`}
        scrolls={scrolls}
        onChange={edit}
        onCaretLine={onCaretLine}
        onSeek={onSeek}
      />
      {diagnostics.length > 0 ? (
        <div className="problems" aria-label="Problems">
          <ul>
            {diagnostics.map((d, i) => {
              const where = owner(d);
              return (
                <li key={i} className={d.severity}>
                  <button type="button" onClick={() => reveal(d)} disabled={d.line === null}>
                    <b>
                      {d.line === null ? (d.severity === "error" ? "Problem" : "Warning") : `Line ${d.line}`}
                      {where && where !== selected?.name ? `, ${where}` : ""}
                    </b>
                    <span>{d.message}</span>
                  </button>
                </li>
              );
            })}
          </ul>
        </div>
      ) : null}
    </section>
  );
  const sound = <SoundView track={selected} colour={trackColour(selectedIndex)} />;

  const maxScore = Math.max(300, width - sizes.tracks - 360);
  const columns = compact ? `${Math.min(sizes.tracks, 200)}px minmax(0, 1fr)` : `${sizes.tracks}px 0 minmax(320px, 1fr) 0 ${Math.min(sizes.score, maxScore)}px`;

  return (
    <>
      <div className="notice-slot">
      {props.error || project.render.state === "failed" ? (
        <div className="notice" role="alert">
          <span>{props.error ?? `The song could not be rendered: ${project.render.failure?.message ?? "unknown error"}`}</span>
          {project.render.state === "failed" ? (
            <button type="button" className="button" onClick={props.onRetryRender}>
              Try again
            </button>
          ) : null}
        </div>
      ) : null}
      </div>
      <main className="work" style={{ gridTemplateColumns: columns }}>
        <TrackList tracks={tracks} selected={selected?.id ?? null} problems={perTrack} onSelect={setSelectedId} />
        {compact ? (
          <div className="centre">
            <div className="tabs" role="tablist" aria-label="View">
              {(["sound", "score"] as const).map((t) => (
                <button key={t} type="button" role="tab" aria-selected={tab === t} className="tab" onClick={() => setTab(t)}>
                  {t === "sound" ? "Sound" : "Score"}
                </button>
              ))}
            </div>
            {tab === "sound" ? sound : scorePane}
          </div>
        ) : (
          <>
            <Splitter label="Track list width" size={sizes.tracks} min={160} max={320} direction={1} onSize={(tracksW) => setSizes((s) => ({ ...s, tracks: tracksW }))} />
            {sound}
            <Splitter label="Score width" size={Math.min(sizes.score, maxScore)} min={300} max={maxScore} direction={-1} onSize={(score) => setSizes((s) => ({ ...s, score }))} />
            {scorePane}
          </>
        )}
      </main>
      <Transport
        transport={transport}
        peaks={props.peaks}
        duration={duration}
        lanes={lanes}
        canPlay={canPlay}
        status={renderStatus(project, props.progress)}
        onPlay={props.onPlay}
        onPause={props.onPause}
        onStop={props.onStop}
        onSeek={props.onSeek}
        onLoop={props.onLoop}
        onVolume={props.onVolume}
      />
    </>
  );
}
