// A plain textarea over a highlighted copy of the same text, with a gutter of
// line numbers and bar labels. Native editing (undo, selection, input
// methods) stays the browser's.
import { forwardRef, memo, useEffect, useImperativeHandle, useMemo, useRef } from "react";
import type { Diagnostic, TrackTiming, TransportView } from "./protocol";
import { lines, offsetOfLine, soundingLines } from "./score";
import { usePosition } from "./Transport";

export const LINE_HEIGHT = 21;
const PAD = 8;

export interface EditorHandle {
  /** Put the caret on a line of the shown text and scroll it into view. */
  reveal: (line: number, column?: number | null) => void;
  focus: () => void;
  caretLine: () => number;
}

interface Props {
  text: string;
  /** Document line number of the first shown line. */
  firstLine: number;
  label: string;
  /** Problems on shown lines, by document line. */
  problems: Diagnostic[];
  /** Bar numbers by document line. */
  bars: Map<number, number>;
  /** Line timing for the shown text, or nothing when it does not apply. */
  timing: TrackTiming[] | null;
  transport: TransportView;
  duration: number;
  /** Remembers scroll positions between texts. */
  scrollKey: string;
  scrolls: Map<string, number>;
  onChange: (text: string) => void;
  onCaretLine: (documentLine: number) => void;
  onSeek: (seconds: number) => void;
  barStarts: Map<number, number>;
}

const WORDS = /(#.*$|\/\/.*$)|("[^"]*")|\b(track|title|tempo|time|instrument|chord|rest|repeat|oscillator|cutoff|resonance|envelope)\b|\b(doubleWhole|whole|half|quarter|eighth|sixteenth|dotted)\b/g;

// One line of highlighted text; only lines whose text changed re-render.
const Line = memo(function Line({ line }: { line: string }) {
  const out: (string | JSX.Element)[] = [];
  let at = 0;
  for (const m of line.matchAll(WORDS)) {
    if (m.index! > at) out.push(line.slice(at, m.index));
    const cls = m[1] ? "t-comment" : m[2] ? "t-string" : m[3] ? "t-key" : "t-duration";
    out.push(
      <span key={m.index} className={cls}>
        {m[0]}
      </span>,
    );
    at = m.index! + m[0].length;
  }
  out.push(line.slice(at) + "\n");
  return <span>{out}</span>;
});

const Highlighted = memo(function Highlighted({ text }: { text: string }) {
  return (
    <pre className="hl" aria-hidden="true">
      {lines(text).map((line, i) => (
        <Line key={i} line={line} />
      ))}
    </pre>
  );
});

interface GutterProps {
  first: number;
  count: number;
  /** "bad" or "warn" per document line, as a stable string key. */
  marks: string;
  bars: Map<number, number>;
  barStarts: Map<number, number>;
  onSeek: (seconds: number) => void;
}

const Gutter = memo(
  forwardRef<HTMLDivElement, GutterProps>(function Gutter({ first, count, marks, bars, barStarts, onSeek }, ref) {
    const state = new Map(marks ? marks.split(",").map((m) => [Number(m.slice(1)), m[0] === "b" ? "bad" : "warn"] as const) : []);
    const rows: JSX.Element[] = [];
    for (let n = first; n < first + count; n++) {
      const bar = bars.get(n);
      const start = barStarts.get(n);
      rows.push(
        <div key={n} className={state.has(n) ? `gl ${state.get(n)}` : "gl"}>
          {bar !== undefined && start !== undefined ? (
            <button type="button" tabIndex={-1} className="bar" title={`Play from bar ${bar}`} onClick={() => onSeek(start)}>
              {bar}
            </button>
          ) : (
            <span />
          )}
          <span className="n">{n}</span>
        </div>,
      );
    }
    return (
      <div className="gutter">
        <div ref={ref} className="gutter-inner">
          {rows}
        </div>
      </div>
    );
  }),
);

/** Bars over the lines that are sounding now; follows playback when idle. */
function NowMarks(props: { timing: TrackTiming[]; transport: TransportView; duration: number; firstLine: number; count: number; area: React.RefObject<HTMLTextAreaElement> }) {
  const position = usePosition(props.transport, props.duration);
  const playing = props.transport.playing;
  const now = useMemo(() => soundingLines(props.timing, position), [props.timing, position]);
  const shown = [...now].filter((l) => l >= props.firstLine && l < props.firstLine + props.count).sort((a, b) => a - b);
  const top = shown[0];

  useEffect(() => {
    const el = props.area.current;
    if (!playing || top === undefined || !el || document.activeElement === el) return;
    const y = PAD + (top - props.firstLine) * LINE_HEIGHT;
    if (y < el.scrollTop || y + LINE_HEIGHT > el.scrollTop + el.clientHeight) el.scrollTop = Math.max(0, y - el.clientHeight / 3);
  }, [top, playing, props.firstLine, props.area]);

  if (position <= 0 && !playing) return null;
  return (
    <>
      {shown.map((l) => (
        <div key={l} className="mark now" style={{ top: PAD + (l - props.firstLine) * LINE_HEIGHT }} />
      ))}
    </>
  );
}

export const ScoreEditor = forwardRef<EditorHandle, Props>(function ScoreEditor(props, ref) {
  const area = useRef<HTMLTextAreaElement>(null);
  const layer = useRef<HTMLDivElement>(null);
  const gutter = useRef<HTMLDivElement>(null);
  const count = lines(props.text).length;
  const bad = new Set(props.problems.filter((d) => d.severity === "error").map((d) => d.line!));
  const warn = new Set(props.problems.filter((d) => d.severity === "warning").map((d) => d.line!));

  const sync = () => {
    const el = area.current;
    if (!el) return;
    const t = `translate(${-el.scrollLeft}px, ${-el.scrollTop}px)`;
    if (layer.current) layer.current.style.transform = t;
    if (gutter.current) gutter.current.style.transform = `translateY(${-el.scrollTop}px)`;
    props.scrolls.set(props.scrollKey, el.scrollTop);
  };

  // Restore this text's scroll position when switching texts.
  useEffect(() => {
    const el = area.current;
    if (!el) return;
    el.scrollTop = props.scrolls.get(props.scrollKey) ?? 0;
    el.scrollLeft = 0;
    sync();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [props.scrollKey]);

  const caret = () => {
    const el = area.current;
    if (el) props.onCaretLine(props.firstLine + lines(el.value.slice(0, el.selectionStart)).length - 1);
  };

  useImperativeHandle(ref, () => ({
    reveal: (line, column) => {
      const el = area.current;
      if (!el) return;
      const offset = offsetOfLine(el.value, line - props.firstLine + 1) + Math.max(0, (column ?? 1) - 1);
      el.focus();
      el.setSelectionRange(offset, offset);
      el.scrollTop = Math.max(0, PAD + (line - props.firstLine) * LINE_HEIGHT - el.clientHeight / 2);
      sync();
    },
    focus: () => area.current?.focus(),
    caretLine: () => {
      const el = area.current;
      return el ? props.firstLine + lines(el.value.slice(0, el.selectionStart)).length - 1 : props.firstLine;
    },
  }));

  const marks = [...[...bad].map((l) => `b${l}`), ...[...warn].map((l) => `w${l}`)].join(",");

  return (
    <div className="editor">
      <Gutter ref={gutter} first={props.firstLine} count={count} marks={marks} bars={props.bars} barStarts={props.barStarts} onSeek={props.onSeek} />
      <div className="code">
        <div ref={layer} className="layer">
          {[...bad, ...warn].map((l) => (
            <div key={`p${l}`} className={bad.has(l) ? "mark bad" : "mark warn"} style={{ top: PAD + (l - props.firstLine) * LINE_HEIGHT }} />
          ))}
          {props.timing ? <NowMarks timing={props.timing} transport={props.transport} duration={props.duration} firstLine={props.firstLine} count={count} area={area} /> : null}
          <Highlighted text={props.text} />
        </div>
        <textarea
          ref={area}
          id="score"
          aria-label={props.label}
          value={props.text}
          spellCheck={false}
          autoCapitalize="off"
          autoComplete="off"
          wrap="off"
          onChange={(e) => {
            props.onChange(e.target.value);
            caret();
          }}
          onScroll={sync}
          onKeyUp={caret}
          onClick={caret}
        />
      </div>
    </div>
  );
});
