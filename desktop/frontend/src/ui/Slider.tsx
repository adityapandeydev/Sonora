import { useEffect, useId, useRef, useState, type KeyboardEvent, type ReactNode } from "react";
import { fromPosition, nudge, toPosition, type Nudge, type Range } from "./range";

interface Props {
  label: string;
  /** Shown instead of the label text; the label still names the control. */
  icon?: ReactNode;
  value: number;
  range: Range;
  /** Double-clicking returns to this value. */
  defaultValue: number;
  /** The fill starts here: 0 for a centred control such as pan. */
  origin?: number;
  format: (value: number) => string;
  disabled?: boolean;
  /** Called with every new value, including during a drag. */
  onInput: (value: number) => void;
  /** Called once when a drag or key press is finished. */
  onCommit?: (value: number) => void;
}

const KEYS: Record<string, Nudge> = {
  ArrowRight: "up",
  ArrowUp: "up",
  ArrowLeft: "down",
  ArrowDown: "down",
  PageUp: "pageUp",
  PageDown: "pageDown",
  Home: "min",
  End: "max",
};

export function Slider(props: Props) {
  const { label, icon, value, range, defaultValue, origin = range.min, format, disabled } = props;
  const id = useId();
  const [held, setHeld] = useState<number | null>(null);
  const dragging = useRef(false);
  const latest = useRef(value);

  // Show the user's value until the owner reports it back; give up waiting
  // after a moment in case the owner refused it.
  const shown = held ?? value;
  useEffect(() => {
    latest.current = shown;
  }, [shown]);
  useEffect(() => {
    if (held === null) return;
    const settle = () => {
      if (!dragging.current) setHeld(null);
    };
    if (value === held) settle();
    const timer = window.setTimeout(settle, 1000);
    return () => window.clearTimeout(timer);
  }, [value, held]);

  const change = (next: number) => {
    latest.current = next;
    setHeld(next);
    props.onInput(next);
  };
  const commit = () => props.onCommit?.(latest.current);

  const release = () => {
    if (!dragging.current) return;
    dragging.current = false;
    if (latest.current === value) setHeld(null);
    else window.setTimeout(() => !dragging.current && setHeld(null), 1000);
    commit();
  };

  const onKeyDown = (e: KeyboardEvent) => {
    const how = KEYS[e.key];
    if (!how || e.altKey || e.ctrlKey || e.metaKey) return;
    e.preventDefault();
    const next = nudge(latest.current, how, range);
    if (next === latest.current) return;
    change(next);
    commit();
  };

  const p = toPosition(shown, range);
  const o = toPosition(origin, range);
  const style = { "--from": Math.min(p, o), "--to": Math.max(p, o) } as React.CSSProperties;

  return (
    <div className={icon ? "slider bare" : "slider"}>
      <label className="name" htmlFor={id} title={icon ? undefined : label}>
        {icon ?? label}
      </label>
      <input
        id={id}
        type="range"
        min={0}
        max={1}
        step="any"
        value={p}
        style={style}
        disabled={disabled}
        aria-label={label}
        aria-valuetext={format(shown)}
        onPointerDown={() => {
          dragging.current = true;
        }}
        onPointerUp={release}
        onPointerCancel={release}
        onLostPointerCapture={release}
        onChange={(e) => change(fromPosition(Number(e.target.value), range))}
        onKeyDown={onKeyDown}
        onDoubleClick={() => {
          change(defaultValue);
          commit();
        }}
      />
      {icon ? null : <output htmlFor={id}>{format(shown)}</output>}
    </div>
  );
}
