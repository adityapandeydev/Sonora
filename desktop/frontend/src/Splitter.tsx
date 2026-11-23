import { useRef, type KeyboardEvent, type PointerEvent } from "react";
import { clampSize } from "./score";

interface Props {
  label: string;
  size: number;
  min: number;
  max: number;
  /** +1 when dragging right grows the panel, −1 when it shrinks it. */
  direction: 1 | -1;
  onSize: (size: number) => void;
}

/** A draggable, keyboard-adjustable border between two panels. */
export function Splitter({ label, size, min, max, direction, onSize }: Props) {
  const from = useRef<{ x: number; size: number } | null>(null);

  const down = (e: PointerEvent<HTMLDivElement>) => {
    if (e.button !== 0) return;
    from.current = { x: e.clientX, size };
    e.currentTarget.setPointerCapture(e.pointerId);
  };
  const move = (e: PointerEvent) => {
    if (from.current) onSize(clampSize(from.current.size + direction * (e.clientX - from.current.x), min, max));
  };
  const up = () => {
    from.current = null;
  };
  const key = (e: KeyboardEvent) => {
    const step = e.shiftKey ? 64 : 16;
    const next = { ArrowLeft: size - direction * step, ArrowRight: size + direction * step, Home: min, End: max }[e.key];
    if (next === undefined) return;
    e.preventDefault();
    onSize(clampSize(next, min, max));
  };

  return (
    <div
      className="splitter"
      role="separator"
      aria-orientation="vertical"
      aria-label={label}
      aria-valuenow={size}
      aria-valuemin={min}
      aria-valuemax={max}
      tabIndex={0}
      onPointerDown={down}
      onPointerMove={move}
      onPointerUp={up}
      onPointerCancel={up}
      onLostPointerCapture={up}
      onKeyDown={key}
    />
  );
}
