// Generate a new score from settings, and bring back kept versions. Both
// replace the score text; the engine keeps the text being replaced.
import { useEffect, useId, useRef, useState, type FormEvent, type ReactNode } from "react";
import { EngineError } from "./engine";
import { engineProblems, initialSettings, LIMITS, lengthOf, localProblems, randomSeed, scaleLabel, type Field } from "./generate";
import type { GenSettings, ScoreVersion } from "./protocol";

const minutes = (s: number) => `${Math.floor(s / 60)}:${String(Math.round(s % 60)).padStart(2, "0")}`;

/** A native modal dialog: focus is trapped, Escape closes, focus returns on close. */
function Modal({ open, title, onClose, children, className }: { open: boolean; title: string; onClose: () => void; children: ReactNode; className?: string }) {
  const ref = useRef<HTMLDialogElement>(null);
  const titleId = useId();
  useEffect(() => {
    const el = ref.current;
    if (!el) return;
    if (open && !el.open) el.showModal();
    if (!open && el.open) el.close();
  }, [open]);
  return (
    <dialog
      ref={ref}
      className={`modal ${className ?? ""}`}
      aria-labelledby={titleId}
      onClose={onClose}
      onCancel={(e) => {
        e.preventDefault();
        onClose();
      }}
      onMouseDown={(e) => {
        // A press on the backdrop (the dialog element itself) closes it.
        if (e.target === e.currentTarget) onClose();
      }}
    >
      {open ? (
        <div className="modal-body">
          <h2 id={titleId}>{title}</h2>
          {children}
        </div>
      ) : null}
    </dialog>
  );
}

interface GenerateProps {
  open: boolean;
  roots: string[];
  scales: string[];
  versions: ScoreVersion[];
  onClose: () => void;
  onGenerate: (settings: GenSettings) => Promise<void>;
}

type Draft = Record<Field, string>;

const toDraft = (s: GenSettings): Draft => ({ seed: String(s.seed), root: s.root, scale: s.scale, bpm: String(s.bpm), bars: String(s.bars) });
const fromDraft = (d: Draft): GenSettings => ({
  seed: d.seed.trim() === "" ? NaN : Number(d.seed),
  root: d.root,
  scale: d.scale,
  bpm: d.bpm.trim() === "" ? NaN : Number(d.bpm),
  bars: d.bars.trim() === "" ? NaN : Number(d.bars),
});

export function GenerateDialog(props: GenerateProps) {
  const [draft, setDraft] = useState<Draft>(() => toDraft(initialSettings(props.versions)));
  const [problems, setProblems] = useState<Partial<Record<Field, string>>>({});
  const [failure, setFailure] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const ids = { root: useId(), scale: useId(), bpm: useId(), bars: useId(), seed: useId() };

  // Each opening starts from the newest generated settings.
  useEffect(() => {
    if (!props.open) return;
    setDraft(toDraft(initialSettings(props.versions)));
    setProblems({});
    setFailure(null);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [props.open]);

  const set = (field: Field, value: string) => {
    setDraft((d) => ({ ...d, [field]: value }));
    setProblems((p) => ({ ...p, [field]: undefined }));
  };

  const settings = fromDraft(draft);
  const seconds = Number.isFinite(settings.bpm) && Number.isFinite(settings.bars) && settings.bpm > 0 ? lengthOf(settings) : null;

  const submit = async (e: FormEvent) => {
    e.preventDefault();
    const local = localProblems(settings);
    setProblems(local);
    setFailure(null);
    if (Object.keys(local).length > 0) {
      document.getElementById(ids[Object.keys(local)[0] as Field])?.focus();
      return;
    }
    setBusy(true);
    try {
      await props.onGenerate(settings);
      props.onClose();
    } catch (err) {
      const fields = err instanceof EngineError ? engineProblems(err.data) : {};
      if (Object.keys(fields).length > 0) setProblems(fields);
      else setFailure(err instanceof Error ? err.message : String(err));
    } finally {
      setBusy(false);
    }
  };

  const hint = (field: Field) =>
    problems[field] ? (
      <span className="field-problem" id={`${ids[field]}-p`}>
        {problems[field]}
      </span>
    ) : null;
  const described = (field: Field) => (problems[field] ? { "aria-invalid": true, "aria-describedby": `${ids[field]}-p` } : {});

  return (
    <Modal open={props.open} title="Generate a song" onClose={busy ? () => {} : props.onClose}>
      <form className="gen-form" onSubmit={submit} noValidate>
        <div className="field-row">
          <label className="field" htmlFor={ids.root}>
            <span>Key</span>
            <select id={ids.root} value={draft.root} onChange={(e) => set("root", e.target.value)} {...described("root")}>
              {props.roots.map((r) => (
                <option key={r}>{r}</option>
              ))}
            </select>
            {hint("root")}
          </label>
          <label className="field grow" htmlFor={ids.scale}>
            <span>Scale</span>
            <select id={ids.scale} value={draft.scale} onChange={(e) => set("scale", e.target.value)} {...described("scale")}>
              {props.scales.map((s) => (
                <option key={s} value={s}>
                  {scaleLabel(s)}
                </option>
              ))}
            </select>
            {hint("scale")}
          </label>
        </div>
        <div className="field-row">
          <label className="field" htmlFor={ids.bpm}>
            <span>Tempo, bpm</span>
            <input id={ids.bpm} type="number" inputMode="decimal" min={LIMITS.bpm.min} max={LIMITS.bpm.max} value={draft.bpm} onChange={(e) => set("bpm", e.target.value)} {...described("bpm")} />
            {hint("bpm")}
          </label>
          <label className="field" htmlFor={ids.bars}>
            <span>Length, bars</span>
            <input id={ids.bars} type="number" inputMode="numeric" min={LIMITS.bars.min} max={LIMITS.bars.max} value={draft.bars} onChange={(e) => set("bars", e.target.value)} {...described("bars")} />
            {hint("bars")}
          </label>
          <p className="field-note" aria-live="polite">
            {seconds !== null && seconds > 0 ? `About ${minutes(seconds)}` : ""}
          </p>
        </div>
        <div className="field-row">
          <label className="field grow" htmlFor={ids.seed}>
            <span>Seed</span>
            <input id={ids.seed} type="number" inputMode="numeric" min={LIMITS.seed.min} max={LIMITS.seed.max} value={draft.seed} onChange={(e) => set("seed", e.target.value)} {...described("seed")} />
            {hint("seed")}
          </label>
          <button type="button" className="button" onClick={() => set("seed", String(randomSeed()))}>
            New seed
          </button>
        </div>
        <p className="sub">The same settings always give the same song. Generating replaces the score; the current score is kept in Versions.</p>
        {failure ? (
          <p className="field-problem" role="alert">
            {failure}
          </p>
        ) : null}
        <div className="modal-actions">
          <button type="button" className="button" onClick={props.onClose} disabled={busy}>
            Cancel
          </button>
          <button type="submit" className="button accent" disabled={busy}>
            {busy ? "Generating" : "Generate"}
          </button>
        </div>
      </form>
    </Modal>
  );
}

interface VersionsProps {
  open: boolean;
  versions: ScoreVersion[];
  onClose: () => void;
  onRestore: (id: number) => Promise<void>;
}

export function VersionsDialog({ open, versions, onClose, onRestore }: VersionsProps) {
  const [busy, setBusy] = useState<number | null>(null);
  const [failure, setFailure] = useState<string | null>(null);
  useEffect(() => {
    if (open) setFailure(null);
  }, [open]);

  const restore = async (id: number) => {
    setBusy(id);
    setFailure(null);
    try {
      await onRestore(id);
      onClose();
    } catch (err) {
      setFailure(err instanceof Error ? err.message : String(err));
    } finally {
      setBusy(null);
    }
  };

  return (
    <Modal open={open} title="Versions" onClose={onClose} className="wide">
      {versions.length === 0 ? (
        <p className="sub">Scores replaced by generating or restoring are kept here.</p>
      ) : (
        <ol className="versions">
          {versions.map((v) => (
            <li key={v.id}>
              <div>
                <b>{v.label}</b>
                <span className="sub">
                  Revision {v.revision}, {v.lines} line{v.lines === 1 ? "" : "s"}
                </span>
              </div>
              {v.current ? (
                <span className="tag">Current</span>
              ) : (
                <button type="button" className="button" disabled={busy !== null} onClick={() => restore(v.id)}>
                  {busy === v.id ? "Restoring" : "Restore"}
                </button>
              )}
            </li>
          ))}
        </ol>
      )}
      <p className="sub">Restoring keeps the score it replaces. The newest 30 versions are kept while Sonora is open.</p>
      {failure ? (
        <p className="field-problem" role="alert">
          {failure}
        </p>
      ) : null}
      <div className="modal-actions">
        <button type="button" className="button" onClick={onClose}>
          Close
        </button>
      </div>
    </Modal>
  );
}
