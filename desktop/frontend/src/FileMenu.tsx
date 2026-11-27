// Project files: new, open, save, save as, and score import and export. The
// engine reads and writes the files; this picks them and makes sure unsaved
// work is never replaced without asking.
import { Dialogs } from "@wailsio/runtime";
import { useCallback, useEffect, useRef, useState, type KeyboardEvent } from "react";
import { EngineError } from "./engine";
import { baseName, fileNameFor, withExtension } from "./mix";
import type { ProjectView } from "./protocol";
import { Modal } from "./ui/Modal";
import type { useProject } from "./useProject";

const PROJECT = ".sonoraproj";
const SCORE = ".sonora";
const projectFilter = [{ DisplayName: "Sonora project", Pattern: `*${PROJECT}` }];
const scoreFilter = [{ DisplayName: "Sonora score", Pattern: `*${SCORE}` }];

/** The engine's own words, including which setting was wrong. */
export function describeFailure(e: unknown): string {
  if (e instanceof EngineError) {
    const params = (e.data as { params?: { message: string }[] } | undefined)?.params;
    return params?.length ? params.map((p) => p.message).join(". ") : e.message;
  }
  return e instanceof Error ? e.message : String(e);
}

const pick = async (f: () => Promise<string | string[]>): Promise<string> => {
  try {
    const r = await f();
    return Array.isArray(r) ? (r[0] ?? "") : r;
  } catch {
    return "";
  }
};

type Session = ReturnType<typeof useProject>;

export function useFiles(session: Session, project: ProjectView | null) {
  const [asking, setAsking] = useState<{ action: string; then: () => Promise<unknown> } | null>(null);
  const [problem, setProblem] = useState<string | null>(null);
  const files = session.files;
  const fail = (e: unknown) => setProblem(describeFailure(e));
  const suggested = (ext: string) => (project?.file.name ? project.file.name.replace(/\.[^.]*$/, "") + ext : fileNameFor(project?.song?.title ?? "Untitled", ext));

  const saveAs = async (): Promise<boolean> => {
    const path = await pick(() => Dialogs.SaveFile({ Title: "Save project", Filename: suggested(PROJECT), Filters: projectFilter, CanCreateDirectories: true, ButtonText: "Save" }));
    if (!path) return false;
    try {
      await files.save(withExtension(path, PROJECT));
      return true;
    } catch (e) {
      fail(e);
      return false;
    }
  };

  const save = async (): Promise<boolean> => {
    if (!project?.file.path) return saveAs();
    try {
      await files.save();
      return true;
    } catch (e) {
      fail(e);
      return false;
    }
  };

  // Ask first when the open project has unsaved changes.
  const guarded = (action: string, then: () => Promise<unknown>) => {
    if (project?.file.modified) setAsking({ action, then });
    else then().catch(fail);
  };

  const open = async () => {
    const path = await pick(() => Dialogs.OpenFile({ Title: "Open project", Filters: projectFilter, CanChooseFiles: true }));
    if (path) guarded("opening another project", () => files.open(path));
  };

  const create = () => guarded("starting a new project", () => files.create());

  const importScore = async () => {
    const path = await pick(() => Dialogs.OpenFile({ Title: "Import score", Filters: scoreFilter, CanChooseFiles: true }));
    if (path) files.importScore(path).catch(fail);
  };

  const exportScore = async () => {
    const path = await pick(() => Dialogs.SaveFile({ Title: "Export score", Filename: suggested(SCORE), Filters: scoreFilter, CanCreateDirectories: true, ButtonText: "Export" }));
    if (path) files.exportScore(withExtension(path, SCORE)).catch(fail);
  };

  const answer = async (choice: "save" | "discard" | "cancel") => {
    const pending = asking;
    setAsking(null);
    if (!pending || choice === "cancel") return;
    if (choice === "save" && !(await save())) return;
    pending.then().catch(fail);
  };

  // Each action starts with a clean slate.
  const fresh = <A extends unknown[]>(f: (...a: A) => unknown) => (...a: A) => {
    setProblem(null);
    return f(...a);
  };

  return {
    save: fresh(save),
    saveAs: fresh(saveAs),
    open: fresh(open),
    create: fresh(create),
    importScore: fresh(importScore),
    exportScore: fresh(exportScore),
    asking,
    answer,
    problem,
    clearProblem: () => setProblem(null),
  };
}

type Files = ReturnType<typeof useFiles>;

const ITEMS: { label: string; keys?: string; run: (f: Files) => unknown; divider?: boolean }[] = [
  { label: "New", keys: "Ctrl+N", run: (f) => f.create() },
  { label: "Open…", keys: "Ctrl+O", run: (f) => f.open() },
  { label: "Save", keys: "Ctrl+S", run: (f) => f.save() },
  { label: "Save as…", keys: "Ctrl+Shift+S", run: (f) => f.saveAs() },
  { label: "Import score…", run: (f) => f.importScore(), divider: true },
  { label: "Export score…", run: (f) => f.exportScore() },
];

export function FileMenu({ files }: { files: Files }) {
  const [open, setOpen] = useState(false);
  const button = useRef<HTMLButtonElement>(null);
  const menu = useRef<HTMLUListElement>(null);

  const close = useCallback((refocus: boolean) => {
    setOpen(false);
    if (refocus) button.current?.focus();
  }, []);

  useEffect(() => {
    if (!open) return;
    menu.current?.querySelector<HTMLButtonElement>("button")?.focus();
    const outside = (e: PointerEvent) => {
      if (!menu.current?.contains(e.target as Node) && e.target !== button.current) close(false);
    };
    window.addEventListener("pointerdown", outside);
    return () => window.removeEventListener("pointerdown", outside);
  }, [open, close]);

  const keys = (e: KeyboardEvent) => {
    const items = [...(menu.current?.querySelectorAll<HTMLButtonElement>("button") ?? [])];
    const i = items.indexOf(document.activeElement as HTMLButtonElement);
    const to = { ArrowDown: (i + 1) % items.length, ArrowUp: (i - 1 + items.length) % items.length, Home: 0, End: items.length - 1 }[e.key];
    if (to !== undefined) {
      e.preventDefault();
      items[to]?.focus();
    } else if (e.key === "Escape" || e.key === "Tab") {
      e.preventDefault();
      close(true);
    }
  };

  return (
    <div className="menu-wrap">
      <button ref={button} type="button" className="button on-shell" aria-haspopup="menu" aria-expanded={open} onClick={() => setOpen((o) => !o)}>
        File
      </button>
      {open ? (
        <ul ref={menu} className="menu" role="menu" aria-label="File" onKeyDown={keys}>
          {ITEMS.map((item) => (
            <li key={item.label} role="none" className={item.divider ? "divider" : undefined}>
              <button
                type="button"
                role="menuitem"
                tabIndex={-1}
                onClick={() => {
                  close(true);
                  item.run(files);
                }}
              >
                <span>{item.label}</span>
                {item.keys ? <kbd>{item.keys}</kbd> : null}
              </button>
            </li>
          ))}
        </ul>
      ) : null}
    </div>
  );
}

export function UnsavedDialog({ files, project }: { files: Files; project: ProjectView }) {
  const name = project.file.name ?? "this project";
  return (
    <Modal open={files.asking !== null} title="Save changes first?" onClose={() => files.answer("cancel")}>
      <p className="sub">
        {name === "this project" ? "This project has" : <><b>{baseName(name)}</b> has</>} changes that are not saved. Save them before {files.asking?.action ?? "continuing"}?
      </p>
      <div className="modal-actions">
        <button type="button" className="button" onClick={() => files.answer("cancel")}>
          Cancel
        </button>
        <button type="button" className="button" onClick={() => files.answer("discard")}>
          Don't save
        </button>
        <button type="button" className="button accent" onClick={() => files.answer("save")}>
          Save
        </button>
      </div>
    </Modal>
  );
}
