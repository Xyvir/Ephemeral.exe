"""Tk dialogs for the instructor build's first-run setup.

Imported only by the child process that :mod:`ephemeral_ui.setup_gui` starts,
so the tray itself never loads tkinter. Each dialog writes its answer as JSON to
a file the parent names, because the Windows build is a windowed executable with
no console to print to.

Kinds:

* ``seed``   — the create-or-join question. Answer: ``{"answer": str}``, where
  ``""`` means create a new swarm; cancelling yields ``{"cancelled": true}``.
* ``images`` — which language images to pre-hydrate, with per-image download
  sizes. Answer: ``{"images": [...], "total_bytes": int | None}``.

tkinter is part of the Python standard library, so no extra dependency is needed.
"""
from __future__ import annotations

import json
import queue
import threading
import tkinter as tk
from pathlib import Path
from tkinter import ttk

_WARN_COLOUR = "#b00020"


def fmt_bytes(n: int | None) -> str:
    if n is None:
        return "unknown"
    if n >= 2**30:
        return f"{n / 2**30:.2f} GB"
    return f"{n / 2**20:.0f} MB"


def image_languages(images: list[str]) -> dict[str, str]:
    """Human labels for each image: the languages that resolve to it."""
    from ephemeral_core.config import LANG_MAP

    langs: dict[str, list[str]] = {image: [] for image in images}
    for lang, value in LANG_MAP.items():
        if isinstance(value, dict):
            image = value.get("image")
            if image in langs and lang not in langs[image]:
                langs[image].append(lang)
    return {image: ", ".join(sorted(names)) for image, names in langs.items()}


def _probe_sizes(images: list[str], out: queue.Queue) -> None:
    """Measure each image's download size off the UI thread.

    Registry metadata only (``podman manifest inspect``), so nothing is pulled.
    """
    from ephemeral_core.space import estimate_pull_size

    for image in images:
        try:
            size = estimate_pull_size(image)
        except Exception:
            size = None
        out.put((image, size))


def seed_dialog(payload: dict) -> dict:
    result: dict = {"cancelled": True}
    root = tk.Tk()
    root.title("Ephemeral: Private swarm")
    root.minsize(560, 0)

    frame = ttk.Frame(root, padding=16)
    frame.pack(fill="both", expand=True)
    ttk.Label(frame, text=payload.get("prompt", ""), wraplength=520,
              justify="left").pack(anchor="w")

    entry = ttk.Entry(frame, width=70)
    entry.pack(fill="x", pady=(12, 0))
    ttk.Label(frame, text="Leave empty to create a new private swarm.").pack(
        anchor="w", pady=(4, 0))

    def ok(_event=None) -> None:
        result.clear()
        result["answer"] = entry.get().strip()
        root.destroy()

    def cancel(_event=None) -> None:
        result.clear()
        result["cancelled"] = True
        root.destroy()

    buttons = ttk.Frame(frame)
    buttons.pack(fill="x", pady=(16, 0))
    ttk.Button(buttons, text="Cancel", command=cancel).pack(side="right")
    ttk.Button(buttons, text="OK", command=ok).pack(side="right", padx=(0, 8))

    root.bind("<Return>", ok)
    root.bind("<Escape>", cancel)
    root.protocol("WM_DELETE_WINDOW", cancel)
    entry.focus_set()
    root.mainloop()
    return result


def images_dialog(payload: dict) -> dict:
    images: list[str] = list(payload.get("images", []))
    free = payload.get("free_bytes") or 0
    drive = payload.get("drive") or "the storage drive"
    cap_gb = payload.get("cap_gb")
    langs = image_languages(images)

    chosen_vars: dict[str, tk.BooleanVar] = {}
    size_labels: dict[str, ttk.Label] = {}
    sizes: dict[str, int | None] = {}
    result: dict = {"images": [], "total_bytes": None}
    closed = False

    root = tk.Tk()
    root.title("Ephemeral: Pre-hydrate images")
    root.geometry("800x560")

    outer = ttk.Frame(root, padding=12)
    outer.pack(fill="both", expand=True)
    ttk.Label(
        outer,
        text=(
            "Choose the language images your students will use. Checked images are "
            "downloaded now, so their first job starts at once; unchecked ones "
            "download the first time a job needs them."
        ),
        wraplength=760,
        justify="left",
    ).pack(anchor="w")

    # Scrollable list: one checkbox row per image.
    box = ttk.Frame(outer)
    box.pack(fill="both", expand=True, pady=8)
    canvas = tk.Canvas(box, highlightthickness=0)
    scroll = ttk.Scrollbar(box, orient="vertical", command=canvas.yview)
    inner = ttk.Frame(canvas)
    inner.bind(
        "<Configure>",
        lambda _e: canvas.configure(scrollregion=canvas.bbox("all")),
    )
    canvas.create_window((0, 0), window=inner, anchor="nw")
    canvas.configure(yscrollcommand=scroll.set)
    canvas.pack(side="left", fill="both", expand=True)
    scroll.pack(side="right", fill="y")

    def _wheel(event) -> None:
        canvas.yview_scroll(int(-event.delta / 120), "units")

    canvas.bind("<Enter>", lambda _e: canvas.bind_all("<MouseWheel>", _wheel))
    canvas.bind("<Leave>", lambda _e: canvas.unbind_all("<MouseWheel>"))

    ttk.Label(inner, text="Image", font=("TkDefaultFont", 9, "bold")).grid(
        row=0, column=0, sticky="w", padx=4)
    ttk.Label(inner, text="Languages", font=("TkDefaultFont", 9, "bold")).grid(
        row=0, column=1, sticky="w", padx=8)
    ttk.Label(inner, text="Download", font=("TkDefaultFont", 9, "bold")).grid(
        row=0, column=2, sticky="e", padx=4)

    for row, image in enumerate(images, start=1):
        var = tk.BooleanVar(value=True)
        chosen_vars[image] = var
        ttk.Checkbutton(inner, text=image, variable=var,
                        command=lambda: refresh()).grid(
            row=row, column=0, sticky="w", padx=4, pady=2)
        ttk.Label(inner, text=langs.get(image, "")).grid(
            row=row, column=1, sticky="w", padx=8)
        size_labels[image] = ttk.Label(inner, text="measuring…")
        size_labels[image].grid(row=row, column=2, sticky="e", padx=4)

    def checked_images() -> list[str]:
        return [i for i in images if chosen_vars[i].get()]

    summary = ttk.Label(outer, wraplength=760, justify="left")
    summary.pack(anchor="w")
    warning = ttk.Label(outer, wraplength=760, justify="left",
                        foreground=_WARN_COLOUR)
    warning.pack(anchor="w", pady=(4, 0))

    def refresh() -> None:
        chosen = checked_images()
        known = [sizes[i] for i in chosen if sizes.get(i) is not None]
        unknown = sum(1 for i in chosen if sizes.get(i) is None and i in sizes)
        pending = sum(1 for i in chosen if i not in sizes)
        total = sum(known) if known else None
        text = (
            f"Selected: {len(chosen)} of {len(images)} images, "
            f"about {fmt_bytes(total)} to download"
        )
        if unknown or pending:
            text += f" (+{unknown + pending} not yet measured)"
        text += f".  Free on {drive}: {fmt_bytes(free) if free else 'unknown'}."
        summary.configure(text=text)

        warnings = []
        if total and free and total * 2 > free:
            warnings.append(
                "This may not fit in the free space — pulls could fail. "
                "Uncheck the large images or free up disk."
            )
        if total and cap_gb and total > float(cap_gb) * 2**30:
            warnings.append(
                f"This is more than the Podman VM disk cap ({cap_gb:.0f} GB)."
            )
        warning.configure(text=" ".join(warnings))

    def set_all(value: bool) -> None:
        for var in chosen_vars.values():
            var.set(value)
        refresh()

    def close() -> None:
        nonlocal closed
        closed = True
        root.destroy()

    def pull() -> None:
        chosen = checked_images()
        known = [sizes[i] for i in chosen if sizes.get(i) is not None]
        result.update(images=chosen, total_bytes=sum(known) if known else None)
        close()

    def skip() -> None:
        result.update(images=[], total_bytes=None)
        close()

    buttons = ttk.Frame(outer)
    buttons.pack(fill="x", pady=(12, 0))
    ttk.Button(buttons, text="Select all", command=lambda: set_all(True)).pack(
        side="left")
    ttk.Button(buttons, text="Select none", command=lambda: set_all(False)).pack(
        side="left", padx=(8, 0))
    ttk.Button(buttons, text="Skip for now", command=skip).pack(side="right")
    pull_button = ttk.Button(buttons, text="Pull selected", command=pull)
    pull_button.pack(side="right", padx=(0, 8))

    root.bind("<Return>", lambda _e: pull())
    root.bind("<Escape>", lambda _e: skip())
    root.protocol("WM_DELETE_WINDOW", skip)

    probe_queue: queue.Queue = queue.Queue()
    threading.Thread(
        target=_probe_sizes, args=(images, probe_queue), daemon=True
    ).start()

    def poll() -> None:
        if closed:
            return
        try:
            while True:
                image, size = probe_queue.get_nowait()
                sizes[image] = int(size) if isinstance(size, (int, float)) else None
                size_labels[image].configure(text=fmt_bytes(sizes[image]))
                refresh()
        except queue.Empty:
            pass
        root.after(200, poll)

    refresh()
    root.after(200, poll)
    pull_button.focus_set()
    root.mainloop()
    return result


DIALOGS = {"seed": seed_dialog, "images": images_dialog}


def run(kind: str, in_path: str, out_path: str) -> int:
    """Show one dialog and write its answer to ``out_path``. Returns an exit code."""
    dialog = DIALOGS.get(kind)
    if dialog is None:
        return 64
    payload = json.loads(Path(in_path).read_text(encoding="utf-8"))
    result = dialog(payload)
    Path(out_path).write_text(json.dumps(result), encoding="utf-8")
    return 0
