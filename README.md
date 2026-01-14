# Ephemeral

**Ephemeral** is a lightweight, daemonless utility for Windows that instantly executes code snippets from your clipboard inside isolated, secure containers.

## The Problem
Windows is a fantastic OS, but it lacks the native "polyglot" flexibility of Linux. 
* Installing Python, Ruby, Node, Go, Rust, and Perl just to run a quick snippet is overkill.
* Managing multiple versions (Python 2.7 vs 3.10) is a nightmare of environment paths.
* Copying code from a textbook, StackOverflow, or your PKMS (Obsidian/Logseq) usually involves opening a heavy IDE, creating a file, saving it, and running it.

## The Ephemeral Solution
Ephemeral acts as a "Sidecar Notebook" for your entire operating system. It leverages **Podman** (via WSL2) to create instant, disposable execution environments.

### Why you want this:
1.  **Language Versatility:** Run Bash, Python, Ruby, R, Julia, Octave, C++, Rust, and more without installing them locally.
2.  **Clean System:** No more `npm_modules` or stray `.py` files cluttering your desktop. The container lives for milliseconds and vanishes.
3.  **Security:** Snippets run in a sandbox (`--network none`). A malicious `rm -rf /` only deletes a temporary container, not your hard drive.
4.  **Legacy Support:** Need to test a script in Python 2.7? Just type `python:2.7`. Ephemeral pulls the specific version for that run.
5.  **Context Agnostic:** It works anywhere you can copy text.

## Usage

1.  **Highlight & Copy** any code block (or click the "Copy Code" button found on many documentation sites):
2.  **Press** `Ctrl+Alt+X` (or use the Tray Icon menu).
3.  **Wait** for the notification (or the status window if a download is required).
4.  **Paste** the result wherever you need it.

*Note: Ephemeral supports Markdown blocks (with or without language tags), Shebang lines (`#!/bin/python`), and uses intelligent syntax detection as a fallback.*

## Legacy Versioning
You can override the default "Latest" version by appending a tag to the language name in your markdown block or shebang:

* `python:2.7` -> Runs in `python:2.7` container.
* `node:14` -> Runs in `node:14-alpine`.
* `ruby-2.6` -> Runs in `ruby:2.6`.

If no version is specified, it defaults to the stable/slim versions defined in the tool.

## Supported Languages

### Standard & Scripting
* **Python** (`python`, `py`)
* **Node.js** (`node`, `js`)
* **Ruby** (`ruby`)
* **Bash** (`bash`, `sh`)
* **Lua** (`lua`)
* **Perl** (`perl`)
* **PHP** (`php`)
* **Haskell** (`haskell`)

### Systems & Compiled
* **C** (`c`, `cc`) -> Compiled via GCC
* **C++** (`cpp`, `c++`) -> Compiled via G++
* **Rust** (`rust`) -> Compiled via RustC
* **Go** (`go`, `golang`)
* **Fortran** (`fortran`, `f90`, `f95`) -> Compiled via GFortran

### Science, Data & Engineering
* **Science Python** (`science`, `numpy`, `pandas`) -> Uses Anaconda3 image
* **R** (`r`, `R`) -> Base R environment
* **Julia** (`julia`)
* **Octave** (`octave`, `matlab`) -> Open-source Matlab alternative
* **Verilog** (`verilog`) -> Compiled via Icarus Verilog

### Shells
* **PowerShell** (`pwsh`, `powershell`, `cmd`, `batch`) -> Official PowerShell Core on Linux

## Building from Source

1.  Install Python 3.10+.
2.  Install dependencies:
    ```bash
    pip install -r requirements.txt
    ```
3.  Run the script:
    ```bash
    python ephemeral.py
    ```

## Building the EXE (Manual)

To create a standalone executable that doesn't require Python installed:

```bash
pip install pyinstaller
pyinstaller --noconsole --onefile --name "Ephemeral" --hidden-import=pystray ephemeral.py
```
