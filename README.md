# Ephemeral

**Ephemeral** is a lightweight, daemonless utility for Windows that instantly executes code snippets from your clipboard inside isolated, secure containers.

![Ephemeral Demo](ephemeral.gif)

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

## Prerequisites

Before running Ephemeral, you must ensure your Windows environment is ready to host Linux containers.

1.  **Enable Virtualization (BIOS/UEFI):**
    * Ensure **Virtualization Technology** (often labeled VT-x, AMD-V, or SVM) is enabled in your computer's BIOS/UEFI settings. This is strictly required for WSL2 to function.

2.  **Enable WSL2:**
    * Open PowerShell as Administrator and run: `wsl --install`
    * Restart your computer if prompted.

3.  **Install Podman for Windows:**
    * Download the installer from the [Podman Website](https://podman.io/docs/installation#windows).
    * Run the installer.
    * Open a terminal and initialize the machine:
        ```powershell
        podman machine init
        podman machine start
        ```
    * *Note: Ephemeral will attempt to auto-start the machine if it's stopped, but the initial `init` setup usually requires manual intervention.*

## Usage

1.  **Highlight & Copy** any code block (or click the "Copy Code" button found on many documentation sites):
2.  **Press** `Ctrl+Alt+X` (or use the Tray Icon menu).
3.  **Wait** for the notification (or the status window if a download is required).
4.  **Paste** the result wherever you need it.

*Note: Ephemeral supports Markdown blocks with language tags, Shebang lines (`#!/bin/python`), and prompts for user-input if no language specified.*

A test suite file is provided in the repo to demonstration the usage of the various supported langauges.

## Features

### Manual Language Entry & History
If you copy raw text without a language tag (e.g., no ` ```python `), Ephemeral will pop up a terminal window asking you to specify the language. 
* It remembers your last used language for rapid iteration.
* Simply press **Enter** to use the default/last-used language.

### Legacy Versioning
You can override the default "Latest" version by appending a tag to the language name in your markdown block or shebang:

* `python:2.7` -> Runs in `python:2.7` container.
* `node:14` -> Runs in `node:14-alpine`.
* `ruby-2.6` -> Runs in `ruby:2.6`.

If no version is specified, it defaults to the stable/slim versions defined in the tool.

### Cache Management
Ephemeral downloads container images as needed. Over time, these can take up disk space.
* **Right-click** the tray icon and select **"Clear Image Cache"**.
* This runs a safe prune command (`podman image prune -a`) to delete all images not currently in use, freeing up space on your drive.

## Supported Languages

### Standard & Scripting
* **Python** (`python`, `py`)
* **Node.js** (`node`, `js`)
* **Ruby** (`ruby`)
* **Bash** (`bash`, `sh`)
* **Lua** (`lua`)
* **Perl** (`perl`)
* **PHP** (`php`)

### Systems & Compiled
* **C** (`c`, `cc`) -> Compiled via GCC
* **C++** (`cpp`, `c++`) -> Compiled via G++
* **Rust** (`rust`) -> Compiled via RustC
* **Go** (`go`, `golang`)
* **Fortran** (`fortran`, `f90`, `f95`) -> Compiled via GFortran
* **Java** (`java`) -> Single-file source execution (Java 11+)

### Modern & Golfing
* **Crystal** (`cr`, `crystal`) -> Ruby-like syntax, C-like speed
* **Nim** (`nim`, `nimrod`) -> Python-like syntax, compiled

### Science, Data & Engineering
* **Science Python** (`science`, `numpy`, `pandas`) -> Uses Anaconda3 image
* **R** (`r`, `R`) -> Base R environment
* **Julia** (`julia`)
* **Octave** (`octave`, `matlab`) -> Open-source Matlab alternative
* **Verilog** (`verilog`) -> Compiled via Icarus Verilog

### Functional & Lisp
* **Haskell** (`haskell`)
* **Common Lisp** (`lisp`, `sbcl`, `cl`) -> SBCL (Steel Bank Common Lisp)
* **Clojure** (`clojure`, `clj`)
* **Elixir** (`elixir`, `ex`, `exs`)
* **OCaml** (`ocaml`, `ml`)

### Logic
* **Prolog** (`prolog`, `swipl`, `pl`) -> SWI-Prolog

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

> If you would like additional default languages included beside the above, please open a PR so I can get them added; prefferably recommended alongside specific lightweight container to run said language.


# Todo:

Currently text-only output is supported; though I can see a use case for adding a mechanism to retreive arbitrary file artifacts and that would be the next major addition.  
  
Add support for serializing multiple varied language codeblocks in the clipboard, running each in sequence and returning their ordered STOUD.  

Add support for 'seed files' (json, csv, yaml, md etc) ; plaintext files added to the container before running the codeblock with the format:

````
```filename.txt
foo
bar
```
````
