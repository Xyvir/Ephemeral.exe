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
        ```

    * *Note: Ephemeral will attempt to auto-start the machine if it's stopped, but the initial `init` setup usually requires manual intervention.*

## Usage


1.  **Highlight & Copy** any code block (or click the "Copy Code" button found on many documentation sites):
2.  **Press** `Ctrl+Alt+X` (or left click the Tray Icon, or right click and use the menu).
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

05ab1e, Bash, Brainfuck, C, C++, CJam, Clojure, Common Lisp, Crystal, Elixir, Fortran, FreeBASIC, 
Go, GolfScript, Haskell, Java, Julia, Lolcode, Lua, Nim, Node.js, OCaml, Octave, Perl, PHP, Piet, 
PowerShell, Prolog, Python, R, Ruby, Rust, Science Python, Unlambda, Verilog.
## Declarative Image Mode
You are not limited to the built-in languages. You can run *any* Docker/Podman image by defining the `image` and `cmd` parameters directly in the markdown header.

**Example: Running FreeBASIC via a custom declarative header:**

````text
```my_custom_test image=primeimages/freebasic cmd="bash -c 'cat > /tmp/run.bas && fbc /tmp/run.bas && /tmp/run'"
Print "This code is running inside a declarative container definition!"
Print "Ephemeral downloaded the image and ran the build chain automatically."
```
````

> If you would like to have a built-in language added to the language map please open a pull request, preferrably containing a declarative example as above for me to test.


## Todo
* **File Artifacts:** Add mechanism to retrieve arbitrary files (images, binaries) generated inside the container.
* **Multi-Block Execution:** Support for running multiple sequential codeblocks found in a single clipboard copy.
* **Seed Files:** Support for injecting data files (json, csv, etc.) into the container context before execution.
