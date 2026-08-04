"""
Ephemeral configuration: language maps, runtime flags, and constants.

Extracted verbatim from the original ephemeral.py (lines 26-149).
No GUI, HTTP, or platform-specific code exists in this module.
"""
import re

# --- Configuration ---
APP_NAME = "Ephemeral"

# Only this specific keyword enables network access
NETWORK_FLAGS = {'unsafe'}

# Keywords that prevent piping /output to the root of the next container
NO_CHAIN_FLAGS = {'nopipe', 'nopiping'}

# Map languages to the 'Clean Slate' Image on Docker Hub
LANG_MAP = {
    # --- Standard Interpreted ---
    'python': {'image': 'ghcr.io/astral-sh/uv:python3.12-alpine', 'cmd': ['uv', 'run', '-']},
    'node':   {'image': 'docker.io/library/node:18-alpine',   'cmd': ['node', '-']},
    'bash':   {'image': 'docker.io/library/alpine:latest',    'cmd': ['sh']},
    'ruby':   {'image': 'docker.io/library/ruby:alpine',      'cmd': ['ruby']},
    
    # --- TiddlyWiki (Build Environment) ---
    'tiddlywiki': {
        'image': 'docker.io/elasticdog/tiddlywiki', 
        'entrypoint': '', 
        'cmd': ['sh', '-c', 'cat > /tmp/build_script.sh && chmod +x /tmp/build_script.sh && /tmp/build_script.sh']
    },

    # --- GitHub Actions / CI Tools ---
    'gh-runner': {
        'image': 'docker.io/catthehacker/ubuntu:act-22.04', 
        'cmd': ['bash']
    },
    'actionlint': {
        'image': 'docker.io/rhysd/actionlint:latest',
        'entrypoint': '',
        'cmd': ['sh', '-c', 'cat > /tmp/main.yml && actionlint /tmp/main.yml']
    },
    # --- Emulation & Cross-Compilation ---
    'pywine': {
        'image': 'docker.io/tobix/pywine:latest',
        'cmd': ['bash']
    },

    # --- Science & Data ---
    'science': {'image': 'docker.io/continuumio/anaconda3', 'cmd': ['python', '-']},
    'octave':  {'image': 'docker.io/tymills620/octave-forge:latest', 'cmd': ['octave', '--no-gui', '--quiet']},
    'r':       {'image': 'docker.io/library/r-base:latest',           'cmd': ['R', '--vanilla', '--slave', '-f', '/dev/stdin']},
    'julia':   {'image': 'docker.io/library/julia:alpine',            'cmd': ['julia']},

    # --- Systems & Compiled (Compile-and-Run Chains) ---
    'c':       {'image': 'docker.io/library/gcc:latest', 'cmd': ['sh', '-c', 'gcc -x c - -o /tmp/run && /tmp/run']},
    'cpp':     {'image': 'docker.io/library/gcc:latest', 'cmd': ['sh', '-c', 'g++ -x c++ - -o /tmp/run && /tmp/run']},
    'fortran': {'image': 'docker.io/library/gcc:latest', 'cmd': ['sh', '-c', 'gfortran -x f95 - -o /tmp/run && /tmp/run']},
    'rust':    {'image': 'docker.io/library/rust:alpine', 'cmd': ['sh', '-c', 'rustc - -o /tmp/run && /tmp/run']},
    'go':      {'image': 'docker.io/library/golang:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/main.go && go run /tmp/main.go']},
    
    # --- Expansion Pack (Systems) ---
    'java':    {'image': 'docker.io/library/eclipse-temurin:21-jdk-alpine', 'cmd': ['sh', '-c', 'cat > /tmp/Main.java && java /tmp/Main.java']},

    # --- Golfing & Modern Compiled ---
    'crystal': {'image': 'docker.io/crystallang/crystal:latest', 'cmd': ['sh', '-c', 'cat > /tmp/run.cr && crystal run /tmp/run.cr']},
    'nim':     {'image': 'docker.io/nimlang/nim:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.nim && nim c -r --verbosity:0 --hints:off /tmp/run.nim']},

    # --- Lisp & Functional ---
    'lisp':    {'image': 'docker.io/clfoundation/sbcl:slim', 'cmd': ['sh', '-c', 'cat > /tmp/run.lisp && sbcl --script /tmp/run.lisp']},
    'clojure': {'image': 'docker.io/library/clojure:temurin-17-alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.clj && clojure -M /tmp/run.clj']},
    'elixir':  {'image': 'docker.io/library/elixir:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.exs && elixir /tmp/run.exs']},
    'ocaml':   {'image': 'docker.io/ocaml/opam', 'cmd': ['sh', '-c', 'cat > /tmp/run.ml && ocaml /tmp/run.ml']},

    # --- Logic ---
    'prolog':  {'image': 'docker.io/library/swipl:latest', 'cmd': ['swipl', '-q', '-f', '/dev/stdin', '-t', 'halt']},

    # --- Esoteric ---
    'brainfuck': {'image': 'docker.io/esolang/brainfuck-esotope', 'cmd': ['sh', '-c', 'cat > /tmp/code && script /tmp/code']},

    # --- Hardware Description (HDL) ---
    'verilog': {'image': 'docker.io/hdlc/iverilog', 'cmd': ['sh', '-c', 'cat > /tmp/run.v && iverilog /tmp/run.v -o /tmp/out && vvp /tmp/out']},

    # --- Functional & Scripting ---
    'haskell': {'image': 'docker.io/library/haskell:slim', 'cmd': ['runghc']},
    'lua':     {'image': 'docker.io/nickblah/lua:5.4-alpine', 'cmd': ['lua', '-']},
    'perl':    {'image': 'docker.io/library/perl:slim',       'cmd': ['perl', '-']},
    'php':     {'image': 'docker.io/library/php:alpine',      'cmd': ['php']},

    # --- Documents & Typesetting ---
    'latex':   {'image': 'docker.io/pandoc/extra', 'entrypoint': '', 'cmd': ['sh', '-c', 'cat > /output/doc.tex && pdflatex -output-directory /output /output/doc.tex']},
    'pandoc':  {'image': 'docker.io/pandoc/extra', 'entrypoint': '', 'cmd': ['sh', '-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.pdf']},
    'pandoc-pdf': {'image': 'docker.io/pandoc/extra', 'entrypoint': '', 'cmd': ['sh', '-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.pdf']},
    'pandoc-docx': {'image': 'docker.io/pandoc/extra', 'entrypoint': '', 'cmd': ['sh', '-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.docx']},

    # --- Windows-like Shells ---
    'pwsh':    {'image': 'mcr.microsoft.com/powershell', 'cmd': ['pwsh', '-NoProfile', '-NonInteractive', '-Command', '-']},
    
    # --- Aliases ---
    'py': 'python', 'js': 'node', 'javascript': 'node', 'npm': 'node', 'npx': 'node', 'cjs': 'node', 'mjs': 'node', 'sh': 'bash',
    'numpy': 'science', 'pandas': 'science',
    'matlab': 'octave',
    'powershell': 'pwsh', 'ps1': 'pwsh', 'cmd': 'pwsh', 'batch': 'pwsh',
    'R': 'r',
    'golang': 'go', 'cc': 'c', 'c++': 'cpp',
    'f90': 'fortran', 'f95': 'fortran',
    'sbcl': 'lisp', 'cl': 'lisp', 'common-lisp': 'lisp',
    'clj': 'clojure', 'ex': 'elixir', 'exs': 'elixir',
    'ml': 'ocaml',
    'swipl': 'prolog', 'pl': 'prolog',
    'cr': 'crystal', 'nimrod': 'nim',
    'bf': 'brainfuck', 'spl': 'shakespeare', '><>': 'fish',
    'cob': 'cobol', 'gnucobol': 'cobol',
    'tw': 'tiddlywiki', 'tw5': 'tiddlywiki', 'wiki': 'tiddlywiki',
    'tex': 'latex', 'pdflatex': 'latex',
    'md': 'pandoc', 'markdown': 'pandoc',
    'runner': 'gh-runner', 'ubuntu-latest': 'gh-runner',
    'lint-action': 'actionlint'
}

# Add esolangs dynamically
ESOLANGS = [
    '05ab1e', 'golfscript', 'lolcode', 'piet', 'cjam', 'cobol'
]
for lang in ESOLANGS:
    if lang not in LANG_MAP:
        LANG_MAP[lang] = {'image': f'docker.io/esolang/{lang}', 'cmd': ['sh', '-c', 'cat > /tmp/code && script /tmp/code']}
