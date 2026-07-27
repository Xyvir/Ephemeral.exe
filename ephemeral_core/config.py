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
    'python': {'image': 'python:3.10-slim', 'cmd': ['python', '-']},
    'node':   {'image': 'node:18-alpine',   'cmd': ['node', '-']},
    'bash':   {'image': 'alpine:latest',    'cmd': ['sh']},
    'ruby':   {'image': 'ruby:alpine',      'cmd': ['ruby']},
    
    # --- TiddlyWiki (Build Environment) ---
    'tiddlywiki': {
        'image': 'elasticdog/tiddlywiki', 
        'entrypoint': '', 
        'cmd': ['-c', 'cat > /tmp/build_script.sh && chmod +x /tmp/build_script.sh && /tmp/build_script.sh']
    },

    # --- GitHub Actions / CI Tools ---
    'gh-runner': {
        'image': 'catthehacker/ubuntu:act-22.04', 
        'cmd': ['bash']
    },
    'actionlint': {
        'image': 'rhysd/actionlint:latest',
        'entrypoint': '',
        'cmd': ['-c', 'cat > /tmp/main.yml && actionlint /tmp/main.yml']
    },
    # --- Emulation & Cross-Compilation ---
    'pywine': {
        'image': 'tobix/pywine:latest',
        'cmd': ['bash']
    },

    # --- Science & Data ---
    'science': {'image': 'continuumio/anaconda3', 'cmd': ['python', '-']},
    'octave':  {'image': 'tymills620/octave-forge:latest', 'cmd': ['octave', '--no-gui', '--quiet']},
    'r':       {'image': 'r-base:latest',           'cmd': ['R', '--vanilla', '--slave', '-f', '/dev/stdin']},
    'julia':   {'image': 'julia:alpine',            'cmd': ['julia']},

    # --- Systems & Compiled (Compile-and-Run Chains) ---
    'c':       {'image': 'gcc:latest', 'cmd': ['sh', '-c', 'gcc -x c - -o /tmp/run && /tmp/run']},
    'cpp':     {'image': 'gcc:latest', 'cmd': ['sh', '-c', 'g++ -x c++ - -o /tmp/run && /tmp/run']},
    'fortran': {'image': 'gcc:latest', 'cmd': ['sh', '-c', 'gfortran -x f95 - -o /tmp/run && /tmp/run']},
    'rust':    {'image': 'rust:alpine', 'cmd': ['sh', '-c', 'rustc - -o /tmp/run && /tmp/run']},
    'go':      {'image': 'golang:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/main.go && go run /tmp/main.go']},
    
    # --- Expansion Pack (Systems) ---
    'java':    {'image': 'eclipse-temurin:21-jdk-alpine', 'cmd': ['sh', '-c', 'cat > /tmp/Main.java && java /tmp/Main.java']},

    # --- Golfing & Modern Compiled ---
    'crystal': {'image': 'crystallang/crystal:latest', 'cmd': ['sh', '-c', 'cat > /tmp/run.cr && crystal run /tmp/run.cr']},
    'nim':     {'image': 'nimlang/nim:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.nim && nim c -r --verbosity:0 --hints:off /tmp/run.nim']},

    # --- Lisp & Functional ---
    'lisp':    {'image': 'clfoundation/sbcl:slim', 'cmd': ['sh', '-c', 'cat > /tmp/run.lisp && sbcl --script /tmp/run.lisp']},
    'clojure': {'image': 'clojure:temurin-17-alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.clj && clojure -M /tmp/run.clj']},
    'elixir':  {'image': 'elixir:alpine', 'cmd': ['sh', '-c', 'cat > /tmp/run.exs && elixir /tmp/run.exs']},
    'ocaml':   {'image': 'ocaml/opam', 'cmd': ['sh', '-c', 'cat > /tmp/run.ml && ocaml /tmp/run.ml']},

    # --- Logic ---
    'prolog':  {'image': 'swipl:latest', 'cmd': ['swipl', '-q', '-f', '/dev/stdin', '-t', 'halt']},

    # --- Esoteric ---
    'brainfuck': {'image': 'esolang/brainfuck-esotope', 'cmd': ['sh', '-c', 'cat > /tmp/code && script /tmp/code']},

    # --- Hardware Description (HDL) ---
    'verilog': {'image': 'hdlc/iverilog', 'cmd': ['sh', '-c', 'cat > /tmp/run.v && iverilog /tmp/run.v -o /tmp/out && vvp /tmp/out']},

    # --- Functional & Scripting ---
    'haskell': {'image': 'haskell:slim', 'cmd': ['runghc']},
    'lua':     {'image': 'nickblah/lua:5.4-alpine', 'cmd': ['lua', '-']},
    'perl':    {'image': 'perl:slim',       'cmd': ['perl', '-']},
    'php':     {'image': 'php:alpine',      'cmd': ['php']},

    # --- Documents & Typesetting ---
    'latex':   {'image': 'pandoc/extra', 'entrypoint': '', 'cmd': ['-c', 'cat > /output/doc.tex && pdflatex -output-directory /output /output/doc.tex']},
    'pandoc':  {'image': 'pandoc/extra', 'entrypoint': '', 'cmd': ['-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.pdf']},
    'pandoc-pdf': {'image': 'pandoc/extra', 'entrypoint': '', 'cmd': ['-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.pdf']},
    'pandoc-docx': {'image': 'pandoc/extra', 'entrypoint': '', 'cmd': ['-c', 'cat > /tmp/input.md && pandoc /tmp/input.md -o /output/converted.docx']},

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
        LANG_MAP[lang] = {'image': f'esolang/{lang}', 'cmd': ['sh', '-c', 'cat > /tmp/code && script /tmp/code']}
