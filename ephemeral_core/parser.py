"""
Ephemeral Markdown parser: codeblock extraction and runtime resolution.

Extracted verbatim from the original ephemeral.py (lines 179-380).
No GUI, HTTP, or platform-specific code exists in this module.
"""
import re
import shlex
import sys

from .config import CHAIN_FLAGS, LANG_MAP, NETWORK_FLAGS, NO_CHAIN_FLAGS


# --- Python Dependency Inference (PEP 723) ---

# Static fallback list of Python standard-library module names, merged with
# sys.stdlib_module_names (Python 3.10+) so inference never tries to pip-install
# modules that ship with the interpreter itself.
_STDLIB_STATIC = frozenset("""
__future__ _abc _aix_support _ast _asyncio _bisect _blake2 _bootsubprocess _bz2 _codecs
_collections _collections_abc _compat_pickle _compression _contextvars _crypt _csv _ctypes
_curses _dataclasses _datetime _dbm _decimal _elementtree _frozen_importlib
_frozen_importlib_external _functools _gdbm _hashlib _heapq _imp _io _json _locale _lsprof
_lzma _markupbase _md5 _multibytecodec _multiprocessing _opcode _operator _osx_support
_overlapped _pickle _posixshmem _posixsubprocess _py_abc _pydecimal _pyio _queue _random
_sha1 _sha2 _sha3 _signal _sitebuiltins _socket _sqlite3 _sre _ssl _stat _statistics
_string _strptime _struct _symtable _thread _threading_local _tkinter _tokenize _tracemalloc
_typing _uuid _warnings _weakref _weakrefset _winapi _xxinterpchannels _xxsubinterpreters
_zoneinfo abc aifc antigravity argparse array ast asynchat asyncio asyncore atexit audioop
base64 bdb binascii binhex bisect builtins bz2 cProfile calendar cgi cgitb chunk cmath
cmd code codecs codeop collections colorsys compileall concurrent configparser contextlib
contextvars copy copyreg crypt csv ctypes curses dataclasses datetime dbm decimal
difflib dis doctest email encodings ensurepip enum errno faulthandler fcntl filecmp
fileinput fnmatch fractions ftplib functools gc genericpath getopt getpass gettext glob
graphlib grp gzip hashlib heapq hmac html http idlelib imaplib imghdr importlib
inspect io ipaddress itertools json keyword lib2to3 linecache locale logging lzma mailbox
mailcap marshal math mimetypes mmap modulefinder msilib msvcrt multiprocessing netrc nis
nntplib nt ntpath nturl2path numbers opcode operator optparse os ossaudiodev pathlib pdb
pickle pickletools pip pkgutil platform plistlib poplib posix posixpath pprint profile
pstats pty pwd py_compile pyclbr pydoc pydoc_data queue quopri random re readline
reprlib resource rlcompleter runpy sched secrets select selectors shelve shlex shutil
signal site smtpd smtplib sndhdr socket socketserver spwd sqlite3 sre_compile sre_constants
sre_parse ssl stat statistics string stringprep struct subprocess sunau symtable sys
sysconfig syslog tabnanny tarfile telnetlib tempfile termios test textwrap threading time
timeit tkinter token tokenize tomllib trace traceback tracemalloc tty turtle turtledemo types
typing unicodedata unittest urllib uu uuid venv warnings wave weakref webbrowser winreg
winsound wsgiobj xdrlib xml xmlrpc zipapp zipfile zipimport zlib zoneinfo
""".split())

STDLIB_MODULES = _STDLIB_STATIC | frozenset(getattr(sys, "stdlib_module_names", ()))

# Matches `from X import ...` and `import ...` statements at the start of a line.
# The character class deliberately excludes newlines so an `import` statement
# cannot span lines (multi-line `from x import (a, b)` still yields root `x`).
_IMPORT_STATEMENT_RE = re.compile(
    r"^\s*(?:from\s+([A-Za-z_][A-Za-z0-9_.]*)\s+import|import\s+([A-Za-z_][A-Za-z0-9_., \t]+))",
    re.MULTILINE,
)

# PEP 723 inline script metadata block: `# /// script` ... `# ///`
_PEP723_BLOCK_RE = re.compile(
    r"^\s*#\s*///\s*script\b.*?^\s*#\s*///\s*$",
    re.MULTILINE | re.DOTALL,
)


def _clean_import_name(name: str) -> str:
    """Reduce an import clause to its top-level package name."""
    name = name.strip()
    name = re.split(r"\s+as\s+", name)[0].strip()  # drop `as alias`
    return name.split(".")[0]


def infer_python_dependencies(code: str) -> list[str]:
    """
    Infer third-party PyPI packages from the import statements in Python code.

    Only the top-level package of each import is returned (e.g. `import
    pandas.core` yields ``pandas``), and standard-library modules are excluded.
    Returns a sorted, de-duplicated list suitable for PEP 723 metadata or
    ``pip install``.
    """
    deps = set()
    for match in _IMPORT_STATEMENT_RE.finditer(code or ""):
        if match.group(1):
            deps.add(_clean_import_name(match.group(1)))
        else:
            for part in match.group(2).split(","):
                if part.strip():
                    deps.add(_clean_import_name(part))
    return sorted(d for d in deps if d and d not in STDLIB_MODULES)


def build_python_dependency_header(deps: list[str]) -> str:
    """Build a PEP 723 inline-script metadata header declaring ``deps``."""
    if not deps:
        return ""
    lines = ["# /// script", "# dependencies = ["]
    lines.extend(f'#     "{d}",' for d in deps)
    lines.append("# ]")
    lines.append("# ///")
    return "\n".join(lines)


def has_pep723_metadata(code: str) -> bool:
    """Return True if the code already declares PEP 723 inline metadata."""
    return bool(_PEP723_BLOCK_RE.search(code or ""))


def extract_declared_dependencies(code: str) -> list[str]:
    """
    Extract the ``dependencies`` list from existing PEP 723 inline metadata.

    Version specifiers are preserved as written (e.g. ``"requests<3"``), so the
    returned values can be fed straight back into ``pip install``.
    """
    match = _PEP723_BLOCK_RE.search(code or "")
    if not match:
        return []
    deps = []
    in_deps = False
    for line in match.group(0).splitlines():
        stripped = line.lstrip("#").strip()
        if stripped.startswith("dependencies") and "=" in stripped:
            in_deps = True
            stripped = stripped.split("=", 1)[1]
        if in_deps:
            deps.extend(re.findall(r"[\"']([^\"']+)[\"']", stripped))
            if "]" in stripped:
                in_deps = False
    return deps


def inject_python_dependency_metadata(code: str, deps: list[str]) -> str:
    """
    Inject a PEP 723 header declaring ``deps`` at the top of the code.

    Existing PEP 723 metadata is never clobbered, and a leading shebang is
    preserved above the injected header.
    """
    if not deps or has_pep723_metadata(code):
        return code
    header = build_python_dependency_header(deps) + "\n"
    if code.lstrip().startswith("#!"):
        first_newline = code.find("\n")
        if first_newline == -1:
            return code
        return code[: first_newline + 1] + header + code[first_newline + 1 :]
    return header + code


def prepare_python_block(block: dict) -> tuple[dict, list[str]]:
    """
    Prepare a Python code block for execution with dependency resolution.

    If the block already declares PEP 723 metadata, its declared dependencies
    are authoritative and the block is returned untouched. Otherwise, third-party
    imports are inferred and a PEP 723 header is injected into the block content.

    Returns ``(block, deps)`` where ``deps`` is the list of packages that must be
    resolvable before the payload runs.
    """
    content = block.get("content", "")
    # Explicit PEP 723 metadata is authoritative: use exactly what the user
    # declared (even an empty `dependencies = []`), never infer or re-inject.
    if has_pep723_metadata(content):
        return block, extract_declared_dependencies(content)
    deps = infer_python_dependencies(content)
    if deps:
        new_block = dict(block)
        new_block["content"] = inject_python_dependency_metadata(content, deps)
        return new_block, deps
    return block, []


# --- Generic Two-Stage Dependency Resolution -------------------------------
#
# The Python path (uv + PEP 723) and the TeX path (tlmgr) share the same
# shape: infer (or read declared) dependencies from block content, install
# them in a network-enabled Stage A container into a shared volume, then run
# the payload in a network-disabled Stage C container that reads that volume.
# A resolver describes how a language family plugs into that procedure:
#
#   infer        -> (list[str], dict): parse the content and return
#                   (dependency specs, block mutations). Empty deps mean the
#                   block runs in the normal single-stage path.
#   stage_a      -> str: POSIX sh snippet run with network access in Stage A
#                   (receives $DEPS_DIR; must populate it for Stage C)
#   run_cmd      -> list[str]: command (plus '-') Stage C uses to execute the
#                   payload when the block's own cmd is not reusable; None to
#                   reuse config['cmd'] unchanged
#   stage_c_env  -> dict[str, str]: extra environment variables Stage C needs
#                   to find the installed dependencies (e.g. TEXMFHOME)
#   payload_lang -> display title for the Stage C output header

# The TeX resolver's Stage A: tlmgr --usermode into the shared mount.
#
# The pandoc/extra image ships a full TeX Live under /opt/texlive with tlmgr
# configured against CTAN (verified: the image builds TeX Live via install-tl).
# User mode installs into $TEXMFHOME (no writes to the root-owned main tree),
# and pointing TEXMFHOME at the shared /deps mount lets Stage C read the
# packages with the network gone — kpathsea searches TEXMFHOME natively, so
# no ls-R database or mktexlsr run is needed.

def _build_tlmgr_stage_a(pkgs: list[str]) -> str:
    """Build the Stage A script for the TeX resolver (one tlmgr call per package)."""
    lines = [
        "mkdir -p /deps/texmf/tex/latex",
        "export TEXMFHOME=/deps/texmf",
        # User mode must be initialized once per fresh container before
        # `tlmgr --usermode` works; it populates the user tree's tlpkg.
        "tlmgr init-usermode >/dev/null 2>&1 || true",
    ]
    for p in pkgs:
        q = shlex.quote(p)
        lines.append(f"tlmgr --usermode install {q} || exit 1")
    return "\n".join(lines) + "\n"


# LaTeX kernel components that \\usepackage accepts but that have no standalone
# tlmgr package (they ship inside TeX Live's latex base collection; asking
# tlmgr for them fails with "not present in repository"). Everything else is
# installed as-is — if a name is genuinely unknown, Stage A fails naming the
# exact package, which is a far better error than silently skipping a real
# dependency.
_TEX_KERNEL_PACKAGES = frozenset({"fontenc", "inputenc", "textcomp", "upquote"})

# `\\usepackage[options]{name}` / `\\RequirePackage...` — the brace group may
# hold a comma-separated package list (standard LaTeX practice).
_USEPACKAGE_RE = re.compile(
    r"^\s*%?\\(?:use|Require)package(?:\[[^\]]*\])?\{([^}]+)\}",
    re.MULTILINE,
)

# --- Pandoc YAML metadata (`header-includes`) recognition --------------------
#
# Pandoc users declare LaTeX packages in YAML metadata, not (only) in TeX
# source, and YAML offers several spellings of the same thing:
#
#   header-includes: |
#     \usepackage{minted}            <- block scalar: one statement per line
#   header-includes:
#     - \usepackage{tcolorbox}       <- list item
#   header-includes: \usepackage{xcolor}
#   header-includes: ["\\usepackage{a}", \\usepackage{b}]   <- inline flow list
#
# A PyYAML dependency is deliberately avoided (the core pipeline has none and
# only preamble *recognition* is needed, not a document tree): these regexes
# find the `header-includes` key and the lines that belong to it. Harvested
# lines are re-scanned by _USEPACKAGE_RE, so package-name parsing stays in
# exactly one place.
#
# A top-level `header-includes:` key at column 0. (Pandoc also accepts nested
# `include-before/includes:` forms; matching only the top-level key keeps the
# scanner predictable without false positives in prose.)
_HEADER_INCLUDES_KEY_RE = re.compile(r"^header-includes\s*:")
# Everything after `header-includes:` on the same line (may be empty).
_HEADER_INCLUDES_INLINE_RE = re.compile(r"^header-includes\s*:\s*(.*)$")
# Continuation lines: deeper-indented block-scalar text or `- item` entries
# (the dash may sit at any indentation, including column 0).
_HEADER_INCLUDES_ITEM_RE = re.compile(r"^(?:\s+(?:-\s+)?|-\s+)(.*)$")
# YAML comments start at a `#` preceded by line start or whitespace (a `#`
# inside quotes is rare in these preamble lines and not worth the complexity).
_YAML_COMMENT_RE = re.compile(r"(?:^|\s)#.*$")
_BLOCK_SCALAR_INDICATORS = frozenset({"|", ">", "|-", ">-", "|+", ">+"})


def _iter_header_include_lines(code: str):
    """
    Yield LaTeX source lines found inside pandoc YAML `header-includes`
    metadata (all common YAML spellings), so they can be re-scanned by
    _USEPACKAGE_RE alongside the block body.

    Handles: block scalars (`|`/`>` with per-line statements), block-sequence
    list items (`- \\usepackage{...}`, dash at any indentation), inline values
    (`header-includes: \\usepackage{...}`, quotes stripped), and inline flow
    lists (`header-includes: ["\\usepackage{a}", "\\usepackage{b}"]`, which
    must START with `[` so package options like `[overload]` are never
    mistaken for a flow list). YAML comments are stripped from harvested
    lines; TeX `%` comments are stripped later by the body scan.
    """
    lines = (code or "").splitlines()
    in_block = False
    for line in lines:
        if not in_block and _HEADER_INCLUDES_KEY_RE.match(line):
            value = _HEADER_INCLUDES_INLINE_RE.match(line).group(1)
            value = _YAML_COMMENT_RE.sub("", value).strip()
            if not value or value in _BLOCK_SCALAR_INDICATORS:
                in_block = True  # entries start on the following lines
                continue
            if value.startswith("[") and value.endswith("]"):
                # Inline flow list: split on commas, drop surrounding quotes.
                for item in value[1:-1].split(","):
                    item = item.strip()
                    if len(item) >= 2 and item[0] == item[-1] and item[0] in "'\"":
                        item = item[1:-1].strip()
                    if item:
                        yield item
            else:
                if len(value) >= 2 and value[0] == value[-1] and value[0] in "'\"":
                    value = value[1:-1].strip()  # quoted inline scalar
                if value:
                    yield value
            continue
        if in_block:
            if not line.strip():
                continue  # blank line inside a block scalar
            if not line[0].isspace() and not line.lstrip().startswith("-"):
                in_block = False  # back at column 0: metadata block is over
                if _HEADER_INCLUDES_KEY_RE.match(line):
                    in_block = True
                    continue
                continue
            m = _HEADER_INCLUDES_ITEM_RE.match(line)
            if m:
                text = _YAML_COMMENT_RE.sub("", m.group(1)).strip()
                if text in _BLOCK_SCALAR_INDICATORS:
                    continue  # nested block-scalar indicator; its lines follow
                if text:
                    yield text


def infer_latex_dependencies(code: str) -> tuple[list[str], dict]:
    """
    Infer TeX Live packages from the `\\usepackage`/`\\RequirePackage`
    statements in LaTeX or pandoc content, including packages declared in
    pandoc YAML `header-includes` metadata (block scalar, list item, inline,
    and inline flow-list forms — see _iter_header_include_lines).

    Returns ``(packages, mutations)``. Package names are used as-is for
    ``tlmgr install`` (the CTAN package name usually matches the
    `\\usepackage` name); kernel components with no tlmgr package are
    filtered via ``_TEX_KERNEL_PACKAGES``, and TeX comments are stripped
    first so commented-out packages are not installed.
    ``mutations`` is empty for the TeX resolver: unlike PEP 723, tlmgr needs
    no inline metadata — Stage A installs straight from the inferred list.

    `\\documentclass` is deliberately NOT scanned: class names map to tlmgr
    package names unreliably (article → latex-base, scrartcl → koma-script),
    and the classes a pandoc-pdf block realistically uses ship in the image.
    """
    code = code or ""
    deps: set[str] = set()
    # Harvest LaTeX lines from pandoc YAML metadata FIRST, so the combined
    # text gets the same TeX-comment stripping as the body below.
    code = code + "\n" + "\n".join(_iter_header_include_lines(code))
    # Strip TeX comments (an unescaped % kills the rest of the line).
    code = re.sub(r"(?<!\\)%.*$", "", code, flags=re.MULTILINE)
    for match in _USEPACKAGE_RE.finditer(code):
        # One brace group can list several packages: \usepackage{amsmath,amssymb}
        for name in match.group(1).split(","):
            name = name.strip()
            if name and name not in _TEX_KERNEL_PACKAGES:
                deps.add(name)
    return sorted(deps), {}


def _python_infer(block: dict) -> tuple[list[str], dict]:
    """Adapter so the Python resolver fits the generic ``(deps, mutations)`` shape."""
    prepared, deps = prepare_python_block(block)
    return deps, {"content": prepared["content"]} if deps else {}


#: Two-stage dependency resolvers, keyed by the IMAGE REPOSITORY (tag stripped)
#: that owns the payload. The image is the right key — not the language name —
#: because a resolver's Stage A tooling must exist inside that exact image
#: (uv in ephemeral-python-uv, tlmgr in pandoc/extra); every language that
#: resolves to the same image (python:3.11 overrides, pandoc-pdf/md aliases)
#: shares the resolver automatically, and a local `image=` override pointing
#: elsewhere correctly falls back to the single-stage path. Languages not
#: matched here (or matched with inference that found nothing) run in the
#: normal single-stage path.
DEP_RESOLVERS: dict[str, dict] = {
    'docker.io/tymills620/ephemeral-python-uv': {
        'infer': _python_infer,
        'stage_a': lambda deps: (
            "uv venv /deps/venv || exit 1\n"
            "uv pip install --no-cache --python /deps/venv/bin/python "
            + " ".join(shlex.quote(d) for d in deps)
            + "\n"
        ),
        'run_cmd': ['/deps/venv/bin/python', '-'],
        'stage_c_env': {},
        'payload_lang': 'python',
    },
    'docker.io/pandoc/extra': {
        'infer': lambda block: infer_latex_dependencies(block.get("content", "")),
        'stage_a': _build_tlmgr_stage_a,
        'run_cmd': None,  # Stage C reuses the block's own cmd (pdflatex / pandoc)
        'stage_c_env': {'TEXMFHOME': '/deps/texmf'},
        'payload_lang': 'latex',
    },
}


def _image_registry_key(image: str) -> str:
    """Strip the tag from a Docker/Podman image reference (keep registry ports)."""
    head, sep, tail = image.rpartition('/')
    if ':' in tail:
        tail = tail.split(':')[0]
    return head + sep + tail


def resolver_for_image(image: str) -> dict | None:
    """Return the two-stage resolver for the block's image, or None."""
    return DEP_RESOLVERS.get(_image_registry_key(image or ""))


def prepare_block_for_resolution(block: dict, resolver: dict) -> tuple[dict, list[str]]:
    """
    Prepare one code block for two-stage dependency resolution.

    The resolver's ``infer`` hook produces ``(deps, mutations)``; the returned
    block is a copy with ``mutations`` applied (e.g. PEP 723 header injection
    for Python). When inference finds nothing, the block is returned untouched
    with an empty dep list and the caller keeps the single-stage path.

    Returns ``(prepared_block, deps)``.
    """
    deps, mutations = resolver["infer"](block)
    if not deps:
        return block, []
    prepared = dict(block)
    prepared.update(mutations)
    return prepared, deps


def strip_ansi_codes(text: str) -> str:
    """Remove ANSI escape sequences from text."""
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
    return ansi_escape.sub('', text)


def strip_shebang(text: str) -> str:
    """Remove the first line if it's a shebang (#!) directive."""
    if not text: return text
    if text.lstrip().startswith("#!"):
        parts = text.split('\n', 1)
        if len(parts) > 1:
            return parts[1]
        return ""
    return text


def __shlex_join(split_command: list[str]) -> str:
    """Compatibility wrapper for shlex.join (added in Python 3.8)."""
    if hasattr(shlex, 'join'):
        return shlex.join(split_command)
    return ' '.join(shlex.quote(arg) for arg in split_command)


def resolve_runtime_config(header_line: str) -> dict | None:
    """
    Parse a codeblock header line into a runtime configuration dictionary.
    
    Resolves the language, image, command, version overrides, network flags,
    and chaining flags from the header tokens.
    
    Returns None if the header is empty or unresolvable.
    """
    if not header_line: return None
    try: tokens = shlex.split(header_line)
    except: tokens = header_line.split() 
    if not tokens: return None

    # 1. Detect Network / Chaining Flags. Chaining is OFF by default;
    #    declaring it (chain/piping/pipe) opts the request into the
    #    sequential, artifact-piping execution path. `nopipe`/`nopiping`
    #    (legacy) still win when both are present.
    network_enabled = False
    chain_enabled = False
    cleaned_tokens = []

    for token in tokens:
        low = token.lower()
        if low in NETWORK_FLAGS:
            network_enabled = True
        elif low in CHAIN_FLAGS:
            chain_enabled = True
        elif low in NO_CHAIN_FLAGS:
            chain_enabled = False
        else:
            cleaned_tokens.append(token)
            
    if not cleaned_tokens: return None
    
    # 2. Parse Language
    base_lang_input = cleaned_tokens[0].lower()
    overrides = {}
    
    for token in cleaned_tokens[1:]:
        if '=' in token:
            key, val = token.split('=', 1)
            overrides[key.lower()] = val

    base_lang = base_lang_input
    version = None
    match = re.match(r"^([a-z0-9\+\#]+)(?:[:\-](\d+(?:\.\d+)*))?$", base_lang_input)
    if match:
        base_lang = match.group(1)
        version = match.group(2) 
        
    if base_lang in LANG_MAP:
        resolved = LANG_MAP[base_lang]
        if isinstance(resolved, str):
            base_lang = resolved
            if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], str):
                 base_lang = LANG_MAP[base_lang]
        elif isinstance(resolved, dict):
            pass

    config = None
    if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], dict):
        config = LANG_MAP[base_lang].copy()
    
    if not config:
        if 'image' in overrides: config = {'image': '', 'cmd': []}
        else:
            image_tag = f"{base_lang_input}" if ':' in base_lang_input else f"{base_lang_input}:latest"
            config = {'image': image_tag, 'cmd': [base_lang, '-']}

    if version and config and 'image' not in overrides:
        original_image = config.get('image', '')
        if ':' in original_image:
            repo = original_image.split(':')[0]
            config['image'] = f"{repo}:{version}"
        else:
            config['image'] = f"{original_image}:{version}"

    if 'image' in overrides: config['image'] = overrides['image']
    if 'cmd' in overrides: config['cmd'] = shlex.split(overrides['cmd'])
    if 'entrypoint' in overrides: config['entrypoint'] = overrides['entrypoint']
    
    config['allow_network'] = network_enabled
    config['allow_chain'] = chain_enabled
    return config


def parse_codeblocks(content: str) -> list[dict]:
    """
    Parse Markdown content into classified codeblocks.
    
    Returns a list of dicts, each either:
      - {'type': 'seed', 'name': str, 'content': str, 'is_b64': bool}
      - {'type': 'code', 'header': str, 'content': str, 'config': dict}
    
    Supports fenced codeblocks (```) and shebang (#!) syntax.
    """
    blocks = []
    if not content or not content.strip():
        return blocks

    # Strip any lines starting with 4 or more backticks (to ignore markdown documentation wrappers)
    content = re.sub(r"(?m)^\s*`{4,}.*$\n?", "", content)

    pattern = r"```(.*?)\n(.*?)```"
    matches = list(re.finditer(pattern, content, re.DOTALL))
    if matches:
        for match in matches:
            header = match.group(1).strip() if match.group(1) else ""
            block_content = match.group(2)
            
            block_lines = block_content.splitlines()
            if block_lines:
                first_line = block_lines[0].strip()
                if first_line.startswith("#!"):
                    shebang_val = first_line.lstrip("#!").strip()
                    block_content = strip_shebang(block_content)
                    
                    # If shebang exists, it OVERWRITES the markdown header
                    header = shebang_val
                
            blocks.append({'header': header, 'content': block_content})
    else:
        parts = re.split(r"(?m)^#![ \t]*", content)
        if len(parts) > 1:
            for i, part in enumerate(parts):
                if i == 0 and not part.strip():
                    continue
                if i == 0 and part.strip():
                    blocks.append({'header': '', 'content': part})
                    continue
                part_lines = part.split('\n', 1)
                header = part_lines[0].strip()
                block_content = part_lines[1] if len(part_lines) > 1 else ""
                blocks.append({'header': header, 'content': block_content})
        else:
            blocks.append({'header': '', 'content': content})

    classified = []
    for b in blocks:
        header = b['header']
        tokens = header.split() if header else []
        is_seed = False
        is_b64 = False
        if tokens:
            first_token = tokens[0]
            if re.search(r'\.[a-zA-Z0-9]{1,8}$', first_token) and ':' not in first_token and first_token.lower() not in LANG_MAP:
                is_seed = True
            elif first_token.lower() == 'file' and len(tokens) > 1:
                first_token = tokens[1]
                is_seed = True
                
            if is_seed and 'b64' in [t.lower() for t in tokens]:
                is_b64 = True
                
        if is_seed:
            classified.append({'type': 'seed', 'name': first_token, 'content': b['content'], 'is_b64': is_b64})
        else:
            config = resolve_runtime_config(header)
            classified.append({'type': 'code', 'header': header, 'content': b['content'], 'config': config})
            
    return classified
