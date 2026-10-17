"""Talk to the local model baked into this container.

The image runs llama.cpp's OpenAI-compatible server on loopback (12434) before
the snippet starts. An ``llm`` block that is plain prose *is* the prompt — the
core rewrites it to ``print(chat("..."))`` — so this is a complete block::

    Explain what a pandas DataFrame is in one sentence.

Anything that parses as Python runs as Python, and ``chat``/``client`` are
already injected into the block's namespace, so no import is needed. Importing
them explicitly (``from ephemeral_llm import chat, client``) is still fine and
is what the core leaves alone when it sees it.

For anything beyond a one-shot reply (streaming, tools, response formats,
embeddings via ``/v1/embeddings``), use :func:`client` and the normal OpenAI
SDK — this module is a convenience, not a wrapper. Everything is local: the
server never leaves the container, and the block still runs with the sandbox's
network removed.

Generated code, run somewhere else
----------------------------------
:func:`ask_code` (or :func:`save_code` on a reply you already have) strips the
prose off a model answer, writes the code block to ``/output/generated`` and
returns the path. With Ephemeral's ``chain`` flag that file is staged into the
*next* container's working directory, so another block can simply run it::

    # block 1: ask for code and keep it
    from ephemeral_llm import ask_code

    ask_code("Write a small Python script that prints the first 10 primes.")

    # block 2 (any language): run what the model wrote
    import generated

The stem is fixed (``generated``, override with ``name=``) but the extension is
whatever language the reply was fenced as — ``generated.py``, ``generated.js``,
``generated.sh`` — so a Node block does ``require("./generated.js")`` and a
shell block does ``sh generated.sh``. A block the model left unlabelled is
saved with no suffix at all. The code is written verbatim: nothing is rewritten,
so what you run is exactly what the model said.

The one thing import cannot do is fire a script's ``__main__`` guard, so
:func:`ask_code` asks for top-level code; when a reply insists on a guard,
:func:`save_code` prints the ``runpy.run_path(..., run_name="__main__")`` call
that runs it anyway.
"""

from __future__ import annotations

import ast
import os
import re

from openai import OpenAI

MODEL = os.environ.get("EPHEMERAL_LLM_MODEL_NAME", "local")
BASE_URL = os.environ.get("EPHEMERAL_LLM_BASE_URL", "http://127.0.0.1:12434/v1")

#: Default stem for :func:`save_code` / :func:`ask_code`.
DEFAULT_NAME = "generated"

#: Where generated code lands. ``/output`` is the only writable path Ephemeral
#: mounts, so anything written here can be chained into the next container.
DEFAULT_DIRECTORY = "/output"


def client(**kwargs) -> OpenAI:
    """An OpenAI client pointed at the in-container llama.cpp server."""
    kwargs.setdefault("base_url", BASE_URL)
    # llama.cpp ignores the key, but the SDK requires one to be set.
    kwargs.setdefault("api_key", os.environ.get("EPHEMERAL_LLM_API_KEY", "local"))
    return OpenAI(**kwargs)


def chat(prompt: str, system: str | None = None, **kwargs) -> str:
    """Send one user message (with an optional system prompt) and return the reply text.

    Extra keyword arguments are passed through to
    ``chat.completions.create`` — ``temperature``, ``max_tokens``, ``model``,
    etc.
    """
    messages = []
    if system:
        messages.append({"role": "system", "content": system})
    messages.append({"role": "user", "content": prompt})
    model = kwargs.pop("model", MODEL)
    response = client().chat.completions.create(
        model=model, messages=messages, **kwargs
    )
    return response.choices[0].message.content or ""


# ---------------------------------------------------------------------------
# Reply -> runnable file
# ---------------------------------------------------------------------------

#: Used by :func:`ask_code` so a small model reliably fences its answer, and so
#: the result runs when the next block imports it. A `main()` + `__main__` guard
#: (a very common shape) would import as a silent no-op, so the prompt asks for
#: top-level code instead of a script skeleton.
_CODE_SYSTEM = (
    "Reply with one complete program in a single fenced code block labelled "
    "with its language, and nothing else. Put the work at the top level: do not "
    "wrap it in a main() function or an `if __name__ == \"__main__\"` guard."
)

#: A fence opener: up to three spaces, three or more backticks or tildes, then
#: the info string (``python``, ``{.js}``, ``bash title=x`` ...).
_FENCE_OPEN = re.compile(r"^ {0,3}(`{3,}|~{3,})[ \t]*(.*)$")

#: ``__main__`` guard — present in most generated scripts, and the reason a
#: bare ``import`` can look like it did nothing.
_MAIN_GUARD = re.compile(
    r"^[ \t]*if[ \t]+__name__[ \t]*==[ \t]*['\"]__main__['\"][ \t]*:", re.MULTILINE
)

_PYTHON_LABELS = {"python", "py", "py3", "python3"}

#: Fence labels -> file extension. The label only picks the suffix; the code is
#: never rewritten, and an unknown label simply gets no suffix.
_EXTENSIONS = {
    "python": "py", "py": "py", "python3": "py", "py3": "py",
    "javascript": "js", "js": "js", "node": "js", "nodejs": "js", "mjs": "js",
    "typescript": "ts", "ts": "ts",
    "bash": "sh", "sh": "sh", "shell": "sh", "zsh": "sh", "console": "sh",
    "ruby": "rb", "rb": "rb", "perl": "pl", "php": "php", "lua": "lua",
    "r": "R", "julia": "jl", "octave": "m", "matlab": "m",
    "go": "go", "golang": "go", "rust": "rs", "rs": "rs",
    "c": "c", "cpp": "cpp", "c++": "cpp", "cc": "cpp", "csharp": "cs",
    "java": "java", "kotlin": "kt", "scala": "scala",
    "sql": "sql", "sqlite": "sql",
    "json": "json", "yaml": "yml", "yml": "yml", "toml": "toml",
    "html": "html", "css": "css", "xml": "xml",
    "markdown": "md", "md": "md", "latex": "tex", "tex": "tex",
    "haskell": "hs", "clojure": "clj", "elixir": "exs", "erlang": "erl",
    "crystal": "cr", "nim": "nim", "verilog": "v", "prolog": "pl",
}


def _label(info: str) -> str:
    """Normalize a fence info string to a bare language label."""
    tokens = (info or "").split()
    if not tokens:
        return ""
    # `{python}`, `{.python}`, `python,` ... -> `python`; `c++` survives intact.
    return tokens[0].strip("{},.;:").lower()


def _fenced_blocks(text: str):
    """Yield ``(label, body)`` for every fenced code block in ``text``.

    Follows CommonMark's rules that matter here: a closer is the same character
    repeated at least as many times as the opener, and anything inside is code
    (so a fence nested in a fence is not a fence).
    """
    lines = text.splitlines()
    i, total = 0, len(lines)
    while i < total:
        opening = _FENCE_OPEN.match(lines[i])
        if not opening:
            i += 1
            continue
        fence = opening.group(1)
        closer = re.compile(
            r"^ {0,3}" + re.escape(fence[0]) + "{" + str(len(fence)) + r",}[ \t]*$"
        )
        body: list[str] = []
        i += 1
        while i < total:
            if closer.match(lines[i]):
                i += 1
                break
            body.append(lines[i])
            i += 1
        yield _label(opening.group(2)), "\n".join(body)


def _extension(label: str) -> str:
    return _EXTENSIONS.get(label, "")


def _same_language(found: str, wanted: str) -> bool:
    """True when two labels name the same language (``js`` == ``javascript``)."""
    if not found or not wanted:
        return False
    if found == wanted:
        return True
    found_ext, wanted_ext = _extension(found), _extension(wanted)
    return bool(found_ext) and found_ext == wanted_ext


def _is_python(source: str) -> bool:
    try:
        ast.parse(source)
    except (SyntaxError, ValueError):
        return False
    return True


def _tidy(body: str) -> str:
    """Trim surrounding blank lines, keep the body's own formatting, end in \\n."""
    return body.strip("\n").rstrip() + "\n"


def _excerpt(text: str) -> str:
    flat = " ".join(text.split())
    return repr(flat[:120] + ("..." if len(flat) > 120 else ""))


def _extract(reply: str, lang: str | None = None) -> tuple[str, str]:
    """Return ``(code, label)`` for the code inside ``reply``.

    Raises :class:`ValueError` when there is nothing usable to save.
    """
    text = (reply or "").strip()
    if not text:
        raise ValueError("the model's reply was empty, so there was no code to save")

    wanted = _label(lang or "")
    blocks = [(label, body) for label, body in _fenced_blocks(text) if body.strip()]

    if blocks:
        if wanted:
            usable = [(label, body) for label, body in blocks if _same_language(label, wanted)]
            # An unlabelled fence cannot contradict the caller; accept it, but
            # only if it really is code for the language they asked for.
            usable += [(label, body) for label, body in blocks if not label]
            if not usable:
                found = ", ".join(sorted({label or "unlabelled" for label, _ in blocks}))
                raise ValueError(f"the reply had no {wanted} block (found: {found})")
            label, body = usable[0]
            if not label and wanted in _PYTHON_LABELS and not _is_python(body):
                raise ValueError("the reply's unlabelled block is not valid Python")
            return _tidy(body), label or wanted

        # Prefer a labelled fence; an unlabelled one is the fallback.
        labelled = [(label, body) for label, body in blocks if label]
        label, body = labelled[0] if labelled else blocks[0]
        return _tidy(body), label

    # No fences at all: models routinely reply with bare code. Accept it only
    # when it is unmistakably a program, so a refusal is never saved as code.
    if wanted and wanted not in _PYTHON_LABELS:
        return _tidy(text), wanted
    if _is_python(text):
        return _tidy(text), "python"
    raise ValueError(
        "the reply had no fenced code block and is not Python: " + _excerpt(text)
    )


def _filename(name: str, label: str) -> str:
    """Build the file name: fixed stem, extension from the reply's language."""
    stem = re.sub(r"[^A-Za-z0-9._-]+", "_", os.path.basename(str(name or "").strip()))
    stem = stem.strip("._-") or DEFAULT_NAME
    if re.search(r"\.[A-Za-z0-9]{1,4}$", stem):  # caller already gave a suffix
        return stem
    ext = _extension(label)
    return f"{stem}.{ext}" if ext else stem


def extract_code(reply: str, lang: str | None = None) -> str:
    """Return just the code from a model reply, with the prose removed.

    Picks the first fenced block (the one matching ``lang`` when given) and
    returns it verbatim; an unlabelled or unfenced reply is accepted when it is
    a valid program. Raises :class:`ValueError` when the reply holds no code.
    """
    return _extract(reply, lang)[0]


def save_code(
    reply: str,
    name: str = DEFAULT_NAME,
    lang: str | None = None,
    directory: str = DEFAULT_DIRECTORY,
    quiet: bool = False,
) -> str:
    """Write the code from a model reply to ``directory`` and return its path.

    The suffix follows the language the reply was fenced as (``generated.py``,
    ``generated.js``, ``generated.sh``), or is omitted when the model named no
    language. With Ephemeral's ``chain`` flag the file is copied into the next
    container's working directory, so that block can run it directly.
    """
    code, label = _extract(reply, lang)
    filename = _filename(name, label)
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(code)

    if not quiet:
        lines = max(1, code.count("\n"))
        kind = label or "code"
        print(f"[ephemeral-llm] saved {path} ({lines} lines, {kind})")
        if _MAIN_GUARD.search(code):
            # runpy defaults to run_name="<run_path>", which would skip the
            # guard again; the run_name is what actually makes this work.
            print(
                f"[ephemeral-llm] note: it guards its entry point with "
                f'`if __name__ == "__main__"`, which `import '
                f"{filename.rsplit('.', 1)[0]}` never satisfies — "
                f'use: import runpy; '
                f'runpy.run_path({filename!r}, run_name="__main__")'
            )
    return path


def ask_code(
    prompt: str,
    name: str = DEFAULT_NAME,
    lang: str | None = None,
    system: str | None = None,
    directory: str = DEFAULT_DIRECTORY,
    quiet: bool = False,
    **kwargs,
) -> str:
    """Ask the model for a program, save it to ``directory``, return the path.

    Same flow as ``save_code(chat(prompt))``, with a system prompt that asks for
    one labelled code block so the language (and therefore the suffix) is
    unambiguous. ``kwargs`` go to :func:`chat`.
    """
    reply = chat(prompt, system=system or _CODE_SYSTEM, **kwargs)
    return save_code(reply, name=name, lang=lang, directory=directory, quiet=quiet)
