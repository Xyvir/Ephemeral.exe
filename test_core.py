"""Smoke tests for ephemeral_core parser and models."""
import os

from ephemeral_core.parser import (
    parse_codeblocks,
    resolve_runtime_config,
    infer_python_dependencies,
    extract_declared_dependencies,
    inject_python_dependency_metadata,
    prepare_python_block,
)
from ephemeral_core.models import ExecutionResult, GroupResult, BlockResult

# --- Test 1: Basic fenced codeblock ---
md = "```python\nprint(42)\n```"
blocks = parse_codeblocks(md)
assert len(blocks) == 1, f"Expected 1 block, got {len(blocks)}"
assert blocks[0]['type'] == 'code'
assert blocks[0]['config']['image'] == 'docker.io/tymills620/ephemeral-python-uv:latest'
assert blocks[0]['config']['cmd'] == ['uv', 'run', '-']
assert blocks[0]['config']['allow_network'] == False
print("PASS: Basic fenced codeblock")

# --- Test 2: Network flag (unsafe) ---
md2 = "```node unsafe\nconsole.log(1)\n```"
blocks2 = parse_codeblocks(md2)
assert blocks2[0]['config']['allow_network'] == True
assert blocks2[0]['config']['image'] == 'docker.io/library/node:18-alpine'
print("PASS: Network flag (unsafe)")

# --- Test 3: No-chain flag ---
md3 = "```python nopipe\nprint(1)\n```"
blocks3 = parse_codeblocks(md3)
assert blocks3[0]['config']['allow_chain'] == False
print("PASS: No-chain flag (nopipe)")

# --- Test 4: Version override ---
md4 = "```python:3.11\nprint(1)\n```"
blocks4 = parse_codeblocks(md4)
assert blocks4[0]['config']['image'] == 'docker.io/tymills620/ephemeral-python-uv:3.11'
print("PASS: Version override (python:3.11)")

# --- Test 5: Alias resolution ---
md5 = "```js\nconsole.log(1)\n```"
blocks5 = parse_codeblocks(md5)
assert blocks5[0]['config']['image'] == 'docker.io/library/node:18-alpine'
print("PASS: Alias resolution (js -> node)")

# --- Test 6: Shebang inside fenced block overwrites header ---
md6 = "```bash\n#! python\nprint(1)\n```"
blocks6 = parse_codeblocks(md6)
assert blocks6[0]['config']['image'] == 'docker.io/tymills620/ephemeral-python-uv:latest'
print("PASS: Shebang overwrites fenced header")

# --- Test 7: Seed file detection ---
md7 = "```data.csv\na,b,c\n1,2,3\n```"
blocks7 = parse_codeblocks(md7)
assert blocks7[0]['type'] == 'seed'
assert blocks7[0]['name'] == 'data.csv'
print("PASS: Seed file detection")

# --- Test 8: Seed file with b64 flag ---
md8 = "```image.png b64\naGVsbG8=\n```"
blocks8 = parse_codeblocks(md8)
assert blocks8[0]['type'] == 'seed'
assert blocks8[0]['is_b64'] == True
print("PASS: Seed file with b64 flag")

# --- Test 9: Multiple blocks with different languages ---
md9 = "```python\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"
blocks9 = parse_codeblocks(md9)
assert len(blocks9) == 2
assert blocks9[0]['config']['image'] == 'docker.io/tymills620/ephemeral-python-uv:latest'
assert blocks9[1]['config']['image'] == 'docker.io/library/node:18-alpine'
print("PASS: Multiple blocks with different languages")

# --- Test 10: Shebang-only (no fences) ---
md10 = "#! python\nprint('hello')\n"
blocks10 = parse_codeblocks(md10)
assert len(blocks10) == 1
assert blocks10[0]['config']['image'] == 'docker.io/tymills620/ephemeral-python-uv:latest'
print("PASS: Shebang-only (no fences)")

# --- Test 11: Empty input ---
blocks_empty = parse_codeblocks("")
assert blocks_empty == []
print("PASS: Empty input")

# --- Test 12: Quad-backtick stripping ---
md12 = "````markdown\n```python\nprint(1)\n```\n````"
blocks12 = parse_codeblocks(md12)
assert len(blocks12) == 1
assert blocks12[0]['config']['image'] == 'docker.io/tymills620/ephemeral-python-uv:latest'
print("PASS: Quad-backtick stripping")

# --- Test 13: Dataclass construction ---
result = ExecutionResult(
    stdout="hello", stderr="", exit_code=0,
    artifact_paths=["/tmp/out.txt"], artifact_dir="/tmp"
)
assert result.exit_code == 0
assert result.artifact_paths == ["/tmp/out.txt"]
print("PASS: ExecutionResult dataclass")

# --- Test 14: resolve_runtime_config with custom image override ---
cfg = resolve_runtime_config("python image=myrepo/mypython:latest")
assert cfg['image'] == 'myrepo/mypython:latest'
assert cfg['cmd'] == ['uv', 'run', '-']
print("PASS: Custom image override")

# --- Test 15: Entrypoint override ---
cfg2 = resolve_runtime_config("tiddlywiki")
assert cfg2['entrypoint'] == ''
print("PASS: Entrypoint from LANG_MAP")

# --- Test 16: Infer third-party deps from imports ---
code16 = """import requests
from numpy import array
import pandas as pd
import os, sys, json
from collections import OrderedDict
import matplotlib.pyplot as plt
"""
deps16 = infer_python_dependencies(code16)
assert deps16 == ['matplotlib', 'numpy', 'pandas', 'requests'], f"Unexpected deps: {deps16}"
assert infer_python_dependencies("print(1)\nimport math\n") == []
print("PASS: Import-based dependency inference")

# --- Test 17: Dotted and multi-line imports ---
code17 = """from sklearn.ensemble import (
    RandomForestClassifier,
    GradientBoosting,
)
import urllib.request
"""
assert infer_python_dependencies(code17) == ['sklearn']
assert infer_python_dependencies("from . import helper\nfrom ..pkg import thing\n") == []
print("PASS: Dotted/multi-line import handling")

# --- Test 18: PEP 723 metadata respected (no double injection) ---
code18 = """# /// script
# dependencies = ["requests<3", "rich"]
# ///
import requests
"""
assert extract_declared_dependencies(code18) == ['requests<3', 'rich']
assert inject_python_dependency_metadata(code18, ['numpy']) == code18
print("PASS: Existing PEP 723 metadata respected")

# --- Test 19: Multi-line declared dependencies preserved ---
code19 = """# /// script
# dependencies = [
#     "aiohttp",
#     "pydantic>=2",
# ]
# ///
import aiohttp
"""
assert extract_declared_dependencies(code19) == ['aiohttp', 'pydantic>=2']
print("PASS: Multi-line declared dependencies")

# --- Test 20: Header injection preserves shebang ---
code20 = "#! python\nimport requests\nprint(1)\n"
injected20 = inject_python_dependency_metadata(code20, ['requests'])
assert injected20.startswith("#! python\n# /// script\n"), injected20
print("PASS: Header injection after shebang")

# --- Test 21: prepare_python_block returns deps + injected content ---
block21 = {'type': 'code', 'header': 'python', 'content': 'import numpy\nprint(1)\n', 'config': {}}
prepared21, deps21 = prepare_python_block(block21)
assert deps21 == ['numpy']
assert prepared21['content'].startswith("# /// script\n")
assert block21['content'] == 'import numpy\nprint(1)\n'  # original untouched
print("PASS: prepare_python_block")

# --- Test 22: language-map image set (super-seed hydration) ---
# The hydrate set must equal the receiver-side allowlist exactly, or a
# "super-seed" node could be missing an image remote jobs may request.
from ephemeral_core.config import LANG_MAP, mapped_images

imgs = mapped_images()
assert len(imgs) > 30, f"expected a rich language map, got {len(imgs)} images"
assert len(imgs) == len(set(imgs)), "mapped_images must dedupe (gcc backs c/cpp/fortran)"
assert "docker.io/tymills620/ephemeral-python-uv:latest" in imgs
assert "docker.io/library/node:18-alpine" in imgs
assert "docker.io/library/gcc:latest" in imgs
# SQL: the image must have a shell (executor appends `sh`) and the aliases
# must point at the real entry, or ```sql/sqlite3 silently resolves to nothing.
assert "docker.io/keinos/sqlite3:latest" in imgs
assert LANG_MAP["sqlite"] == "sql" and LANG_MAP["sqlite3"] == "sql"
assert LANG_MAP["sql"]["cmd"][0] == "sh"

from ephemeral_net.sandbox import default_image_allowlist

assert set(imgs) == set(default_image_allowlist()), \
    "hydrate set must match the receiver-side allowlist"
print(f"PASS: language-map image set ({len(imgs)} unique images == allowlist)")

# --- Test 22b: local-LLM monolith wiring ---
# The image ENTRYPOINT boots llama.cpp before user code, so the language entry
# must NOT override the entrypoint (tiddlywiki-style `entrypoint: ''` would
# replace the boot script with plain sh) and must not use `uv run` (the
# two-stage dependency path re-invokes the payload as /deps/venv/bin/python -,
# skipping the boot step). It also must stay usable under --network none.
llm_cfg = resolve_runtime_config("llm")
assert llm_cfg["image"] == "docker.io/tymills620/ephemeral-qwen-monolith:latest"
assert llm_cfg.get("entrypoint") is None, "llm must inherit the image ENTRYPOINT"
assert llm_cfg["cmd"] == ["python", "-"]
assert llm_cfg["allow_network"] is False
assert llm_cfg["allow_chain"] is False
assert llm_cfg["image"] in imgs
assert LANG_MAP["qwen"] == "llm" and LANG_MAP["local-llm"] == "llm"
assert resolve_runtime_config("qwen")["image"] == llm_cfg["image"]
print("PASS: local-LLM monolith wiring")

# --- Test 23: Chaining is OFF by default ---
md23 = "```python\nprint(1)\n```"
blocks23 = parse_codeblocks(md23)
assert blocks23[0]['config']['allow_chain'] == False
print("PASS: Chaining off by default (no flag)")

# --- Test 24: Chaining opt-in flags (chain / piping / pipe) ---
for flag in ('chain', 'piping', 'pipe'):
    cfg = resolve_runtime_config(f"python {flag}")
    assert cfg['allow_chain'] == True, f"{flag} should enable chaining"
print("PASS: chain / piping / pipe opt in to chaining")

# --- Test 25: nopipe still overrides chain ---
cfg = resolve_runtime_config("python chain nopipe")
assert cfg['allow_chain'] == False
print("PASS: nopipe overrides chain")

# --- Test 26: Run grouping + chained detection ---
from ephemeral_core.executor import group_into_runs, request_is_chained, MAX_PARALLEL_RUNS

md26 = "```python\nprint(1)\n```\n\n```node\nconsole.log(2)\n```\n\n```python\nprint(3)\n```"
runs26 = group_into_runs(parse_codeblocks(md26))
assert len(runs26) == 3, f"3 different runs expected, got {len(runs26)}"
assert not request_is_chained(runs26)

# Same-config blocks merge into one run; chaining declared anywhere -> True
md26b = "```python\nprint(1)\n```\n\n```python\nprint(2)\n```"
assert len(group_into_runs(parse_codeblocks(md26b))) == 1
md26c = "```python chain\nprint(1)\n```\n\n```node\nconsole.log(2)\n```"
assert request_is_chained(group_into_runs(parse_codeblocks(md26c)))
assert MAX_PARALLEL_RUNS == 4, "local parallel guardrail must be 4"
print("PASS: Run grouping + chained detection")

# --- Test 27: Container resource limits scale to small hosts ---
from ephemeral_core import executor as executor_mod

_saved_host_mib = executor_mod._host_memory_mib
_saved_limits_env = {k: os.environ.pop(k, None) for k in (
    "EPHEMERAL_MEMORY_LIMIT", "EPHEMERAL_CPU_LIMIT", "EPHEMERAL_PIDS_LIMIT")}
try:
    # 1 GiB host -> ~half of RAM (min 256 MiB, 64 MiB steps), 1 cpu, 256 pids
    executor_mod._host_memory_mib = lambda: 1024
    assert executor_mod._container_resource_limits() == \
        ['--memory', '512m', '--cpus', '1', '--pids-limit', '256'], \
        executor_mod._container_resource_limits()
    # 2 GiB host -> 1024m ceiling
    executor_mod._host_memory_mib = lambda: 2048
    assert executor_mod._container_resource_limits()[1] == '1024m'
    # Tiny 512 MiB host -> clamped to the 256 MiB floor
    executor_mod._host_memory_mib = lambda: 512
    assert executor_mod._container_resource_limits() == \
        ['--memory', '256m', '--cpus', '1', '--pids-limit', '256']
    # Unknown memory (macOS/Windows) and large hosts keep the historical defaults
    executor_mod._host_memory_mib = lambda: None
    assert executor_mod._container_resource_limits() == \
        ['--memory', '2g', '--cpus', '2', '--pids-limit', '512']
    executor_mod._host_memory_mib = lambda: 8192
    assert executor_mod._container_resource_limits() == \
        ['--memory', '2g', '--cpus', '2', '--pids-limit', '512']
    # Explicit env overrides win on any host
    os.environ["EPHEMERAL_MEMORY_LIMIT"] = "700m"
    os.environ["EPHEMERAL_CPU_LIMIT"] = "1.0"
    os.environ["EPHEMERAL_PIDS_LIMIT"] = "128"
    executor_mod._host_memory_mib = lambda: 1024
    assert executor_mod._container_resource_limits() == \
        ['--memory', '700m', '--cpus', '1.0', '--pids-limit', '128']
finally:
    executor_mod._host_memory_mib = _saved_host_mib
    for k, v in _saved_limits_env.items():
        if v is not None:
            os.environ[k] = v
print("PASS: Container resource limits scale to host RAM (small-VPS safe)")

# --- Test 28: native image architecture checks --------------------------
# A foreign-architecture image must not be considered warm. Multi-platform
# tags are resolved by the registry/Podman pull for the local architecture;
# the local cache check only accepts the architecture actually installed.
_saved_machine = executor_mod.platform.machine
_saved_check_call = executor_mod.subprocess.check_call
_saved_check_output = executor_mod.subprocess.check_output
try:
    executor_mod.platform.machine = lambda: "aarch64"
    assert executor_mod.host_arch() == "arm64"

    executor_mod.subprocess.check_output = lambda *args, **kwargs: b"amd64\n"
    assert not executor_mod.image_is_compatible("octave")
    executor_mod.subprocess.check_output = lambda *args, **kwargs: b"arm64\n"
    assert executor_mod.image_is_compatible("octave")

    executor_mod.subprocess.check_call = lambda *args, **kwargs: 0
    assert executor_mod.check_image_exists("octave")

    inventory = [
        {"Architecture": "amd64", "Names": ["amd-image"]},
        {"Architecture": "arm64", "Names": ["arm-image"]},
    ]
    executor_mod.subprocess.check_output = lambda *args, **kwargs: __import__("json").dumps(inventory).encode()
    assert executor_mod.list_local_images() == ["arm-image"]
finally:
    executor_mod.platform.machine = _saved_machine
    executor_mod.subprocess.check_call = _saved_check_call
    executor_mod.subprocess.check_output = _saved_check_output
print("PASS: foreign cached images are excluded from native warm-image routing")

# --- Test 29: result fences outgrow the payload's own backticks -----------
# Block output is arbitrary text, and a model reply routinely contains its own
# ``` fences. A fixed three-backtick wrapper ends at the first of those, so the
# rest of the result spills out of its panel and renders as stray markdown.
# CommonMark closes a fence only with a run at least as long, so the wrapper
# must be one backtick longer than the longest run inside the payload.
import re
from ephemeral_core.executor import _md_fence

assert _md_fence("42") == "```text\n42\n```", _md_fence("42")
assert _md_fence("a ``` b") == "````text\na ``` b\n````", _md_fence("a ``` b")
assert _md_fence("`````") == "``````text\n`````\n``````", _md_fence("`````")


def _scan_one(text):
    """CommonMark-style scan: the body of the single outermost fence."""
    lines = text.split("\n")
    opened = re.match(r"^(`{3,})", lines[0])
    assert opened, lines[0]
    width = len(opened.group(1))
    closers = [i for i, line in enumerate(lines[1:], 1) if re.fullmatch(rf"`{{{width},}}", line)]
    assert len(closers) == 1, closers
    return "\n".join(lines[1:closers[0]])


for payload in ["plain", "```python\nprint(1)\n```", "````css\nx\n````", "inline `code` only"]:
    assert _scan_one(_md_fence(payload)) == payload, payload

# The envelope the executor actually returns for a reply that contains a
# fenced block: four backticks around the whole payload, so the inner ```python
# cannot close it.
import tempfile
from ephemeral_core.executor import _format_success_result

_out_dir = tempfile.mkdtemp()
_envelope = _format_success_result(
    "MARK\n```python\nprint(1)\n```\n", "", [(0, "MARK", "llm")],
    "llm", 1, 1, True, _out_dir, False,
).stdout_formatted
assert _envelope.startswith("## Llm Result\n\n````text\n"), repr(_envelope)
assert _envelope.rstrip("\n").endswith("\n````"), repr(_envelope)
assert _scan_one(_envelope.split("\n", 2)[2].rstrip("\n")) == "```python\nprint(1)\n```", repr(_envelope)

# The "this is already a result" safety guard must still recognise a widened
# fence, or a pasted result would be executed a second time.
assert re.search(r"^## (Run|Result) .*[\r\n]+`{3,}text", "## Run 1\n" + _md_fence("```"), re.MULTILINE)
print("PASS: result fences outgrow the payload's own backticks")

# --- Test 30: llm blocks are chat boxes that stay Python -------------------
# Bare prose becomes print(chat("...")) and `chat`/`client` are injected, so
# an llm block needs no boilerplate. Real Python still runs as Python, blocks
# that import the helper themselves are untouched, and nothing here touches a
# non-monolith image (plain `python` blocks keep their own behaviour).
import json
from ephemeral_core.parser import (
    is_local_llm_config,
    prepare_llm_block,
    reads_as_python,
)


def _llm(content):
    return prepare_llm_block({"type": "code", "header": "llm", "content": content, "config": {}})["content"]


prompt = "Can you write a small python script to calculate the first 10 primes?"
assert _llm(prompt) == (
    "from ephemeral_llm import chat, client\n"
    'print(chat("Can you write a small python script to calculate the first 10 primes?"))\n'
), _llm(prompt)

# Multi-line prose, quotes and newlines arrive intact in one valid literal.
prose = 'Explain "groupby" in\none sentence.'
wrapped = _llm(prose)
assert wrapped.startswith("from ephemeral_llm import chat, client\nprint(chat("), wrapped
assert json.loads(wrapped.split("print(chat(", 1)[1].rsplit("))\n", 1)[0]) == prose

# Code keeps running as code, with the names injected.
code30 = 'df = pd.DataFrame({"n": [1]})\nprint(chat("sum?", max_tokens=8))'
assert _llm(code30) == "from ephemeral_llm import chat, client\n" + code30

# Explicit imports are respected (no duplicate preamble), and blank is a no-op.
own = 'from ephemeral_llm import chat\nprint(chat("hi"))'
assert _llm(own) == own
assert _llm("   \n") == "   \n"

# Prose vs program: the heuristic that decides which branch a block takes.
for source in (prompt, "hello", "summarise this please:"):
    assert not reads_as_python(source), source
for source in ('print("hi")', "1 + 1", "x = 1", "import pandas as pd", "def f():\n    return 1"):
    assert reads_as_python(source), source

# Image gating: only the monolith gets the rewrite.
assert is_local_llm_config({"image": "docker.io/tymills620/ephemeral-qwen-monolith:latest"})
assert not is_local_llm_config({"image": "docker.io/tymills620/ephemeral-python-uv:latest"})
assert not is_local_llm_config({})

# Idempotent: a re-prepared (already rewritten) block is left alone.
once = _llm(prompt)
assert prepare_llm_block({"type": "code", "content": once})["content"] == once
print("PASS: llm bare prompts and implicit chat()/client()")

# --- Test 31: generated code is saved for the next container --------------
# The image's helper strips the prose off a model reply and writes the code to
# /output, where `chain` stages it into the following container. The suffix
# follows whatever language the model fenced (not Python specifically), so the
# next block can import/require/run it in its own language.
import importlib.util
import io
import pathlib
import sys
import tempfile
import types
from contextlib import redirect_stdout

ROOT = pathlib.Path(__file__).resolve().parent
LLM_MODULE = ROOT / "dockerfiles" / "ephemeral-qwen-monolith" / "ephemeral_llm.py"

try:  # the image's module imports the OpenAI SDK; stub it when absent
    import openai  # noqa: F401
    _openai_stub = False
except ImportError:
    _stub = types.ModuleType("openai")

    class _OpenAI:
        def __init__(self, **kwargs):
            self.kwargs = kwargs

    _stub.OpenAI = _OpenAI
    sys.modules["openai"] = _stub
    _openai_stub = True

_spec = importlib.util.spec_from_file_location("ephemeral_llm_under_test", LLM_MODULE)
llm = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(llm)

reply = (
    "Certainly! Here's a small Python script:\n\n"
    "```python\n"
    "def is_prime(n):\n"
    "    return n > 1 and all(n % i for i in range(2, int(n ** 0.5) + 1))\n"
    "\n"
    "print([n for n in range(2, 30) if is_prime(n)][:10])\n"
    "```\n\n"
    "Run it anywhere.\n"
)
code31 = llm.extract_code(reply)
assert code31.startswith("def is_prime"), code31
assert "Certainly" not in code31 and "Run it anywhere" not in code31
assert code31.endswith("\n") and not code31.endswith("\n\n")

# Code-agnostic: the fence label, not Python, decides the file's suffix.
assert llm.extract_code("Here you go:\n\n```js\nconsole.log('hi');\n```\n") == "console.log('hi');\n"
assert llm._filename("generated", "javascript") == "generated.js"
assert llm._filename("generated", "bash") == "generated.sh"
assert llm._filename("generated", "") == "generated"
assert llm._filename("generated", "piet") == "generated"  # unknown label, no suffix
assert llm._filename("primes.py", "python") == "primes.py"
assert llm._filename("../../etc/passwd", "python") == "passwd.py"

# Fence selection: first labelled block by default, the requested one on ask.
mixed = (
    "First the shell step:\n\n```bash\necho hi\n```\n\n"
    "Then the Python:\n\n```python\nprint(1)\n```\n"
)
assert llm.extract_code(mixed) == "echo hi\n"
assert llm.extract_code(mixed, lang="python") == "print(1)\n"
assert llm.extract_code(mixed, lang="py") == "print(1)\n"  # alias
try:
    llm.extract_code(mixed, lang="ruby")
    raise AssertionError("a missing language must not silently save another")
except ValueError as exc31:
    assert "bash" in str(exc31) and "python" in str(exc31), str(exc31)

# A wider fence keeps an inner ```python``` intact as code.
assert llm.extract_code("Look:\n\n````markdown\n```python\nprint(1)\n```\n````\n") == (
    "```python\nprint(1)\n```\n"
)
assert llm.extract_code("~~~{.js}\nlet x = 1;\n~~~") == "let x = 1;\n"

# Unlabelled fences follow the caller's language — but only if they're code.
assert llm.extract_code("```\nprint(2)\n```", lang="python") == "print(2)\n"
try:
    llm.extract_code("```\nnot python at all here.\n```", lang="python")
    raise AssertionError("an unlabelled prose block must not become Python")
except ValueError:
    pass

# Unfenced replies: bare code is accepted, prose is not.
assert llm.extract_code("print(3)\n") == "print(3)\n"
try:
    llm.extract_code("I can't help with that.")
    raise AssertionError("a prose-only reply must not be written as code")
except ValueError as exc31:
    assert "can't help" in str(exc31), str(exc31)

with tempfile.TemporaryDirectory() as tmp31:
    path31 = llm.save_code(reply, directory=tmp31)
    assert os.path.basename(path31) == "generated.py", path31
    with open(path31, encoding="utf-8") as fh31:
        assert fh31.read() == code31  # written verbatim

    printed31 = io.StringIO()
    with redirect_stdout(printed31):
        llm.save_code("```bash\necho hi\n```", directory=tmp31)
    assert "generated.sh" in printed31.getvalue(), printed31.getvalue()

    # Created on demand, and an unknown language still lands (no suffix).
    nested31 = os.path.join(tmp31, "nested")
    assert llm.save_code("```piet\nnope\n```", directory=nested31, quiet=True).endswith("generated")

    # A __main__ guard doesn't fire on import — point at runpy instead.
    printed31 = io.StringIO()
    with redirect_stdout(printed31):
        llm.save_code("```python\nif __name__ == '__main__':\n    print(4)\n```", directory=tmp31)    # runpy without run_name="__main__" would skip the guard as well.
    assert "runpy.run_path" in printed31.getvalue(), printed31.getvalue()
    assert 'run_name="__main__"' in printed31.getvalue(), printed31.getvalue()

    # ask_code = chat + save in one call.
    llm.chat = lambda *a, **k: "```python\nprint(5)\n```"
    asked31 = llm.ask_code("write a script", directory=tmp31, quiet=True)
    with open(asked31, encoding="utf-8") as fh31:
        assert fh31.read() == "print(5)\n"

if _openai_stub:
    del sys.modules["openai"]
print("PASS: model replies become runnable, code-agnostic files")

# --- Test 32: locally staged modules are not PyPI requirements ------------
# Seeds and chained /output files are written into the workdir before the code
# runs, so `import generated` (or `import helpers` from a seeded file) must
# resolve locally instead of sending uv to the package registry.
from ephemeral_core.executor import _local_module_names

assert _local_module_names([]) == set()
seeds32 = [
    {"type": "seed", "name": "generated.py"},
    {"type": "seed", "name": "pkg/__init__.py"},
    {"type": "seed", "name": "data.json"},
    {"type": "seed", "name": "lib\\util.py"},  # windows-style separator
    {"type": "code", "name": "ignored.py"},
]
local32 = _local_module_names(seeds32)
assert local32 == {"generated", "pkg", "util"}, local32

assert infer_python_dependencies("import generated\nimport pandas as pd") == ["generated", "pandas"]
assert infer_python_dependencies("import generated\nimport pandas as pd", local32) == ["pandas"]
assert infer_python_dependencies("from generated import first_n_primes", {"generated"}) == []

# No PEP 723 header is injected for a locally staged module.
staged32, deps32 = prepare_python_block(
    {"type": "code", "content": "import generated\nprint(generated.__name__)"}, local_modules=local32
)
assert deps32 == [], deps32
assert staged32["content"] == "import generated\nprint(generated.__name__)"
print("PASS: staged modules (seeds, chained generated code) aren't PyPI deps")

# --- Test 33: every run gets a time cap, `notimeout` lifts it locally -----
# A caller that passes no timeout (the tray's local path) used to get an
# unbounded run, so one `while True` could hold the host forever. The core now
# applies DEFAULT_TIMEOUT; only a local header can opt out, via a keyword the
# swarm strips (see test_net's sanitizer test).
from ephemeral_core.config import DEFAULT_TIMEOUT, NO_TIMEOUT_FLAGS
from ephemeral_core.executor import effective_timeout

assert DEFAULT_TIMEOUT == 300, DEFAULT_TIMEOUT
assert resolve_runtime_config("python")["ignore_timeout"] is False
for flag33 in sorted(NO_TIMEOUT_FLAGS):
    cfg33 = resolve_runtime_config(f"python {flag33}")
    assert cfg33["ignore_timeout"] is True, flag33
    # The keyword is consumed, not passed through as a language or an override.
    assert cfg33["image"].endswith("ephemeral-python-uv:latest"), cfg33
    assert cfg33["cmd"] == ["uv", "run", "-"], cfg33

# The cap fills in only where the caller left it open; explicit wins.
assert effective_timeout({}, None) == DEFAULT_TIMEOUT
assert effective_timeout({"ignore_timeout": False}, None) == DEFAULT_TIMEOUT
assert effective_timeout({"ignore_timeout": True}, None) is None
assert effective_timeout({"ignore_timeout": True}, 5) == 5
assert effective_timeout({"ignore_timeout": False}, 60) == 60
print("PASS: default time cap, with a local-only notimeout opt-out")

print("\n=== ALL 33 TESTS PASSED ===")
