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

from ephemeral_net.sandbox import default_image_allowlist

assert set(imgs) == set(default_image_allowlist()), \
    "hydrate set must match the receiver-side allowlist"
print(f"PASS: language-map image set ({len(imgs)} unique images == allowlist)")

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

print("\n=== ALL 28 TESTS PASSED ===")
