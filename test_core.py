"""Smoke tests for ephemeral_core parser and models."""
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

print("\n=== ALL 22 TESTS PASSED ===")
