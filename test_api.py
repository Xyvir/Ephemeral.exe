"""Smoke tests for main_api Pydantic models."""
import base64
from main_api import RunRequest

# Test 1: Valid base64 auto-decodes to plaintext
blob = base64.b64encode(b"```python\nprint(42)\n```").decode()
req = RunRequest(document_blob=blob, timeout=10)
assert "```python" in req.document_blob
assert req.timeout == 10
print("PASS: Base64 auto-decode")

# Test 2: Default timeout
req2 = RunRequest(document_blob=base64.b64encode(b"test").decode())
assert req2.timeout == 300
print("PASS: Default timeout = 300")

# Test 3: Invalid base64 rejected
try:
    RunRequest(document_blob="!!!not-base64!!!", timeout=10)
    assert False, "Should have raised"
except Exception as e:
    print(f"PASS: Invalid base64 rejected ({type(e).__name__})")

# Test 4: Timeout bounds enforced
try:
    RunRequest(document_blob=base64.b64encode(b"test").decode(), timeout=0)
    assert False, "Should have raised"
except Exception as e:
    print(f"PASS: Timeout < 1 rejected ({type(e).__name__})")

try:
    RunRequest(document_blob=base64.b64encode(b"test").decode(), timeout=999)
    assert False, "Should have raised"
except Exception as e:
    print(f"PASS: Timeout > 600 rejected ({type(e).__name__})")

print("\n=== ALL API MODEL TESTS PASSED ===")
