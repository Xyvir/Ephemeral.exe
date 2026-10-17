"""Talk to the local model baked into this container.

The image runs llama.cpp's OpenAI-compatible server on loopback (12434) before
the snippet starts, so every ``llm`` block can just do::

    from ephemeral_llm import chat
    print(chat("Explain groupby in one sentence."))

For anything beyond a one-shot reply (streaming, tools, response formats,
embeddings via ``/v1/embeddings``), use :func:`client` and the normal OpenAI
SDK — this module is a convenience, not a wrapper. Everything is local: the
server never leaves the container, and the block still runs with the sandbox's
network removed.
"""

from __future__ import annotations

import os

from openai import OpenAI

MODEL = os.environ.get("EPHEMERAL_LLM_MODEL_NAME", "local")
BASE_URL = os.environ.get("EPHEMERAL_LLM_BASE_URL", "http://127.0.0.1:12434/v1")


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
