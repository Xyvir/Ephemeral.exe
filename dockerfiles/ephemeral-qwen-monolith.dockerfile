# Ephemeral Qwen Monolith (WORK IN PROGRESS — untested).
# llama.cpp server preloading a small Qwen2 instruct GGUF, intended for a
# future local-LLM language. Deliberately NOT wired into LANG_MAP yet; this
# exists so the image can be built/published by CI without any runtime
# behavior change to the cluster.
FROM ghcr.io/ggml-org/llama.cpp:server
ADD https://huggingface.co/Qwen/Qwen2-0.5B-Instruct-GGUF/resolve/main/qwen2-0_5b-instruct-q4_k_m.gguf /models/model.gguf
ENTRYPOINT ["/llama-server", "-m", "/models/model.gguf", "--port", "12434", "--host", "0.0.0.0"]
