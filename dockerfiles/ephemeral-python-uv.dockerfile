# Ephemeral Polyglot Python Runner (uv + C-libraries for 90%+ common packages)
# Builds/publishes as tymills620/ephemeral-python-uv:latest (see
# .github/workflows/publish-images.yml).
FROM ghcr.io/astral-sh/uv:python3.12-bookworm-slim
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
# --- PDF & Document Generation (WeasyPrint, pdf2image, ReportLab, lxml) ---
libpango-1.0-0 \
libpangoft2-1.0-0 \
libcairo2 \
libgdk-pixbuf-2.0-0 \
libffi-dev \
shared-mime-info \
poppler-utils \
libxml2 \
libxslt1.1 \
# --- Vision & Graphics (OpenCV, Pillow, Matplotlib) ---
libgl1-mesa-glx \
libglib2.0-0 \
libsm6 \
libxext6 \
libxrender1 \
# --- Audio & Video Handling (Pydub, MoviePy, ffmpeg-python) ---
ffmpeg \
libsndfile1 \
# --- Database Client Libraries (psycopg2, mysqlclient, sqlite) ---
libpq5 \
libmariadb3 \
sqlite3 \
# --- Cryptography, Networking & Git (GitPython, Paramiko) ---
ca-certificates \
curl \
git \
openssl \
# --- System Fonts (Renders clean text in PDFs, SVGs & Charts) ---
fonts-dejavu-core \
fonts-liberation \
fontconfig \
# Clean up apt cache to keep image minimal
&& rm -rf /var/lib/apt/lists/*
WORKDIR /tmp
