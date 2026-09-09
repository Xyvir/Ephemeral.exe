# Ephemeral Octave Forge — vanilla Octave plus the control and signal
# packages pre-installed and auto-loaded at startup.
# Builds/publishes as tymills620/octave-forge:latest (see
# .github/workflows/publish-images.yml).
FROM docker.io/gnuoctave/octave:latest
# Install control and signal packages from Octave Forge
RUN octave --eval "pkg install -forge control; pkg install -forge signal;"
# Automatically load both packages into memory on startup
RUN echo "pkg load control signal" >> /root/.octaverc
