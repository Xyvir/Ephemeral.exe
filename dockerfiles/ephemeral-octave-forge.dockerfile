# Ephemeral Octave Forge — vanilla Octave plus the control and signal
# packages pre-installed and auto-loaded at startup, with the MATLAB-parity
# pzplot shim and the figure-to-artifact hook described below.
# Builds/publishes as tymills620/octave-forge:latest (see
# .github/workflows/publish-images.yml).
FROM docker.io/gnuoctave/octave:latest
# Install control and signal packages from Octave Forge
RUN octave --eval "pkg install -forge control; pkg install -forge signal;"
# Load them from Octave's site-wide startup file, which runs before ~/.octaverc
# for every block whatever HOME the container is given (the old /root/.octaverc
# entry only applied while HOME was /root). Appended rather than replaced so
# the base image's own startup code survives.
#
# Figures stay invisible: there is no X display and gnuplot is the only toolkit
# installed, so nothing can open a window — the plot reaches the user as an
# artifact instead. The toolkit is deliberately NOT pinned here: gnuplot warns
# on stderr the first time it is used, and selecting it at startup would put
# that warning in every block's output, including blocks that never plot.
#
# There is no atexit hook here on purpose: Octave destroys figures before its
# atexit handlers run (measured on 11.3.0 — the handler sees zero figures), so
# the only moment a plot still exists is inside the block. The wrapper in
# ephemeral_core/config.py therefore sources the payload and calls
# __ephemeral_save_figures () before exiting.
RUN printf '%s\n' \
    'pkg load control' \
    'pkg load signal' \
    'set (0, "defaultfigurevisible", "off");' \
    >> /usr/share/octave/site/m/startup/octaverc
# MATLAB's pzplot does not exist in the control package; the shim maps it onto
# pzmap so an unmodified MATLAB script plots. The figure saver lives beside it,
# on the same path, so every block resolves both by name.
COPY dockerfiles/octave/pzplot.m /usr/share/octave/site/m/pzplot.m
COPY dockerfiles/octave/__ephemeral_save_figures.m /usr/share/octave/site/m/__ephemeral_save_figures.m
