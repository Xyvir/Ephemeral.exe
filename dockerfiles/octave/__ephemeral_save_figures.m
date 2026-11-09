function __ephemeral_save_figures ()
  ## Write every figure still open when the block exits into /output as a PNG,
  ## so a plotting block returns artifacts the way a MATLAB script is written:
  ## `rlocus (G)` and the plot comes back, with no print () call.
  ##
  ## Registered by the image's site-wide startup file — see
  ## dockerfiles/ephemeral-octave-forge.dockerfile.
  ##
  ## Nothing in here may raise: an exit hook that errors would replace the
  ## block's own result (or error) with a confusing one, so each save and the
  ## whole walk are guarded.
  try
    if (exist ("/output", "dir") != 7)
      return;
    end

    figures = get (0, "children");
    for k = 1:numel (figures)
      handle = figures(k);
      if (! strcmp (get (handle, "type"), "figure"))
        continue;
      end
      ## A `figure ()` with nothing drawn on it is not a plot: saving it would
      ## hand back a blank page. Legend-only figures keep an axes child, so
      ## they are still caught.
      if (isempty (get (handle, "children")))
        continue;
      end

      ## Never clobber a name the block wrote itself — an explicit
      ## print (f, "/output/figure-1.png", "-dpng") means the student already
      ## chose that file. Take the first free index instead.
      index = 1;
      target = sprintf ("/output/figure-%d.png", index);
      while (exist (target, "file"))
        index = index + 1;
        target = sprintf ("/output/figure-%d.png", index);
      end

      try
        print (handle, target, "-dpng");
      catch
      end
    end
  catch
  end
end
