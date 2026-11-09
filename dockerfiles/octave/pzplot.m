function varargout = pzplot (varargin)
  ## MATLAB parity shim: the Octave Forge control package ships `pzmap` but not
  ## MATLAB's `pzplot` (the Octave wiki lists pzplot as unimplemented). It is
  ## the same pole-zero plot, so forward every argument and every output.
  [varargout{1:nargout}] = pzmap (varargin{:});
end
