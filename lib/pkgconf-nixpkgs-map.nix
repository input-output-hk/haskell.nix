# pkgconfig entries to nixpkgs map
pkgs: {
    cairo-pdf = pkgs.cairo;
    cairo-ps = pkgs.cairo;
    cairo-svg = pkgs.cairo;
    xft = pkgs.xorg.libXft;
    xau = pkgs.xorg.libXau;
    libR = pkgs.R;
    fftw3f = pkgs.fftwFloat;
    fftw3 = pkgs.fftw;
  };
