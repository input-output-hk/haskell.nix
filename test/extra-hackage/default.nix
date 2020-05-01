{ cabalProject', testSrc }:

let  
  
  hackage = import ./hackage;

  tarball = {
    name = "extra-hackage-demo";
    index = ./01-index.tar.gz;
  };

  demo-src = ./external-package-demo-0.1.0.0.tar.gz;

  project = cabalProject' {
    src = testSrc "extra-hackage/external-package-user";

    extra-hackages = [ hackage ];
    extra-hackage-tarballs = [ tarball ];

    modules = [
      # To prevent nix-build from trying to download it from the
      # actual Hackage.
      { packages.external-package-demo.src = demo-src; }
    ];
  };

in
  project
