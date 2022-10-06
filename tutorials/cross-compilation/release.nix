# release.nix
let
  myProject = import ./default.nix;

  pkgsNative = import <nixpkgs> {};
  pkgsRaspberryPi = import <nixpkgs> {
    crossSystem = pkgsNative.lib.systems.examples.raspberryPi;
  };

  native = myProject { pkgs = pkgsNative; };
  crossRaspberryPi = myProject { pkgs = pkgsRaspberryPi; };

in {
  my-project-native = native.my-project.components.exes.my-project;
  my-project-raspberry-pi = crossRaspberryPi.my-project.components.exes.my-project;
}