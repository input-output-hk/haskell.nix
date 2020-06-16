{ n }:
# This test makes sure that adding materialize function call with no materialized files
# does not impact performance by reducing the ability of nix to perform concurrent
# builds.
let
  inherit (import ../../. {}) pkgs;
  hello = pkgs.haskell-nix.materialize { materialized = null; } (pkgs.runCommand "hello" {} ''
      echo ${n}
      echo EVENT start hello
      sleep 2
      echo EVENT end hello
      echo hello > $out
    '');
  world = pkgs.haskell-nix.materialize { materialized = null; } (pkgs.runCommand "world" {} ''
      echo ${n}
      echo EVENT start world
      sleep 3
      echo EVENT end world
      echo world > $out
    '');
  hello-world = pkgs.runCommand "hello-world" {} ''
      cat ${hello} > $out
      echo ' '
      cat ${world} > $out
    '';
in hello-world


