{ runCommand, nix-tools
, hackageIndex
} : runCommand "hackage-nix" {} ''
    HOME=${hackageIndex} ${nix-tools}/bin/hackage-to-nix $out
  ''
