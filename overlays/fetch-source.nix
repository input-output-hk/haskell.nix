final: prev:
let
  lockFile = builtins.fromJSON (builtins.readFile ../flake.lock);
  # Courtesy of `flake-compat`
  # Format number of seconds in the Unix epoch as %Y%m%d%H%M%S.
  formatSecondsSinceEpoch = t:
    let
      rem = x: y: x - x / y * y;
      days = t / 86400;
      secondsInDay = rem t 86400;
      hours = secondsInDay / 3600;
      minutes = (rem secondsInDay 3600) / 60;
      seconds = rem t 60;

      # Courtesy of https://stackoverflow.com/a/32158604.
      z = days + 719468;
      era = (if z >= 0 then z else z - 146096) / 146097;
      doe = z - era * 146097;
      yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
      y = yoe + era * 400;
      doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
      mp = (5 * doy + 2) / 153;
      d = doy - (153 * mp + 2) / 5 + 1;
      m = mp + (if mp < 10 then 3 else -9);
      y' = y + (if m <= 2 then 1 else 0);

      pad = s: if builtins.stringLength s < 2 then "0" + s else s;
    in "${toString y'}${pad (toString m)}${pad (toString d)}${pad (toString hours)}${pad (toString minutes)}${pad (toString seconds)}";
in {
  haskell-nix = prev.haskell-nix // {
    sources = prev.haskell-nix.sources // builtins.listToAttrs (map (name: {
        inherit name;
        value = final.fetchFromGitLab {
                    domain = "gitlab.haskell.org";
                    owner = "ghc";
                    repo = "ghc";
                    fetchSubmodules = true;
                    inherit (lockFile.nodes.${name}.locked) rev;
                    sha256 = lockFile.nodes.${name}.locked.narHash;
                } // {
                  lastModifiedDate = formatSecondsSinceEpoch lockFile.nodes.${name}.locked.lastModified;
                };
    }) ["ghc910X" "ghc911"]);
  };
}
