{ testSrc, haskell-nix, evalPackages }:

(haskell-nix.cabalProject {
  src = evalPackages.fetchFromGitHub {
    owner = "hasura";
    repo = "graphql-engine";
    sha256 = "0v5fs4ma2vxs1bygp45j62jg68bk4skvnf8g9j81b6jydda18lzs";
    rev = "ad07c06e5037f0deb83a2d3ccf1703df6cad1d35";
  } + "/server";
  sha256map = {
    "https://github.com/hasura/pg-client-hs.git"."70a849d09bea9461e72c5a5bbde06df65aab61c0" = "1941gj5yp24kx0xb1nd774nwp5vnpsp6m83isqkwpyz9spl4sq7l";
    "https://github.com/hasura/graphql-parser-hs.git"."2e8adedbb426d487df77bde244b7fe3cbd40a255" = "0srz1f969ipbrm1mk4lr2cj7rf9h17mbimc8pjds8g4x0q9ym6mm";
    "https://github.com/hasura/ci-info-hs.git"."6af5a68450347a02295a9cd050d05a8b2f5c06ab" = "0rn1799z4y7z1c6ijrr0gscarg25zmnfq0z9rrmk4ad727vf1ppc";
  };
}).graphql-engine.components.exes
