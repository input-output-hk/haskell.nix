{ testSrc, evalPackages, buildPackages }:

(buildPackages.haskell-nix.cabalProject {
  src = evalPackages.fetchgit {
    url = "https://github.com/haskell/haskell-language-server.git";
    fetchSubmodules = true;
    rev = "d2654185eef1b0d703cebc694e85438e20600e37";
    sha256 = "0s0k2i0imkcn9zykhrlqq0r4ssv25mwbpdyc675xkzgl1vj1l8kd";
  };
  lookupSha256 = { location, tag, ... }: {
      "https://github.com/wz1000/shake"."fb3859dca2e54d1bbb2c873e68ed225fa179fbef" = "0sa0jiwgyvjsmjwpfcpvzg2p7277aa0dgra1mm6afh2rfnjphz8z";
      "https://github.com/peti/cabal-plan"."894b76c0b6bf8f7d2f881431df1f13959a8fce87" = "06iklj51d9kh9bhc42lrayypcpgkjrjvna59w920ln41rskhjr4y";
    }."${location}"."${tag}";
}).haskell-language-server.components.exes.haskell-language-server
