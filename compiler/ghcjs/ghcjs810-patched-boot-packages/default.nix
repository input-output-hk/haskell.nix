{ pkgs
, ...
}:
let
  patch = p: path: pkgs.runCommand "patched" { buildInputs = [ pkgs.gnupatch ]; } ''
    cp -r ${path} $out
    chmod -R +w $out
    cd $out
    patch -p1 <${p}
  '';
  getSubdir = d: path: pkgs.runCommand "subdir" { } ''
    cp -r ${path}/${d} $out
  '';
in
[
  {
    name = "primitive";
    src = pkgs.fetchgit {
      url = "https://github.com/haskell/primitive.git";
      rev = "11ff865fd2157a1dfd7dfd525b1cf27e1a300633";
      sha256 = "1skim1vlmf0v4ixm8h8ja3m1pz0h10xrfdh1rfzawsy8rvrp2cc0";
    };
  }
  {
    name = "dlist";
    src = pkgs.fetchgit {
      url = "https://github.com/spl/dlist.git";
      rev = "567cfeb2f5da30f2e5faf167895b3ee04c698ca0";
      sha256 = "0b8ij3ls7bccbp9rlm8dnflnf1wim9xsg42wmirvwxwmh2c0r0hg";
    };
  }
  {
    name = "vector";
    src = pkgs.fetchgit {
      url = "https://github.com/haskell/vector.git";
      rev = "de10fd281fa5d8beee3623e809a71385a5ef6864";
      sha256 = "09vhbvrkw90xgqfrybmhy120rwcy4snw2f97004alr3n96lwpg1q";
    };
  }
  {
    name = "ghcjs-base";
    src = patch ./ghcjs-base.patch (pkgs.fetchgit {
      url = "https://github.com/ghcjs/ghcjs-base.git";
      rev = "85e31beab9beffc3ea91b954b61a5d04e708b8f2";
      sha256 = "15fdkjv0l7hpbbsn5238xxgzfdg61g666nzbv2sgxkwryn5rycv0";
    });
  }
  {
    name = "text";
    src = pkgs.fetchgit {
      url = "https://github.com/dfordivam/text.git";
      rev = "126174753ea8e5f45df8fcbba609e3f1c453bf27";
      sha256 = "0l7nbln2w77s12fm4ybhi0jsfnxkyiwskfx3b682pfisa6n32rgm";
    };
  }
  {
    name = "hashable";
    src = patch ./hashable.patch (pkgs.fetchgit {
      url = "https://github.com/haskell-unordered-containers/hashable.git";
      rev = "819fdbeaeeb62ed1344fb057de17fa29b10fa3e1";
      sha256 = "0xnwfwnnh0bxq16wx0qmdakvgvj8i22km3bhvvki15rd9jil04cc";
    });
  }
  {
    name = "integer-logarithms";
    src = pkgs.fetchgit {
      url = "https://github.com/haskellari/integer-logarithms.git";
      rev = "df09cef2c8eb0d5899e2bcccee2a4fad413bdc11";
      sha256 = "1k17hjnjmy44dfywx9nr804qiqmjjhkpfpll5bsfvzvpnl5r75nj";
    };
  }
  {
    name = "scientific";
    src = pkgs.fetchgit {
      url = "https://github.com/basvandijk/scientific.git";
      rev = "1d049cdf04e2d92ab187754044b0df933c5e6b0d";
      sha256 = "1v4fmfq8hyhdb5ck5r2b45s1izr7hx97v4g7s597ajbv72ldvilq";
    };
  }
  {
    name = "attoparsec";
    src = pkgs.fetchgit {
      url = "https://github.com/dfordivam/attoparsec.git";
      rev = "cbfb944aeff61144059a28ada93c579e93ccf7d2";
      sha256 = "0bbr4943439sy3grwpwbna4mk7iv87ijaiv3jjfhxhp7y67z51h8";
    };
  }
  {
    name = "splitmix";
    src = pkgs.fetchgit {
      url = "https://github.com/haskellari/splitmix.git";
      rev = "d8c1b57bb75178675423f87b1ff58429f37bc9fe";
      sha256 = "1m8v439vlwqpqw28dbr04n6m0j2cfb169ly285lp0bjmzcwlr289";
    };
  }
  {
    name = "random";
    src = pkgs.fetchgit {
      url = "https://github.com/haskell/random.git";
      rev = "8c59bd0fda84fa581d53069b45100d810f5a778f";
      sha256 = "1drdipfbmpg2g2cdrnq1k8yysh86i7ljwm6zkrrs3g8l47n4ha4d";
    };
  }
  {
    name = "uuid-types";
    src = getSubdir "uuid-types" (pkgs.fetchgit {
      url = "https://github.com/haskell-hvr/uuid.git";
      rev = "e8dc7126582ba4d3abc5e0f26dc1d8477caab01e";
      sha256 = "037cnfz7rhkw383rn117b994jzs93i3bwdv47x5qiza5h5syd76f";
    });
  }
  {
    name = "unordered-containers";
    src = pkgs.fetchgit {
      url = "https://github.com/haskell-unordered-containers/unordered-containers.git";
      rev = "db9ddde2b243366976a1bdfbb12a5e257ca762c3";
      sha256 = "1wd5hxppj1gg9fa1ihcn3s2y0i70ykv2sf8sym6dlf3r4snda9zi";
    };
  }
  {
    name = "base-orphans";
    src = pkgs.fetchgit {
      url = "https://github.com/haskell-compat/base-orphans.git";
      rev = "1af3a3512efbcc12e0c34637a0e433ab6e195932";
      sha256 = "14b6qwc3dbnmrhqk66a2cn4pvv3nf6kb3h7ym1hjn5xs7n62zhw1";
    };
  }
  {
    name = "time-compat";
    src = pkgs.fetchgit {
      url = "https://github.com/haskellari/time-compat.git";
      rev = "67b8b063295601fb1db185baad363a3f33ed63a9";
      sha256 = "0nbjsn7yrygv4i1fd8x90mnf4janq536ffvkjd5zsnyzm8qk8k3j";
    };
  }
  {
    name = "th-abstraction";
    src = pkgs.fetchgit {
      url = "https://github.com/glguy/th-abstraction.git";
      rev = "d75761bc5a0f912aeeb508e2c227b9fd830153fd";
      sha256 = "070ihxncymzsbrgvwmrm717n0qq5bf9nvc2k210nkil1cwrsnhvc";
    };
  }
  {
    name = "tagged";
    src = pkgs.fetchgit {
      url = "https://github.com/ekmett/tagged.git";
      rev = "15fcfe90c779efea951ecf4d451d709aed5f7d32";
      sha256 = "1x1wr4zf7k1kvd6rj06h9shw7p5di1jra9fysyw0jx7riiq0v3ch";
    };
  }
  {
    name = "base-compat";
    src = getSubdir "base-compat" (pkgs.fetchgit {
      url = "https://github.com/haskell-compat/base-compat.git";
      rev = "e8dbac72251e898fc4b1c1abb34d6578da12f476";
      sha256 = "1gs15g8h7p8gab9j5fjvjlhrfv112w91plraf9chq80c5slvk016";
    });
  }
  {
    name = "base-compat-batteries";
    src = getSubdir "base-compat-batteries" (pkgs.fetchgit {
      url = "https://github.com/haskell-compat/base-compat.git";
      rev = "e8dbac72251e898fc4b1c1abb34d6578da12f476";
      sha256 = "1gs15g8h7p8gab9j5fjvjlhrfv112w91plraf9chq80c5slvk016";
    });
  }
  {
    name = "aeson";
    src = patch ./aeson.patch (pkgs.fetchgit {
      url = "https://github.com/dfordivam/aeson.git";
      rev = "9a8f57a52eb748a030bc2bcf5c3159a5e2e696b2";
      sha256 = "1y8j3ddxxddpnb7c0vi73ijb08sr09z1gdzfg1rfyzn82d6i38zv";
    });
  }
]
