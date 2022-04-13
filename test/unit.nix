{ pkgs, lib, haskellLib, compiler-nix-name }:

let
  emptyConfig = {
    components = {
       benchmarks = { };
       exes = { };
       foreignlibs = { };
       library = { buildable = true; planned = true; };
       sublibs = { };
       tests = { };
    };
    package.identifier.name = "empty";
    package.buildable = true;
  };

  componentsConfig = {
    components = {
       benchmarks = { bbb = { buildable = true; planned = true; }; };
       exes = { eee = { buildable = true; planned = true; }; };
       foreignlibs = { fff = { buildable = true; planned = true; }; };
       library = { buildable = true; planned = true; };
       sublibs = { };
       tests = { ttt = { buildable = true; planned = true; }; };
    };
    package.identifier.name = "nnn";
    package.buildable = true;
  };

  testRepoData = {
    url = "https://github.com/input-output-hk/haskell.nix.git";
    ref = "487eea1c249537d34c27f6143dff2b9d5586c657";
    sha256 = "077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123";
  };
in
lib.runTests {
  # identity function for applyComponents
  test-applyComponents-id = {
    expr = haskellLib.applyComponents (componentId: component: component) emptyConfig;
    expected = emptyConfig.components;
  };

  # map a component to its component name and check these are correct
  test-applyComponents-library = {
    expr = haskellLib.applyComponents (componentId: component: componentId.cname) emptyConfig;
    expected = emptyConfig.components // { library = "empty"; };
  };

  test-applyComponents-components = {
    expr = haskellLib.applyComponents (componentId: component: component) componentsConfig;
    expected = componentsConfig.components;
  };

  # testing that the tests work
  testId = {
    expr = lib.id 1;
    expected = 1;
  };

  testParseBlock1 = {
    expr = __toJSON (haskellLib.parseSourceRepositoryPackageBlock "cabal.project" {} ''
        type: git
        location: https://github.com/input-output-hk/haskell.nix.git
        tag: 487eea1c249537d34c27f6143dff2b9d5586c657
        --sha256: 077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123
      -- end of block
    '');
    expected = __toJSON {
      otherText = "-- end of block\n";
      sourceRepo = testRepoData // { subdirs = ["."]; };
    };
  };

  testParseBlock2 = {
    expr = __toJSON (haskellLib.parseSourceRepositoryPackageBlock "cabal.project" {} ''
        type: git
        location: https://github.com/input-output-hk/haskell.nix.git
        tag: 487eea1c249537d34c27f6143dff2b9d5586c657
        --sha256: 077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123
        subdir: dir
      -- end of block
    '');
    expected = __toJSON {
      otherText = "-- end of block\n";
      sourceRepo = testRepoData // { subdirs = ["dir"]; };
    };
  };

  testParseBlock3 = {
    expr = __toJSON (haskellLib.parseSourceRepositoryPackageBlock "cabal.project" {} ''
        type: git
        location: https://github.com/input-output-hk/haskell.nix.git
        tag: 487eea1c249537d34c27f6143dff2b9d5586c657
        --sha256: 077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123
        subdir: dir1 dir2
      -- end of block
    '');
    expected = __toJSON {
      otherText = "-- end of block\n";
      sourceRepo = testRepoData // { subdirs = ["dir1" "dir2"]; };
    };
  };

  testParseBlock4 = {
    expr = __toJSON (haskellLib.parseSourceRepositoryPackageBlock "cabal.project" {} ''
        type: git
        location: https://github.com/input-output-hk/haskell.nix.git
        tag: 487eea1c249537d34c27f6143dff2b9d5586c657
        --sha256: 077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123
        subdir:
          dir1
          dir2
      -- end of block
    '');
    expected = __toJSON {
      otherText = "-- end of block\n";
      sourceRepo = testRepoData // { subdirs = ["dir1" "dir2"]; };
    };
  };

  testParseRepositoryBlock = {
      expr = __toJSON (haskellLib.parseRepositoryBlock "cabal.project" {} {}
        pkgs.evalPackages.haskell-nix.cabal-install.${compiler-nix-name}
        pkgs.evalPackages.haskell-nix.nix-tools.${compiler-nix-name} ''
          ghcjs-overlay
            url: https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/bfc363b9f879c360e0a0460ec0c18ec87222ec32
            secure: True
            root-keys:
            key-threshold: 0
            --sha256: sha256-y1vQnXI1XzkjnC4h66tVDmu2TZjZPcMrZEnE3m0XOfg=
          -- end of block
      '');
      expected = __toJSON {
        name = "ghcjs-overlay";
        repoContents = "/nix/store/gzjj6rjjgvkm5midldy292ghbq7hszna-ghcjs-overlay";
        repo = {
          ghcjs-overlay = "/nix/store/gzjj6rjjgvkm5midldy292ghbq7hszna-ghcjs-overlay";
        };
        hackage = {
          Cabal = {
            "3.2.1.0" = {
              revisions = {
                default = "/nix/store/3ndxx3k43gmjkfl1qn1x39g15hz5amad-Cabal-3.2.1.0-r0-2b5309e942658e3b16e6938115867538e70a647d98e3dc967f2be20d6b886e61.nix";
                r0 = "/nix/store/3ndxx3k43gmjkfl1qn1x39g15hz5amad-Cabal-3.2.1.0-r0-2b5309e942658e3b16e6938115867538e70a647d98e3dc967f2be20d6b886e61.nix";
              };
              sha256 = "826970f742b63d751f6fe3be7f862b7b1e419ddfafef3014c01de54f12874a4a";
            };
          };
          basement = {
            "0.0.12"= {
              revisions = {
                default = "/nix/store/3h1xwcij240kj243f23siw3p30hdbnrj-basement-0.0.12-r0-600669787199915545f99754496f13f955203b94dbb31de50093362c03367bb7.nix";
                r0 = "/nix/store/3h1xwcij240kj243f23siw3p30hdbnrj-basement-0.0.12-r0-600669787199915545f99754496f13f955203b94dbb31de50093362c03367bb7.nix";
              };
              sha256 = "cf8f96fd92438739a516881abb7e14747118e82a12634d44acc83173fb87f535";
            };
          };
          clock = {
            "0.8.2" = {
              revisions = {
                default = "/nix/store/mdsf1fgbpi4m1yvsg5z8z4hk5w7i63x7-clock-0.8.2-r0-2a8441d9f531bb51bb1806e56e9e9a43e5f0214faea4f31219c15120128ca43a.nix";
                r0 = "/nix/store/mdsf1fgbpi4m1yvsg5z8z4hk5w7i63x7-clock-0.8.2-r0-2a8441d9f531bb51bb1806e56e9e9a43e5f0214faea4f31219c15120128ca43a.nix";
              };
              sha256 = "57715a01df74568c638f1138b53642094de420bafd519e9f53ec7fe92876121e";
            };
          };
          cryptonite = {
            "0.29" = {
              revisions = {
                default = "/nix/store/i84rvw53j9b6p53dalg6xq85blrkrk01-cryptonite-0.29-r0-231db2acdaefc978865af9b72a6e65c4ebc70238174a7ad9076d68900f3d866d.nix";
                r0 = "/nix/store/i84rvw53j9b6p53dalg6xq85blrkrk01-cryptonite-0.29-r0-231db2acdaefc978865af9b72a6e65c4ebc70238174a7ad9076d68900f3d866d.nix";
              };
              sha256 = "f104836bdaeed5243ff7e9fc0757d7255778f0af22976eef2b7789e7e1094283";
            };
          };
          double-conversion = {
            "2.0.2.0" = {
              revisions = {
                default = "/nix/store/cb41xbmf6abcq30sj2a1b00qim8nyhqx-double-conversion-2.0.2.0-r0-698f94e66b6263a1049b56ede47aa48e224c764f345c3130265742513443595b.nix";
                r0 = "/nix/store/cb41xbmf6abcq30sj2a1b00qim8nyhqx-double-conversion-2.0.2.0-r0-698f94e66b6263a1049b56ede47aa48e224c764f345c3130265742513443595b.nix";
              };
              sha256 = "67c83bf4619624ef6b950578664cbcd3bc12eaed0d7a387997db5e0ba29fb140";
            };
          };
          foundation = {
            "0.0.26.1" = {
              revisions = {
                default = "/nix/store/5vw12rxa3as8b82kz1arddwzs45fzj6q-foundation-0.0.26.1-r0-9a2f63a33dc6b3c1425c4755522b8e619d04fdfcfef72e358155b965b28745a8.nix";
                r0 = "/nix/store/5vw12rxa3as8b82kz1arddwzs45fzj6q-foundation-0.0.26.1-r0-9a2f63a33dc6b3c1425c4755522b8e619d04fdfcfef72e358155b965b28745a8.nix";
              };
              sha256 = "3c588f6bcf875762ac18b03b17a7ee3c0c60c8e2c884c0192269b0a97e89d526";
            };
          };
          network = {
            "3.1.2.1" = {
              revisions = {
                default = "/nix/store/sfc02hqr02pa3rwhha1skvlkiwmgvsf7-network-3.1.2.1-r0-ed4b1bb733613df5a2ebecd5533d08f20bacb0dc59ed207fd4b4670fe718713f.nix";
                r0 = "/nix/store/sfc02hqr02pa3rwhha1skvlkiwmgvsf7-network-3.1.2.1-r0-ed4b1bb733613df5a2ebecd5533d08f20bacb0dc59ed207fd4b4670fe718713f.nix";
              };
              sha256 = "21869fd942cb9996ba26ba9418cdd44ac869f81caba08e5a2b2cdfe792ae4518";
            };
            "3.1.2.5" = {
              revisions = {
                default = "/nix/store/rrh5f3hb8g5bwgd6y7lwfky3nrgsg5b2-network-3.1.2.5-r0-433a5e076aaa8eb3e4158abae78fb409c6bd754e9af99bc2e87583d2bcd8404a.nix";
                r0 = "/nix/store/rrh5f3hb8g5bwgd6y7lwfky3nrgsg5b2-network-3.1.2.5-r0-433a5e076aaa8eb3e4158abae78fb409c6bd754e9af99bc2e87583d2bcd8404a.nix";
              };
              sha256 = "ee914e9b43bfb0f415777eb0473236803b14a35d48f6172079260c92c6ceb335";
            };
          };
          terminal-size = {
            "0.3.2.1" = {
              revisions = {
                default = "/nix/store/k9gk020ygp6j94sa3xv46fkgrc8jl5zn-terminal-size-0.3.2.1-r0-7b2d8e0475a46961d07ddfb91dee618de70eff55d9ba0402ebeac1f9dcf9b18b.nix";
                r0 = "/nix/store/k9gk020ygp6j94sa3xv46fkgrc8jl5zn-terminal-size-0.3.2.1-r0-7b2d8e0475a46961d07ddfb91dee618de70eff55d9ba0402ebeac1f9dcf9b18b.nix";
              };
              sha256 = "8e4fbfea182f3bf5769744196ca88bb2cb1c80caa617debe34336f90db27131e";
            };
          };
          unix-compat = {
            "0.5.3" = {
              revisions = {
                default = "/nix/store/mc1vh6wzg0vp1c1srxrqmpv9fsqay6dw-unix-compat-0.5.3-r0-9c6d68f9afb5baa6be55e8415dd401835ce0d4dfc2090f1c169fcd61c152ebac.nix";
                r0 = "/nix/store/mc1vh6wzg0vp1c1srxrqmpv9fsqay6dw-unix-compat-0.5.3-r0-9c6d68f9afb5baa6be55e8415dd401835ce0d4dfc2090f1c169fcd61c152ebac.nix";
              };
              sha256 = "2fe56781422d5caf47dcbbe82c998bd33f429f8c7093483fad36cd2d31dbdceb";
            };
          };
        };
      };
    };
}
