{ pkgs, lib, haskellLib }:

let
  emptyConfig = {
    components = {
       benchmarks = { };
       exes = { };
       foreignlibs = { };
       library = "library";
       sublibs = { };
       tests = { };
    };
    package.identifier.name = "empty";
  };

  componentsConfig = {
    components = {
       benchmarks = { bbb = "bbb"; };
       exes = { eee = "eee"; };
       foreignlibs = { fff = "fff"; };
       library = "library";
       sublibs = { };
       tests = { ttt = "ttt"; };
    };
    package.identifier.name = "nnn";
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
    expr = __toJSON (haskellLib.parseBlock "cabal.project" (_: null) ''
        type: git
        location: https://github.com/input-output-hk/haskell.nix.git
        tag: 487eea1c249537d34c27f6143dff2b9d5586c657
        --sha256: 077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123
      -- end of block
    '');
    expected = __toJSON {
      otherText = "-- end of block\n";
      sourceRepo = [(testRepoData // { subdir = "."; })];
    };
  };

  testParseBlock2 = {
    expr = __toJSON (haskellLib.parseBlock "cabal.project" (_: null) ''
        type: git
        location: https://github.com/input-output-hk/haskell.nix.git
        tag: 487eea1c249537d34c27f6143dff2b9d5586c657
        --sha256: 077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123
        subdir: dir
      -- end of block
    '');
    expected = __toJSON {
      otherText = "-- end of block\n";
      sourceRepo = [(testRepoData // { subdir = "dir"; })];
    };
  };

  testParseBlock3 = {
    expr = __toJSON (haskellLib.parseBlock "cabal.project" (_: null) ''
        type: git
        location: https://github.com/input-output-hk/haskell.nix.git
        tag: 487eea1c249537d34c27f6143dff2b9d5586c657
        --sha256: 077j5j3j86qy1wnabjlrg4dmqy1fv037dyq3xb8ch4ickpxxs123
        subdir: dir1 dir2
      -- end of block
    '');
    expected = __toJSON {
      otherText = "-- end of block\n";
      sourceRepo = [(testRepoData // { subdir = "dir1"; }) (testRepoData // { subdir = "dir2"; })];
    };
  };

  testParseBlock4 = {
    expr = __toJSON (haskellLib.parseBlock "cabal.project" (_: null) ''
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
      sourceRepo = [(testRepoData // { subdir = "dir1"; }) (testRepoData // { subdir = "dir2"; })];
    };
  };
}
