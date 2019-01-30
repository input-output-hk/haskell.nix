{ lib, haskellLib }:

let
  emptyConfig = {
    components = {
       benchmarks = { };
       exes = { };
       foreignlibs = { };
       library = "library";
       sublibs = { };
       tests = { };
       all = "all";
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
       all = "all";
    };
    package.identifier.name = "nnn";
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
    expected = emptyConfig.components // { library = "empty"; all = "empty"; };
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
}
