_: prev: {
  # Most GHC builds aren't compatible with sphinx 6+
  sphinx_5 = prev.sphinx.overridePythonAttrs(_: rec {
    version = "5.3.0";

    src = prev.fetchFromGitHub {
      owner = "sphinx-doc";
      repo = "sphinx";
      rev = "refs/tags/v${version}";
      postFetch = ''
        # Change ä to æ in file names, since ä can be encoded multiple ways on different
        # filesystems, leading to different hashes on different platforms.
        cd "$out";
        mv tests/roots/test-images/{testimäge,testimæge}.png
        sed -i 's/testimäge/testimæge/g' tests/{test_build*.py,roots/test-images/index.rst}
      '';
      hash = "sha256-80bVg1rfBebgSOKbWkzP84vpm39iLgM8lWlVD64nSsQ=";
    };

    # Some tests require network access
    doCheck = false;

    pythonImportsCheck = [ "sphinx" ];
  });

}
