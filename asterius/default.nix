{ fetchExternal }:
  fetchExternal {
    name     = "asterius";
    specJSON = ./asterius.json;
    override = "asterius";
  }

