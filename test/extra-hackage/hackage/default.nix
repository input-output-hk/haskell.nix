with builtins; mapAttrs (_: mapAttrs (_: data: rec {
 inherit (data) sha256;
 revisions = (mapAttrs (_rev: rdata: {
  inherit (rdata) revNum sha256;
  outPath = ./. + "/hackage/${rdata.outPath}";
 }) data.revisions) // {
  default = revisions."${data.revisions.default}";
 };
})) (fromJSON (readFile ./hackage.json))
