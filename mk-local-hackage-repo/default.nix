# Create a local hackare repo, we can use as a repository in a cabal config
#
# This will include:
# - 01-index.tar.gz (the index file)
# - root.json and
# - mirrors.json as metadata items.
# - snapshot.json that records the index, root and mirrors.
# - timestamp.json that will record the snapshot.json
#
# This is all part of The Update Framework (TUF) and the specific implementation
# cabal-install (via hackage-security) does of it.
#
# We will create a completely unsigned bare repository.  Using signing keys within
# nix would be pointless as we'd have to hardcode them to produce the same output
# reproducably.
#
pkgs:
{ name, index }:

pkgs.evalPackages.runCommand "hackage-repo-${name}" { nativeBuildInputs = [ pkgs.evalPackages.nix ]; } ''
mkdir -p $out
export expires="4000-01-01T00:00:00Z"

ln -sf ${index} $out/01-index.tar.gz
export index_md5=$(nix-hash --flat --type md5 ${index})
export index_sha256=$(nix-hash --flat --type sha256 ${index})
export index_length=$(stat --printf="%s" ${index})

substituteAll ${./root.json} $out/root.json
export root_md5=$(nix-hash --flat --type md5 $out/root.json)
export root_sha256=$(nix-hash --flat --type sha256 $out/root.json)
export root_length=$(stat --printf="%s" $out/root.json)

substituteAll ${./mirrors.json} $out/mirrors.json
export mirrors_md5=$(nix-hash --flat --type md5 $out/mirrors.json)
export mirrors_sha256=$(nix-hash --flat --type sha256 $out/mirrors.json)
export mirrors_length=$(stat --printf="%s" $out/mirrors.json)

substituteAll ${./snapshot.json} $out/snapshot.json
export snapshot_md5=$(nix-hash --flat --type md5 $out/snapshot.json)
export snapshot_sha256=$(nix-hash --flat --type sha256 $out/snapshot.json)
export snapshot_length=$(stat --printf="%s" $out/snapshot.json)

substituteAll ${./timestamp.json} $out/timestamp.json
''
