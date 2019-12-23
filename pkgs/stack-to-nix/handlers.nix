pkgs: with pkgs; with lib;

[
  ({
    handle = spec: self:
      let
        components = splitString "-" spec;
        name = concatStringsSep "-" (init components);
        version = last components;
      in
      { "${name}" = lib.callHackage self name version {}; };

    test = isString;
  })
  ({
    handle = spec: self:
      let
        src = fetchGit {
          url = spec.git;
          ref = "*";
          rev = spec.commit;
        };

        subdirToAttr = subdir:
          let
            name = cabalPackageName "${src}/${subdir}";
          in
          nameValuePair name (cabalToNix self name src {} ''--subpath="${subdir}"'');

        subdirs = spec.subdirs or [ "." ];
      in
      listToAttrs (map subdirToAttr subdirs);

    test = spec: isAttrs spec && spec ? git;
  })
]
