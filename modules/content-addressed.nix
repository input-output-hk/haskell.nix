{ lib, config, ... }:
with lib;
with lib.types;
{
  _file = "haskell.nix/modules/content-addressed.nix";
  options = {
    contentAddressed = {
      enable = mkOption {
        type = bool;
        default = false;
        description = ''
          Build content addressed derivations, requires Nix to have experimental feature
          `ca-derivations` enabled.
        '';
      };
      include = mkOption {
        type = anything;
        default = _: true;
        description = ''
          Predicate indicating which components are content addressed, by default every
          component is content addressed.
          The predicate argument is the `nameOnly` string used by the `comp-builder` i.e.
          \"$${package.identifier.name}-$${componentId.ctype}-$${componentId.cname}"
        '';
        example = literalExpression ''
          {
            include = name: builtins.elem name contentAddressedComponents;
          }
        '';
      };
    };
  };
}
