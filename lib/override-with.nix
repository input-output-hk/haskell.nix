# overrideWith - Allow the user to override the given default path
# with a NIX_PATH entry.
override: default:
 let
   try = builtins.tryEval (builtins.findFile builtins.nixPath override);
 in if try.success then
   builtins.trace "using search host <${override}>" try.value
 else
   default
