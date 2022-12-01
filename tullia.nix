let
  ciInputName = "GitHub event";
  repository = "input-output-hk/haskell.nix";
in {
  tasks.ci = {config, lib, ...}: {
    preset = {
      nix.enable = true;

      github.ci = {
        enable = config.actionRun.facts != {};
        inherit repository;
        revision = config.preset.github.lib.readRevision ciInputName null;
      };
    };

    command.text = config.preset.github.status.lib.reportBulk {
      bulk.text = "nix eval .#ciJobs --apply __attrNames --json | nix-systems -i";
      each.text = ''nix build -L .#ciJobs."$1".required'';
      skippedDescription = lib.escapeShellArg "No nix builder available for this system";
    };

    memory = 1024 * 8;
    nomad.resources.cpu = 10000;
  };

  actions."haskell.nix/ci" = {
    task = "ci";
    io = ''
      let github = {
        #input: "${ciInputName}"
        #repo: "${repository}"
      }
      
      #lib.merge
      #ios: [
        #lib.io.github_push & github,
        #lib.io.github_pr   & github,
      ]
    '';
  };
}
