self: system:

let
  ciInputName = "GitHub event";
in rec {
  tasks = let
    inherit (self.inputs.tullia) flakeOutputTasks taskSequence;

    common = {
      config,
      ...
    }: {
      preset = {
        # needed on top-level task to set runtime options
        nix.enable = true;

        github-ci = {
          enable = config.actionRun.facts != {};
          repo = "input-output-hk/haskell.nix";
          sha = config.preset.github-ci.lib.getRevision ciInputName null;
        };
      };
    };

    mkHydraJobTask = flakeOutputTask: {...}: {
      imports = [common flakeOutputTask];

      memory = 1024 * 8;
      nomad.resources.cpu = 10000;
    };
    mkHydraJobTasks = __mapAttrs (_: mkHydraJobTask);

    hydraJobTasks = mkHydraJobTasks (flakeOutputTasks ["ciJobs" system] self);

    ciTasks = taskSequence "ci/" hydraJobTasks (__attrNames hydraJobTasks);
  in
    ciTasks // {
      "ci" = {lib, ...}: {
        imports = [common];
        after = __attrNames ciTasks;
      };
    };

  actions = {
    "haskell.nix/ci" = {
      task = "ci";
      io = ''
        let github = {
          #input: "${ciInputName}"
          #repo: "input-output-hk/haskell.nix"
        }
        
        #lib.merge
        #ios: [
          #lib.io.github_push & github,
          #lib.io.github_pr   & github,
        ]
      '';
    };
  };
}
