{ }:

{
  git = {
    enable = true;
    lfs.enable = true;
    signing = {
      key = "~/.ssh/id_ed25519.pub";
      # Sign tags by default; commits remain opt-in below.
      signByDefault = true;
    };
    settings = {
      user.name = "Ben Siraphob";
      user.email = "bensiraphob@gmail.com";
      gpg.format = "ssh";
      pull.rebase = true;
      github.user = "siraben";
      advice.detachedHead = false;
      url."ssh://git@github.com/".insteadOf = "https://github.com/";
      commit.gpgSign = false;
      core.commitGraph = true;
      fetch.writeCommitGraph = true;
      # Performance improvements
      core.preloadIndex = true;
      core.fscache = true;
      core.untrackedCache = true;
      feature.manyFiles = true;
      gc.writeCommitGraph = true;
      # Diff performance
      diff.algorithm = "histogram";
    };
  };
}
