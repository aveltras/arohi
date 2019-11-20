let

  githubTarball = owner: repo: rev:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    };

  latestPkgs = import (githubTarball "NixOS" "nixpkgs" "f1682a7f126d4d56dfbb96bb8c8c5582abb22828") {};
  reflexPlatform = import (githubTarball "reflex-frp" "reflex-platform" "aa8a9d1ac3d41ad51fbe04e575d4350da65cf3db") {};
  gitignore = latestPkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  
in reflexPlatform.project ({ pkgs, ... }: {

  useWarp = false;
  withHoogle = false;
  
  packages = {
    example = ./example;
  };

  shells = {
    ghc = ["example"];
    ghcjs = ["example"];
  };

  overrides = self: super: {
    reflex-datasource = self.callCabal2nix "reflex-datasource" (gitignore ./reflex-datasource) {};
  };
  
  shellToolOverrides = ghc: super: {
    ghcid = pkgs.haskell.lib.justStaticExecutables super.ghcid;
    haskell-ide-engine = null;
  };
  
})
