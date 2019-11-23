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
    reflex-datasource = ./reflex-datasource;
    reflex-datasource-client = ./reflex-datasource-client;
    reflex-datasource-server = ./reflex-datasource-server;
    reflex-devserver = ./reflex-devserver;
    reflex-route = ./reflex-route;
    reflex-route-client = ./reflex-route-client;
    reflex-route-server = ./reflex-route-server;
  };

  shells = {
    ghc = [
      "example"
      "reflex-datasource"
      "reflex-datasource-client"
      "reflex-datasource-server"
      "reflex-devserver"
      "reflex-route"
      "reflex-route-client"
      "reflex-route-server"
    ];
    ghcjs = ["example" "reflex-datasource" "reflex-datasource-client"];
  };

  # overrides = self: super: {
  #   reflex-datasource = self.callCabal2nix "reflex-datasource" (gitignore ./reflex-datasource) {};
  #   reflex-datasource-client = self.callCabal2nix "reflex-datasource-client" (gitignore ./reflex-datasource-client) {};
  #   reflex-datasource-server = self.callCabal2nix "reflex-datasource-server" (gitignore ./reflex-datasource-server) {};
  # };
  
  shellToolOverrides = ghc: super: {
    ghcid = pkgs.haskell.lib.justStaticExecutables super.ghcid;
    haskell-ide-engine = null;
  };
  
})
