let

  githubTarball = owner: repo: rev:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    };

  reflexPlatform = import (githubTarball "reflex-frp" "reflex-platform" "develop") {};
  
in reflexPlatform.project ({ pkgs, ... }: {

  withHoogle = false;
  
  packages = {
    reflex-datasource = ./reflex-datasource;
    reflex-datasource-client = ./reflex-datasource-client;
    reflex-datasource-server = ./reflex-datasource-server;
    reflex-devserver = ./reflex-devserver;
    reflex-route = ./reflex-route;
    reflex-route-client = ./reflex-route-client;
    reflex-route-server = ./reflex-route-server;
    skeleton = ./skeleton;
  };

  shells = {
    ghc = [
      "reflex-datasource"
      "reflex-datasource-client"
      "reflex-datasource-server"
      "reflex-devserver"
      "reflex-route"
      "reflex-route-client"
      "reflex-route-server"
      "skeleton"
    ];
    ghcjs = [
      "reflex-datasource"
      "reflex-datasource-client"
      "reflex-route"
      "reflex-route-client"
      "skeleton"
    ];
  };  
})
