let

  githubTarball = owner: repo: rev:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    };

  reflexPlatform = import (githubTarball "reflex-frp" "reflex-platform" "develop") {};
  
in reflexPlatform.project ({ pkgs, ... }: {

  withHoogle = false;
  
  packages = {
    arohi-config = ./arohi-config;
    arohi-datasource = ./arohi-datasource;
    arohi-datasource-client = ./arohi-datasource-client;
    arohi-datasource-server = ./arohi-datasource-server;
    arohi-route = ./arohi-route;
    arohi-route-client = ./arohi-route-client;
    arohi-route-server = ./arohi-route-server;
    arohi-server = ./arohi-server;
    skeleton = ./skeleton;
  };

  shells = {
    ghc = [
      "arohi-config"
      "arohi-datasource"
      "arohi-datasource-client"
      "arohi-datasource-server"
      "arohi-route"
      "arohi-route-client"
      "arohi-route-server"
      "arohi-server"
      "skeleton"
    ];
    ghcjs = [
      "arohi-config"
      "arohi-datasource"
      "arohi-datasource-client"
      "arohi-route"
      "arohi-route-client"
      "skeleton"
    ];
  };  
})
