{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "efc6d923c633207d18bd4d8cae3e20110a377864";
    sha256 = "121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5";
    })
}:
(import reflex-platform {}).project ({ pkgs, ... }:
let
  reflexDomContribSrc = builtins.fetchGit {
    url = "https://github.com/reflex-frp/reflex-dom-contrib.git";
    rev = "11db20865fd275362be9ea099ef88ded425789e7";
  };

  override = self: pkg: with pkgs.haskell.lib;
    doJailbreak (pkg.overrideAttrs
    (old: {
      buildInputs = old.buildInputs ++ [ self.doctest self.cabal-doctest ];
    }));

in {
  useWarp = true;

  overrides = self: super: with pkgs.haskell.lib; rec {
    reflex-dom-contrib = override self
      (self.callCabal2nix "reflex-dom-contrib" reflexDomContribSrc { });
  };

  packages = {
    todo-common = ./todo-common;
    todo-backend = ./todo-backend;
    todo-frontend = ./todo-frontend;
  };

  shells = {
    ghc = ["todo-common" "todo-backend" "todo-frontend"];
    ghcjs = ["todo-common" "todo-frontend"];
  };
})