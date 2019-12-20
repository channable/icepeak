let
  pkgs = import ../nix/nixpkgs.nix {};
  icepeak-server = pkgs.haskellPackages.callPackage ./server.nix {};
in
  icepeak-server
