let pkgs = import ./nix/nixpkgs-pinned.nix { };
in {
  icepeak = pkgs.haskellPackages.icepeak;
  icepeak-client = pkgs.haskellPackages.icepeak-client;
}
