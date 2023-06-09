# Optionally put the specified nix version of the package in the environment
{ environment ? "shell" }:
let
  pkgs = import ./nix/nixpkgs-pinned.nix { };

  defaultEnv = pkgs.haskellPackages.shellFor {
    packages = p: [ p.icepeak p.icepeak-client ];

    buildInputs = [
      pkgs.niv

      # Used for the integration tests
      (pkgs.python310.withPackages (p: [ p.websockets p.requests ]))

      # TODO: Replace Stack with Cabal
      # pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.implicit-hie
      pkgs.haskellPackages.stack
    ];

    withHoogle = true;
  };

  environments = { shell = defaultEnv; };
in environments."${environment}"
