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

      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.cabal-fmt
      pkgs.haskellPackages.channalu
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.implicit-hie

      # for icepeak-ts-client dev tools, npm, npx
      pkgs.nodejs_20

      # for icepeak-ts-client langauge server
      pkgs.nodePackages_latest.typescript-language-server
      pkgs.nodePackages_latest.typescript
    ];

    withHoogle = true;
  };

  environments = { shell = defaultEnv; };
in environments."${environment}"
