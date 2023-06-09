# Provide almost the same arguments as the actual nixpkgs.
# This allows us to further configure this nixpkgs instantiation in places where we need it.
{ overlays ? [ ] # additional overlays
, config ? { } # Imported configuration
}:
# Provides our instantiation of nixpkgs with all overlays and extra tooling
# that we pull in from other repositories.
# This expression is what all places where we need a concrete instantiation of nixpkgs should use.
let
  sources = import ./sources.nix;

  nixpkgs = import sources.nixpkgs {
    overlays = [ (import ./overlay.nix { inherit sources; }) ] ++ overlays;
    config = {
      imports = [ config ];

      allowUnfree = true;
    };
  };
in nixpkgs
