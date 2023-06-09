{ sources ? import ./sources.nix }:
self: super:
let
  # Overrides all haskell packages to not automatically add cost centers.
  # Note: profilingDetail is ends up as the `--profiling-detail` cabal flag,
  # and as the `-fno-prof-auto` ghc flag. In nixpkgs this is set to
  # "exported-functions" by default
  disableAutoProfs = haskellSelf: haskellSuper: {
    mkDerivation = drv:
      haskellSuper.mkDerivation (drv // { profilingDetail = "none"; });
  };

  haskellOverlay = import ./haskell-overlay.nix {
    inherit sources;
    pkgs = self;
  };
in {
  sources = if super ? sources then super.sources // sources else sources;

  haskellPackages = (super.haskellPackages.extend haskellOverlay)
  # Uncomment the following line to disable automatic cost centers for
  # libraries. Use this for profiling. Note: this will trigger a large
  # recompile!
  # .extend disableAutoProfs
  ;
}
