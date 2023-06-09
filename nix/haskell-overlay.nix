{ sources ? import ./sources.nix, pkgs }:
self: super: {
  icepeak = self.callPackage ../server/icepeak.nix { };
  icepeak-client = self.callPackage ../client-haskell/icepeak-client.nix { };
}
