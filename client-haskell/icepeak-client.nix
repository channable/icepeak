{ lib, mkDerivation

# Core packages
, hpack, nix-gitignore

# Haskell packages
, aeson, base, binary, bytestring, http-client, http-types, text, retry
, exceptions }:
mkDerivation {
  pname = "icepeak-client";
  version = "0.1.1";

  # Opt in to hpack. We don't commit the cabal file in our repo currently.
  buildTools = [ hpack ];
  preConfigure = "hpack .";

  src = let
    # We do not want to include all files, because that leads to a lot of things
    # that nix has to copy to the temporary build directory that we don't want
    # to have in there (e.g. the `.dist-newstyle` directory, the `.git`
    # directory, etc.)
    prefixWhitelist = builtins.map builtins.toString [ ./package.yaml ./src ];
    # Compute source based on whitelist
    whitelistFilter = path: _type:
      lib.any (prefix: lib.hasPrefix prefix path) prefixWhitelist;
    gitignore = builtins.readFile ./.gitignore;
    gitignoreFilter =
      nix-gitignore.gitignoreFilterPure whitelistFilter gitignore ./.;
    whitelistedSrc = lib.cleanSourceWith {
      src = lib.cleanSource ./.;
      filter = gitignoreFilter;
    };
  in whitelistedSrc;

  isLibrary = true;
  isExecutable = false;

  libraryHaskellDepends = [
    aeson
    base
    binary
    bytestring
    http-client
    http-types
    text
    retry
    exceptions
  ];

  homepage = "https://github.com/channable/icepeak";

  license = lib.licenses.bsd3;
}
