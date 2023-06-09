{ lib, pkgs, mkDerivation

# Core packages
, hpack, nix-gitignore

# Haskell packages
, aeson, async, base, bytestring, clock, containers, directory, hashable, hspec
, hspec-wai, http-types, jwt, monad-logger, mtl, network, optparse-applicative
, prometheus-client, prometheus-metrics-ghc, QuickCheck, quickcheck-instances
, random, raven-haskell, scotty, sqlite-simple, securemem, stm, text, time, unix
, unordered-containers, uuid, wai, wai-extra, wai-middleware-prometheus
, wai-websockets, warp, websockets }:

mkDerivation {
  pname = "icepeak";
  version = "2.0.0";

  # Opt in to hpack. We don't commit the cabal file in our repo currently.
  buildTools = [ hpack ];
  preConfigure = "hpack .";

  src = let
    # We do not want to include all files, because that leads to a lot of things
    # that nix has to copy to the temporary build directory that we don't want
    # to have in there (e.g. the `.dist-newstyle` directory, the `.git`
    # directory, etc.)
    prefixWhitelist =
      builtins.map builtins.toString [ ./package.yaml ./app ./src ./tests ];
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

  isLibrary = false;
  isExecutable = true;

  postInstall = ''
    # Remove output's lib/: This is only needed when building a library.
    # Doing this drastically reduces the output's closure size.
    rm --recursive --verbose $out/lib
  '';

  executableHaskellDepends = [
    aeson
    async
    base
    bytestring
    clock
    containers
    directory
    hashable
    http-types
    jwt
    monad-logger
    mtl
    network
    optparse-applicative
    prometheus-client
    prometheus-metrics-ghc
    random
    raven-haskell
    scotty
    sqlite-simple
    securemem
    stm
    text
    time
    unix
    unordered-containers
    uuid
    wai
    wai-extra
    wai-middleware-prometheus
    wai-websockets
    warp
    websockets
  ];

  testHaskellDepends = [ hspec hspec-wai QuickCheck quickcheck-instances ];

  license = lib.licenses.bsd3;
}
