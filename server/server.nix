{ aeson
, async
, base
, bytestring
, containers
, directory
, hashable
, hspec
, hspec-wai
, http-types
, jwt
, mkDerivation
, monad-logger
, mtl
, network
, optparse-applicative
, prometheus-client
, prometheus-metrics-ghc
, QuickCheck
, quickcheck-instances
, random
, raven-haskell
, scotty
, sqlite-simple
, securemem
, stdenv
, stm
, text
, time
, unix
, unordered-containers
, uuid
, wai
, wai-extra
, wai-middleware-prometheus
, wai-websockets
, warp
, websockets
}:

mkDerivation {
  pname = "icepeak";
  version = "0.6.2";
  src = ./.;

  isLibrary = false;
  isExecutable = true;

  executableHaskellDepends = [
    aeson
    async
    base
    bytestring
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

  testHaskellDepends = [
    hspec
    hspec-wai
    QuickCheck
    quickcheck-instances
  ];

  license = stdenv.lib.licenses.bsd3;
}
