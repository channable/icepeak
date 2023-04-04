{ aeson
, async
, base
, bytestring
, clock
, containers
, directory
, hashable
, hpack
, hspec
, hspec-wai
, http-types
, jwt
, mkDerivation
, monad-logger
, mtl
, network
, optparse-applicative
, pkgs
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
  version = "1.5.0";

  # Opt in to hpack. We don't commit the cabal file in our repo currently.
  buildTools = [ hpack ];
  preConfigure = "hpack .";

  src =
    let
      # Naive whitelist of file function. We could make this nicer eventually,
      # but this works. This also makes it obvious we do not run the integration
      # tests as part of building.
      srcPaths = builtins.map builtins.toString [
        ./package.yaml

        ./tests
        ./tests/AccessControlSpec.hs
        ./tests/ApiSpec.hs
        ./tests/CoreSpec.hs
        ./tests/JwtSpec.hs
        ./tests/OrphanInstances.hs
        ./tests/PersistenceSpec.hs
        ./tests/RequestSpec.hs
        ./tests/SocketSpec.hs
        ./tests/Spec.hs
        ./tests/StoreSpec.hs
        ./tests/SubscriptionTreeSpec.hs

        ./src
        ./src/AccessControl.hs
        ./src/Config.hs
        ./src/Core.hs
        ./src/HTTPMethodInvalid.hs
        ./src/HttpServer.hs
        ./src/JwtAuth.hs
        ./src/JwtMiddleware.hs
        ./src/Logger.hs
        ./src/Metrics.hs
        ./src/MetricsServer.hs
        ./src/Persistence.hs
        ./src/SentryLogging.hs
        ./src/Server.hs
        ./src/Store.hs
        ./src/Subscription.hs
        ./src/WebsocketServer.hs

        ./app
        ./app/Icepeak
        ./app/Icepeak/Main.hs
        ./app/IcepeakTokenGen
        ./app/IcepeakTokenGen/Main.hs
      ];
    in
      builtins.filterSource
        (path: (type: builtins.elem path srcPaths))
        ./.;

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

  testHaskellDepends = [
    hspec
    hspec-wai
    QuickCheck
    quickcheck-instances
  ];

  license = pkgs.lib.licenses.bsd3;
}
