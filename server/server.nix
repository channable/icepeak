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
  version = "2.0.0";

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
        ./tests/Icepeak
        ./tests/Icepeak/Server
        ./tests/Icepeak/Server/AccessControlSpec.hs
        ./tests/Icepeak/Server/ApiSpec.hs
        ./tests/Icepeak/Server/CoreSpec.hs
        ./tests/Icepeak/Server/JwtSpec.hs
        ./tests/Icepeak/Server/PersistenceSpec.hs
        ./tests/Icepeak/Server/RequestSpec.hs
        ./tests/Icepeak/Server/SocketSpec.hs
        ./tests/Icepeak/Server/StoreSpec.hs
        ./tests/Icepeak/Server/SubscriptionTreeSpec.hs
        ./tests/OrphanInstances.hs
        ./tests/Spec.hs

        ./src
        ./src/Icepeak
        ./src/Icepeak/Server
        ./src/Icepeak/Server/AccessControl.hs
        ./src/Icepeak/Server/Config.hs
        ./src/Icepeak/Server/Core.hs
        ./src/Icepeak/Server/HTTPMethodInvalid.hs
        ./src/Icepeak/Server/HttpServer.hs
        ./src/Icepeak/Server/JwtAuth.hs
        ./src/Icepeak/Server/JwtMiddleware.hs
        ./src/Icepeak/Server/Logger.hs
        ./src/Icepeak/Server/Metrics.hs
        ./src/Icepeak/Server/MetricsServer.hs
        ./src/Icepeak/Server/Persistence.hs
        ./src/Icepeak/Server/SentryLogging.hs
        ./src/Icepeak/Server/Server.hs
        ./src/Icepeak/Server/Store.hs
        ./src/Icepeak/Server/Subscription.hs
        ./src/Icepeak/Server/WebsocketServer.hs

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
