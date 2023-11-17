{ sources ? import ./sources.nix, pkgs }:
self: super: {
  icepeak = self.callPackage ../server/icepeak.nix { };
  icepeak-client = self.callPackage ../client-haskell/icepeak-client.nix { };
  channalu =
    let
      fourmolu_args = [
        "--indentation=2"
        "--function-arrows=leading"
        "--comma-style=leading"
        "--import-export-style=diff-friendly"
        "--indent-wheres=false"
        "--record-brace-space=false"
        "--newlines-between-decls=1"
        "--haddock-style=single-line"
        "--haddock-style-module=single-line"
        "--let-style=mixed"
        "--in-style=left-align"
        "--unicode=never"
        "--respectful=true"
      ];
    # The executable is still called `fourmolu`, because this enables integration with HLS. HLS
    # recognises only a hardcoded set of pretty printers, because they have different CLIs. By
    # pretending that channalu is fourmolu, we reap the benefit of HLS knowing how to call it.
    in pkgs.writeShellScriptBin "fourmolu" ''
      exec ${self.fourmolu}/bin/fourmolu ${pkgs.lib.concatStringsSep " " fourmolu_args} "$@"
    '';
}
