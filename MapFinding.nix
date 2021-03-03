{ mkDerivation, base, hip, repa, stdenv }:
mkDerivation {
  pname = "MapFinding";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hip repa ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
