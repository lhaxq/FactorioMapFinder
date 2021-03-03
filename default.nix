let
  pkgs = import
      (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz)
      { };
in
  { mapFinding = pkgs.haskellPackages.callPackage ./MapFinding.nix { };
  }
