{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-brick = self.callPackage (import ./nix/reflex-brick.nix) {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./roguelike.nix {};
in
  drv
