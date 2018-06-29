let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-brick-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-brick.json;
    reflex-brick = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-brick";
      inherit (reflex-brick-info-pinned) rev sha256;
    };
  };
in
  sources.reflex-brick

