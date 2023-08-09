with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    (ghc.withPackages(p: [ p.aeson p.lens p.cabal-install p.ghcid ]) )
    ghcid
  ];

}
