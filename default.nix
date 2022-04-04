{ pkgs ? import <nixpkgs> { } }:
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides =
      let
        package = name: path: args:
          (self: super: {
            "${name}" = (super.callCabal2nix name path {
              optparse-applicative = super.optparse-applicative_atools;
            }).overrideAttrs (_: {
              postInstall = ''
                exe=${name}
                mkdir -p $out/share/bash-completion/completions
                $out/bin/$exe --bash-completion-script $exe >$out/share/bash-completion/completions/$exe
              '';
            });
          });
      in
      pkgs.lib.composeManyExtensions
        [
          (self: super: {
            optparse-applicative_atools = pkgs.haskell.lib.dontCheck (super.callCabal2nix "optparse-applicative" ./optparse-applicative { });
          })
          (package "abackend" ./abackend { })
          (package "acalendar" ./acalendar { })
          (package "anotes" ./anotes { })
          (package "astorage" ./astorage { })
        ];
  };
in
rec {
  inherit (haskellPackages)
    abackend
    acalendar
    anotes
    astorage;
  atools = [
    abackend
    acalendar
    anotes
    astorage
  ];
  shell = haskellPackages.shellFor {
    packages = (_: atools);
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ormolu
    ];
  };
}
