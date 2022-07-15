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
          (package "anote" ./anote { })
          (package "astorage" ./astorage { })
          (package "atask" ./atask { })
        ];
  };
in
rec {
  inherit (haskellPackages)
    abackend
    acalendar
    anote
    astorage
    atask;
  atools = [
    abackend
    acalendar
    anote
    astorage
    atask
  ];
  shell = haskellPackages.shellFor {
    packages = (_: atools);
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ormolu
    ];
  };
}
