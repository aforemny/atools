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
            iCalendar = pkgs.haskell.lib.dontCheck (super.callCabal2nix "iCalendar" ./iCalendar { });
          })
          (package "abackend" ./abackend { })
          (package "acalendar" ./acalendar { })
          (package "anote" ./anote { })
          (package "astorage" ./astorage { })
          (package "atask" ./atask { })
          (package "atime" ./atime { })
        ];
  };
in
rec {
  inherit (haskellPackages)
    abackend
    acalendar
    anote
    astorage
    atask
    atime;
  atools = [
    abackend
    acalendar
    anote
    astorage
    atask
    atime
  ];
  shell = haskellPackages.shellFor {
    packages = (_: atools);
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ormolu
      pkgs.vdirsyncer
    ];
    shellHook = ''
      HISTFILE=${pkgs.lib.escapeShellArg ./.}/.history; export HISTFILE
    '';
  };
}
