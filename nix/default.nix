# This is a bit of a transitional mess right now. Eventually, the bigger
# dependencies will go in their own git repos and the smaller ones will be
# moved to ortholang/nix. Either way, they'll be managed with niv.

let
  sources = import ./sources.nix {};
  pkgs = import sources.nixpkgs {};

  # TODO remove in favor of newer vanilla ncbi-blast?
  psiblast-exb = pkgs.callPackage sources.psiblast-exb { };

  easel = pkgs.callPackage sources.easel { };
  hmmer = pkgs.callPackage sources.hmmer { inherit easel; };

  ncbi-blast = pkgs.callPackage sources.ncbi-blast {}; # follows latest version (2.9.0 now)
  crb-blast  = pkgs.callPackage sources.crb-blast  { inherit ncbi-blast; }; # uses old blast v 2.2.29


  # see https://nixos.org/nix-dev/2014-December/015243.html
  muscle = pkgs.callPackage sources.muscle { };

  diamond = pkgs.callPackage sources.diamond { };

  mmseqs2 = pkgs.callPackage sources.mmseqs2 {};

  mcl    = pkgs.callPackage sources.mcl    { };
  fastme = pkgs.callPackage sources.fastme { };
  orthofinder = pkgs.callPackage sources.orthofinder {
    inherit (pkgs.lib) makeBinPath;
    inherit mcl fastme ncbi-blast diamond;
  };

  # TODO inherit mmseqs2 + mcl again? probably use an overlay to build pkgs instead
  # TODO also get python3Packages back?
  # sonicparanoid = pkgs.callPackage sources.sonicparanoid { inherit mmseqs2 mcl; };
  quick-multi-paranoid = pkgs.callPackage sources.quick-multi-paranoid {};
  sonicparanoid = pkgs.callPackage sources.sonicparanoid {
    inherit mmseqs2 mcl quick-multi-paranoid;
  };

  # TODO detect whether MPI version will work on a given computer and adjust
  raxml  = pkgs.callPackage sources.raxml { mpi = true; };

  # TODO upload and import via niv
  # TODO with treecl and blastdbget packaged separately, does the whole myPython2 set serve a purpose anymore?
  myPython2 = pkgs.python27Packages // rec {
    # TODO upload separate repo and switch to using it with niv
    blastdbget = pkgs.python27Packages.callPackage sources.blastdbget {};
    biopython  = pkgs.python27Packages.callPackage ./pydeps/biopython {}; # old version 1.76 with python2 support
  };

  # TODO upload repo and import via niv
  # TODO and probably put most of release.nix back here? or in its default.nix
  # TODO package biopython 1.76, or whatever the latest was to support python27, for treecl
  # treeCl = pkgs.python27Packages.callPackage ../../treecl-nix/release.nix {};

  myPython3 = pkgs.python3Packages // rec {
  };

  # TODO put back inside myPython3?
  busco = myPython3.callPackage sources.busco {
    inherit (pkgs.lib) makeBinPath;
    inherit ncbi-blast hmmer;
  };
  
  # TODO clean this up
  justorthologs = pkgs.callPackage sources.justorthologs {
    python = myPython2.python.withPackages (_: [myPython2.biopython]);
  };

  # TODO add a module, or remove this if not helpful

# TODO these should probably be converted to a list of overlays
in pkgs // {
  inherit ncbi-blast crb-blast psiblast-exb;
  inherit fastme;
  inherit diamond hmmer mmseqs2 muscle;

  # TODO will these interfere with each other?
  python27Packages = myPython2;
  python3Packages = myPython3;
  inherit (myPython2) blastdbget;
  inherit busco; # TODO back inside myPython3?
  inherit quick-multi-paranoid;

  inherit raxml mcl;
  inherit orthofinder sonicparanoid justorthologs;
}
