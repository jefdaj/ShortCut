# TODO rename modules.nix?

with import ./nix;
let
  # TODO can a lot/all of this be removed?
  environment = import ./environment.nix;

  # fixes "Fontconfig error: Cannot load default config file"
  # from nixpkgs/pkgs/development/libraries/pipewire/default.nix
  fontsConf = makeFontsConf {
    fontDirectories = [ ];
  };

  myR = rWrapper.override { packages = with rPackages; [
    dplyr
    ggplot2
    readr
    tidyr
    UpSetR
    VennDiagram
    biomartr
    data_table
    futile_logger
  ];};

  # TODO why is patching shebangs on the wrapped scripts necessary??
  mkMod = src: extraRunDeps: extraWraps:
    let name = "OrthoLang-" + baseNameOf src;
        runDeps = lib.lists.unique (environment ++ extraRunDeps);
    in stdenv.mkDerivation {
      inherit src name extraRunDeps extraWraps;
      NIX_LDFLAGS = "-lfontconfig"; # for R plotting scripts
      buildInputs = [ makeWrapper ] ++ runDeps;
      installPhase = ''
        source ${stdenv}/setup
        mkdir -p $out/bin
        for script in $src/*; do
          base="$(basename "$script")"
          dest="$out/bin/$base"
          substituteAll $script $dest
          chmod +x $dest
          wrapProgram $dest \
            --prefix PATH : "${pkgs.lib.makeBinPath runDeps}" ${extraWraps} \
            --set NIX_SSL_CERT_FILE "${cacert}/etc/ssl/certs/ca-bundle.crt" \
            --set FONTCONFIG_FILE "${fontsConf}"
        done
      '';

      # problem:  https://github.com/NixOS/nixpkgs/issues/11133
      # solution: https://github.com/NixOS/nixpkgs/pull/74942
      fixupPhase = if stdenv.isDarwin then ''
        for script in $out/bin/.*-wrapped; do
          patchShebangs "$script"
          substituteInPlace $script --replace "#!/nix" "#!/usr/bin/env /nix"
        done
      '' else ''
        for script in $out/bin/.*-wrapped; do
          patchShebangs $script
        done
      '';
    };

# only the modules list here is typically used,
# but keeping a set of the other attributes lets you enter shells for them
# (see modules-shell.nix for example)
in rec {
  myBlast = ncbi-blast; # for swapping it out if needed

  # TODO remove these in favor of buildPythonPath!
  myPy2Wrap = "--prefix PYTHONPATH : \"$out/bin:${myPy2.python.sitePackages}\"";
  myPy3Wrap = "--prefix PYTHONPATH : \"$out/bin:${myPy3.python.sitePackages}\"";

  myPy2 = python27.buildEnv.override {

    # see https://github.com/NixOS/nixpkgs/issues/22319
    ignoreCollisions = true;

    extraLibs = with python27Packages; [
      biopython # old version 1.76 with python2 support
      justorthologs
      numpy
      scipy
    ];
  };

  myPy3 = python3.buildEnv.override {

    # see https://github.com/NixOS/nixpkgs/issues/22319
    ignoreCollisions = true;

    extraLibs = with python3Packages; [
      # TODO add deterministic_zip
      biopython
      numpy # TODO remove?
      scipy # TODO remove?
    ];
  };

  ortholang-biomartr      = mkMod ./OrthoLang/Modules/BioMartR      [ myR ] "";
  ortholang-blasthits     = mkMod ./OrthoLang/Modules/BlastHits     [ myR ] "";
  ortholang-blastrbh      = mkMod ./OrthoLang/Modules/BlastRBH      [ myR ] "";
  ortholang-plots         = mkMod ./OrthoLang/Modules/Plots         [ myR ] "";
  ortholang-setstable     = mkMod ./OrthoLang/Modules/SetsTable     [ myR ] "";
  ortholang-range         = mkMod ./OrthoLang/Modules/Range         [ myR ] "";
  ortholang-blast         = mkMod ./OrthoLang/Modules/Blast         [ myBlast bzip2 parallel ] "";
  ortholang-blastdb       = mkMod ./OrthoLang/Modules/BlastDB       [ myBlast ] "";
  ortholang-blastdbget    = mkMod ./OrthoLang/Modules/Blastdbget    [ myBlast blastdbget ] ""; # TODO remove myBlast?
  ortholang-crbblast      = mkMod ./OrthoLang/Modules/CRBBlast      [ crb-blast ] "";
  ortholang-flowchart      = mkMod ./OrthoLang/Modules/FlowChart      [ graphviz ] ""; # TODO remove?
  ortholang-diamond       = mkMod ./OrthoLang/Modules/Diamond       [ diamond ] "";
  ortholang-hmmer         = mkMod ./OrthoLang/Modules/Hmmer         [ myPy2 hmmer ] myPy2Wrap;
  ortholang-justorthologs = mkMod ./OrthoLang/Modules/JustOrthologs [ myPy2 justorthologs ] myPy2Wrap;
  ortholang-mmseqs        = mkMod ./OrthoLang/Modules/MMSeqs        [ mmseqs2 ] "";
  ortholang-muscle        = mkMod ./OrthoLang/Modules/Muscle        [ muscle ] "";
  ortholang-psiblast      = mkMod ./OrthoLang/Modules/PsiBlast      [ myBlast ] "";
  ortholang-zip           = mkMod ./OrthoLang/Modules/Zip           [ zip myPy3 ] myPy3Wrap;

  # TODO should the wrap not be necessary?
  ortholang-seqio         = mkMod ./OrthoLang/Modules/SeqIO         [ myPy3 ] myPy3Wrap;
  # ortholang-orthofinder   = mkMod ./OrthoLang/Modules/OrthoFinder   [ myPy2 myBlast diamond orthofinder mcl fastme ] myPy2Wrap;
  ortholang-sonicparanoid = mkMod ./OrthoLang/Modules/SonicParanoid [ sonicparanoid ] myPy3Wrap;

  ortholang-treecl        = mkMod ./OrthoLang/Modules/TreeCl        [ myPy2 treeCl ] myPy2Wrap;

  # this config file is only a template; it needs to be completed by busco.sh at runtime
  ortholang-busco = mkMod ./OrthoLang/Modules/Busco
                     [ myBlast hmmer busco python36 which ]
                     "--set BUSCO_CONFIG_FILE ${busco}/config/config.ini";

  ortholang-curl          = mkMod ./OrthoLang/Modules/Curl          [ curl ] "";
  ortholang-load          = mkMod ./OrthoLang/Modules/Load          [ ] "";
  ortholang-orthogroups   = mkMod ./OrthoLang/Modules/OrthoGroups   [ python36 ] "";
  ortholang-greencut      = mkMod ./OrthoLang/Modules/GreenCut      [ myPy2 ] myPy2Wrap;

  # This is the only attribute used by the main build.
  modules = [
    ortholang-biomartr
    ortholang-blast
    ortholang-blastdb
    ortholang-blastdbget
    ortholang-blasthits
    ortholang-blastrbh
    ortholang-crbblast
    ortholang-flowchart
    ortholang-diamond
    ortholang-hmmer
    ortholang-justorthologs
    ortholang-mmseqs
    ortholang-muscle
    # ortholang-orthofinder
    ortholang-plots
    ortholang-setstable
    ortholang-psiblast
    ortholang-seqio
    ortholang-sonicparanoid
    # ortholang-treecl
    ortholang-busco
    ortholang-curl
    ortholang-load
    ortholang-range
    ortholang-orthogroups
    ortholang-greencut
    ortholang-zip
  ];
}
