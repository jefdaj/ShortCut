with import ./nixpkgs;

let
  shortcut = import ./default.nix;
  devDepends = [
    stack
    # pypi2nix
  ];

in haskell.lib.overrideCabal shortcut (drv: {

  buildDepends = drv.buildDepends ++ devDepends;

  # TODO this isn't being run by overrideCabal at all. get it to work
  # TODO is the find command going? that could maybe make the difference
  shellHook = ''
    ${drv.shellHook or ""}
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8
    export LANGUAGE=en_US.UTF-8
    # export TASTY_HIDE_SUCCESSES=True
  '' ++
  (if stdenv.hostPlatform.system == "x86_64-darwin" then "" else ''
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
  '');

})