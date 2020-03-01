let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "4c80c4c";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? gitignoreSource ./.
# Cabal project name
, name ? "zulip-archive"
, ...
}:

import rib { 
  inherit root name; 
  source-overrides = let 
    githubRepo = fq: rev: 
       builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
    dhallSrc = githubRepo "dhall-lang/dhall-haskell" "7aed7cf";
    prettyprinterSrc = githubRepo "quchen/prettyprinter" "320538b";
  in {
    dhall = dhallSrc + "/dhall";
    generic-random = githubRepo "Lysxia/generic-random" "1a091b6";
    prettyprinter = (import <nixpkgs> {}).runCommand "prettyprinter" {}
    ''
      cp -r -L ${prettyprinterSrc}/prettyprinter $out
    '';
  };
}
