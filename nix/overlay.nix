# Autogenerated from euler.yaml. Do not edit.

self: super:
let
  beam-mysql-src = super.eulerBuild.allowedPaths {
    root = ./..;
    paths = [
      ../src
      ../test
      ../db
      ../pool
      ../beam-mysql.cabal
      ../LICENSE
    ];
  };

  bytestring-lexing-repo = builtins.fetchTarball {
    url = "https://github.com/juspay/bytestring-lexing/archive/0a46db1139011736687cb50bbd3877d223bcb737.tar.gz";
    sha256 = "1jrwhlp8xs4m21xfr843278j3i7h4sxyjpq67l6lzc36pqan9zlz";
  };
  bytestring-lexing-path = bytestring-lexing-repo;

  mysql-haskell-repo = builtins.fetchTarball {
    url = "https://github.com/juspay/mysql-haskell/archive/788022d65538db422b02ecc0be138b862d2e5cee.tar.gz";
    sha256 = "030qq1hgh15zkwa6j6x568d248iyfaw5idj2hh2mvb7j8xd1l4lv";
  };
  mysql-haskell-path = mysql-haskell-repo;

  isDarwin = super.stdenv.isDarwin;
linuxBuildTools = with super; lib.optionals (!isDarwin) [ mysql57 numactl ];
dontCheckDarwin =
  if isDarwin then super.haskell.lib.dontCheck else x: x;
in
super.eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper: {
    record-dot-preprocessor = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "record-dot-preprocessor";
        ver = "0.2.7";
        sha256 = "0dyn5wpn0p4sc1yw4zq9awrl2aa3gd3jamllfxrg31v3i3l6jvbw";
      } { });
    };
    bytestring-lexing = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "bytestring-lexing" bytestring-lexing-path { });
    };
    mysql-haskell = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "mysql-haskell" mysql-haskell-path { });
    };
    binary-parsers = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.binary-parsers);
    };
    wire-streams = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.wire-streams);
    };
    mason = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "mason";
        ver = "0.2.3";
        sha256 = "1dcd3n1lxlpjsz92lmr1nsx29mwwglim0gask04668sdiarr3x1v";
      } { });
    };
    
    beam-mysql = dontCheckDarwin (super.haskell.lib.addBuildTools (self.eulerBuild.fastBuild {
      drv = super.haskell.lib.addBuildTools (hself.callCabal2nix "beam-mysql" beam-mysql-src { }) (with self; [ coreutils zlib ]);
      overrides = {
        # We want to run tests for our packages most of the time
        runTests = true;
      };
    }) linuxBuildTools);
  })
