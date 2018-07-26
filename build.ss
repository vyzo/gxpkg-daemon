#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '("server"
    (exe: "main" bin: "gxpkgd")))
