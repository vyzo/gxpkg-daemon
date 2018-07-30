#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '("db"
    "server"
    (exe: "main" bin: "gxpkgd")))
