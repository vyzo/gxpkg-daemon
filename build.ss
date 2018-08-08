#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '("utils"
    "db"
    "providers/github"
    "server"
    (exe: "main" bin: "gxpkgd")))
