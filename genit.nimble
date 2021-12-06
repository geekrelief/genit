# Package

version       = "0.5.0"
author        = "Don-Duong Quach"
description   = "A macro/DSL for inlining templates."
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.0.0"

task gendoc, "generate docs":
  exec "nim doc ./src/genit.nim"