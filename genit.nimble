# Package

version       = "0.17.0"
author        = "Don-Duong Quach"
description   = "A macro/DSL for inlining templates."
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.2"

task gendoc, "generate docs":
  exec "nim doc ./src/genit.nim"
  exec "cp ./src/htmldocs/genit.html ./src/htmldocs/geekrelief.github.io/genit/index.html"

task test, "run tests":
  exec "nim c -r ./tests/testgen.nim"

task genit, "test in genit":
  exec "nim c -r ./src/genit.nim"