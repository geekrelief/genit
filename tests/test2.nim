import unittest

import g

test "basic items":
  var sum = 0
  g 1, 2:
    sum += it
  
  check sum == 3

  g r, g, b:
    let it = 1
  
  check declared(r)
  check r == 1
  check declared(g)
  check g == 1
  check declared(b)
  check b == 1

test "nested, shadowed":
  var sum = 0
  g 1, 2:
    g(3, 5): # inner it shadows outer it, so we get 3+3, 5+5 twice
      sum += it + it
  
  check sum == 32

test "nested, renamed":
  var sum = 0
  g 1, 2:
    g(it = this, 3, 5):
      sum += it + this
  
  check sum == 22