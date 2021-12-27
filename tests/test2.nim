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

test "nested":
  var sum = 0
  g 1, 2:
    g(it = this, 1, 2):
      sum += it + this
  
  check sum == 12