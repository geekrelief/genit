import unittest

import g

#[
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
]#

type 
  State1 = object
    aComponent, bComponent, cComponent, dComponent: int

  State2 = object
    aComponent, cComponent: int

  Color = tuple
    r, g, b: uint8

#[
test "accQuoted":
  g red, green, blue:
    var `it State1`: State1 

  check declared(redState1)
]#

#[
test "mixed named and unnamed args":
  g red, green, blue:
    var `it State1`: State1 

  g red, green, blue:
    var `it State2`: State2
  greenState2.aComponent = 100 
  greenState2.cComponent = 300 

  g(c = component, g = greenState, a, c):
    `g 1`.`it c` = `g 2`.`it c` 
  
  check greenState1.aComponent == 100
  check greenState1.cComponent == 300

test "tuples":

  g.debug:
    g (red, (255, 0, 0)), (green, (0, 255, 0)), (blue, (0, 0, 255)):
      `it[0] State1`.aComponent = it[1][0]
      `it[0] State1`.bComponent = it[1][1]
      `it[0] State1`.cComponent = it[1][2]
  
  check redState1.aComponent == 255
  check greenState1.bComponent == 255
  check blueState1.cComponent == 255

  g (first, 1), (second, 2), (third, 3):
    var `it[0]` = it[1] 

  check first == 1
  check second == 2
  check third == 3


test "stringify and index":
  g red, green, blue:
    var it = ($$it, %it)

  check red == ("red", 0)
  check green == ("green", 1)
  check blue == ("blue", 2)


test "stringify named":
  g(l = Label, name, age):
    var `it l` = $$it & $$l
  check nameLabel == "nameLabel"
  check ageLabel == "ageLabel"

test "no args":
  g:
    var foo = "bar"
  check foo == "bar"
]#


test "capitalize":
  g red, green, blue:
    var `^it` = $$it
  
  check Red == "red"
  check Green == "green"
  check Blue == "blue"