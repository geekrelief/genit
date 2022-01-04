import unittest

import g
import std/macros

test "no args":
  g:
    var foo = "bar"
  check foo == "bar"

test "bracket":
  let arr = [100,200,300]
  var a: int
  g:
    a = arr[2]

  check a == 300

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

type 
  State1 = object
    aComponent, bComponent, cComponent, dComponent: int

  State2 = object
    aComponent, cComponent: int

  Color = tuple
    r, g, b: uint8

test "accQuoted":
  g(v = value):
    var `v`: int = 1
  
  check declared(value)

  g red, green, blue:
    var `it State1`: State1 

  check declared(redState1)

test "unnamed args / enum":
  g(c = Color):
    type c = enum
      `Red c`
      `Green c`
      `Blue c`
      `No c`

  check declared(Color)
  check declared(RedColor)
  check declared(GreenColor)
  check declared(BlueColor)
  check declared(NoColor)

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

  g red, green, blue:
    var `it State1`: State1 

  g (red, (255, 0, 0)), (green, (0, 255, 0)), (blue, (0, 0, 255)):
    `it[0] State1`.aComponent = it[1][0]
    `it[0] State1`.bComponent = it[1][1]
    `it[0] State1`.cComponent = it[1][2]

  check redState1.aComponent == 255
  check greenState1.bComponent == 255
  check blueState1.cComponent == 255

test "two operators":
  g (first, 1), (second, 2), (third, 3):
    var `^it[0]` = $$it[1] 

  check First == "1"
  check Second == "2"
  check Third == "3"

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


test "capitalize":
  g red, green, blue:
    var `^it` = $$it
  
  check Red == "red"
  check Green == "green"
  check Blue == "blue"

test "case":
  type Color = enum
    Red
    Green
    Blue
    None
  
  let color = Green

  let index1 = g((Red, (255, 0, 0)), (Green, (0, 255, 0)), (Blue, (0, 0, 255))):
    case color:
      of `it[0]`: it[1]
      else: (0, 0, 0) 

  check index1 == (0, 255, 0)


test "case2":
  g(c = Color):
    type c = enum
      `Red c`
      `Green c`
      `Blue c`
      `No c`

    var color1 = `Green c`
    proc getColor(color: c): (int, int, int) =
      g (Red, (255, 0, 0)), (Green, (0, 255, 0)), (Blue, (0, 0, 255)):
        case color:
          of `it[0] c`: it[1]
          else: (0, 0, 0) 

    var index1 = getColor(color1)
    check index1 == (0, 255, 0)
    var index2 = getColor(`No c`)
    check index2 == (0, 0, 0)

test "typedef enum it":
  g Free, Carried, FlyingUp, FlyingBack:
    type 
      BoxState = enum
        `bs it`
      Verbs = enum
        `it`
  
  check ord(bsFlyingBack) == 3
  check ord(FlyingUp) == 2


test "typedef object it":
  g(c = Component):
    g red, green, blue:
      type
        RGB = object
          `it c`*: float
    var color = RGB(`red c`:1.0, `green c`:0.0, `blue c`:1.0)

  check declared(RGB)
  check color.redComponent == 1

test "expand operator":
  type Color = enum
    none = 0 
    red = 1
    green = 2
    blue = 3

  let color = blue

  g(c = [none, red, green, blue]):
    var varVal = g(~c):
      case color:
        of it: %it

    let letVal = g(~c):
      case varVal:
        of %it: it
        else: none 

  check varVal == 3
  check letVal == blue

test "case inner gen":
  type Color = enum
    red
    blue

  var val = red
  g(c = Color):
    var answer = case val:
      of red:
        g("red"):
          it
      else:
        g("blue"):
          it
  check answer == "red"

test "set const via it expression":
  g(none, red, green, blue):
    const `it Val` = %it
  
  check redVal == 1


test "with enum":
  template mypragma() {.pragma.}

  type Color {.mypragma.} = enum
    none
    red
    green
    blue

  gw Color:
    var `^it` = $$it
  
  check Red == "red"

test "with enum values":
  type NumberColor = enum
    nnone = -1
    nred = (1, "red")
    ngreen = "green"
    nblue = 3

  gw NumberColor:
    var `^it[0]` = ($$it[0], it[1])
  
  check Nred == ("nred", (1, "red"))
  check Ngreen == ("ngreen", "green")


import etype
test "with object":
  type Color = object
    r, g, b: uint8
  
  var c: Color

  gw Color:
    c.it = 255'u8
  
  check c.r == 255'u8
  check c.g == 255'u8
  check c.b == 255'u8

  var fc: FColor
  gw FColor:
    fc.it = 1f
  
  check fc.getR == 0f
  check fc.getG == 1f
  check fc.getB == 0f