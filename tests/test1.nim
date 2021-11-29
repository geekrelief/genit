# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import genit

type 
  State1 = object
    aComponent, bComponent, cComponent, dComponent: int

  State2 = object
    aComponent, cComponent: int

  Color = tuple
    r, g, b: uint8

gen red, green, blue:
  var `it State1`: State1 

gen red, green, blue:
  var `it State2`: State2

test "unnamed args: it":
  check declared(redState1)
  check declared(greenState1)
  check declared(blueState1)

  check declared(redState2)
  check declared(greenState2)
  check declared(blueState2)

test "renamed it":
  gen(it = this, a, b, c):
    redState1.`this Component` = 100 + %this
    var `s this` = $$this
  
  check redState1.aComponent == 100
  check redState1.bComponent == 101
  check redState1.cComponent == 102
  check sa == "a"
  check sb == "b"
  check sc == "c"

test "mixed named and unnamed args":
  greenState2.aComponent = 100 
  greenState2.cComponent = 300 

  gen(c = component, g = greenState, a, c):
    `g 1`.`it c` = `g 2`.`it c` 
  
  check greenState1.aComponent == 100
  check greenState1.cComponent == 300

test "tuples":
  gen (red, (255, 0, 0)), (green, (0, 255, 0)), (blue, (0, 0, 255)):
    `it[0] State1`.aComponent = it[1][0]
    `it[0] State1`.bComponent = it[1][1]
    `it[0] State1`.cComponent = it[1][2]
  
  check redState1.aComponent == 255
  check greenState1.bComponent == 255
  check blueState1.cComponent == 255

  gen (first, 1), (second, 2), (third, 3):
    var `it[0]` = it[1] 

  check first == 1
  check second == 2
  check third == 3

test "stringify and index":
  gen red, green, blue:
    var it = ($$it, %it)
  
  check red == ("red", 0)
  check green == ("green", 1)
  check blue == ("blue", 2)

test "nested":
  redState2.aComponent = 100
  redState2.cComponent = 300
  greenState2.aComponent = 100
  greenState2.cComponent = 300
  blueState2.aComponent = 100
  blueState2.cComponent = 300

  gen(c = Component, s = State):
    gen(it = this, a, c):
      gen(it = that, red, green, blue):
        `that s 1`.`this c` = `that s 2`.`this c` + %that + %this
  
  check redState1.aComponent == 100
  check redState1.cComponent == 301
  check greenState1.aComponent == 101
  check greenState1.cComponent == 302
  check blueState1.aComponent == 102
  check blueState1.cComponent == 303

test "stringify named":
  gen(l = Label, name, age):
    var `it l` = $$it & $$l
  check nameLabel == "nameLabel"
  check ageLabel == "ageLabel"

test "no args":
  gen:
    var foo = "bar"
  check foo == "bar"

test "case":
  gen(c = Color):
    type c = enum
      `Red c`
      `Green c`
      `Blue c`
      `No c`

    var color1 = `Green c`
    proc getColor(color: c): (int, int, int) =
      gen (Red, (255, 0, 0)), (Green, (0, 255, 0)), (Blue, (0, 0, 255)):
        case color:
          of `it[0] c`: it[1]
          else: (0, 0, 0) 

    var index1 = getColor(color1)
    check index1 == (0, 255, 0)
    var index2 = getColor(`No c`)
    check index2 == (0, 0, 0)


test "typedef enum":
  gen Free, Carried, FlyingUp, FlyingBack:
    type 
      BoxState = enum
        `BoxState it`
      Verbs = enum
        `it`
  
  check ord(BoxStateFlyingBack) == 3

test "typedef object":
  gen(c = Component, red, green, blue):
    type
      RGB = object
        `it c`*: float

    var color = RGB(`red c`:1.0, `green c`:0.0, `blue c`:1.0)

  check color.redComponent == 1


test "capitalize":
  gen red, green, blue:
    var `^it` = $$it
  
  check Red == "red"