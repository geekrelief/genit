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
    redState1.`this Component` = 100 + this_index
    var `s this` = str_this
  
  check redState1.aComponent == 100
  check redState1.bComponent == 101
  check redState1.cComponent == 102
  check sa == "a"
  check sb == "b"
  check sc == "c"

test "named args":
  redState2.aComponent = 100
  redState2.cComponent = 300

  gen(r = redState, c = Component, a, c):
    `/r 1`.`it /c` = `/r 2`.`it /c`
  
  check redState1.aComponent == 100
  check redState1.cComponent == 300

test "mixed named and unnamed args":
  greenState2.aComponent = 100 
  greenState2.cComponent = 300 

  gen(c = component, g = greenState, a, c):
    `/g 1`.`it /c` = `/g 2`.`it /c` 
  
  check greenState1.aComponent == 100
  check greenState1.cComponent == 300

test "tuples":
  gen (red, (255, 0, 0)), (green, (0, 255, 0)), (blue, (0, 0, 255)):
    `it0 State1`.aComponent = it1[0]
    `it0 State1`.bComponent = it1[1]
    `it0 State1`.cComponent = it1[2]
  
  check redState1.aComponent == 255
  check greenState1.bComponent == 255
  check blueState1.cComponent == 255

test "counter and stringify":
  gen red, green, blue:
    var it = (str_it, it_index)
  
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
      gen red, green, blue:
        `it /s 1`.`this /c` = `it /s 2`.`this /c` + it_index + this_index
  
  check redState1.aComponent == 100
  check redState1.cComponent == 301
  check greenState1.aComponent == 101
  check greenState1.cComponent == 302
  check blueState1.aComponent == 102
  check blueState1.cComponent == 303

test "stringify named":
  gen(l = Label, name, age):            # produces
    var `it /l` = str_it & /str_l # var nameLabel = "nameLabel"
                                  # var ageLabel = "ageLabel"
  check nameLabel == "nameLabel"
  check ageLabel == "ageLabel"

test "no args":
  gen:
    var foo = "bar"

  check foo == "bar"