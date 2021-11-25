## :Author: Don-Duong Quach
## :License: MIT
##
## `Source <https://github.com/geekrelief/genit/>`_
##
## This module has a macro ``gen`` which implements a small DSL for inline code generation. ``gen`` acts
## like an anonymous, dirty, template/macro for generating lots of similar repetitive code.
## 
## Arguments are subsituted into a body of code using the following rules.
##
## Rules
## =====
##
## Named Arguments
## ---------------
## Named arguments are substituted into the body.
runnableExamples:
  gen(r = Radial, c = Component): # produces: 
    var `r c`: float              # var RadialComponent: float
  doAssert typeof(RadialComponent) is float
##
## Unnamed Arguments
## -----------------
## Unnamed arguments repeat the body and replaces the item identifier, ``it``, with the argument. 
runnableExamples:
  gen red, green, blue: # produces:
    var `it Val`: int   # var redVal: int
                        # var greenVal: int
                        # var blueVal: int
  doAssert typeof(redVal) is int
  doAssert typeof(greenVal) is int
  doAssert typeof(blueVal) is int
##
## Index
## -----
## The index for unnamed arguments can be accessed using the ``%`` operator.
runnableExamples:
  gen red, green, blue: # produces
    var it = %it        # var red = 0
                        # var green = 1
                        # var bluel = 2
  doAssert red == 0
  doAssert green == 1
  doAssert blue == 2
##
## Rename ``it``
## -------------
## The item identifier, ``it``, can be renamed using a named argument.
runnableExamples:
  gen(it = this, a, b): # produces:
    var this = 1        # var a = 1
                        # var b = 1
  doAssert a == 1
  doAssert b == 1
##
## Tuple Arguments
## ---------------
## Each unnamed argument can be a tuple, and its parts can be accessed by indexing like a regular tuple.
## If the indexed tuple is an l-value, it must be surrounded by accent quotes to be legal Nim.
runnableExamples:
  gen (first, 1), (second, 2), (third, 3): # produces:
    var `it[0]` = it[1]                    # var first = 1
                                           # var second = 2
                                           # var third = 3
  doAssert first == 1
  doAssert second == 2
  doAssert third == 3
##
## Stringify
## ---------
## The ``$$``, stringify, operator turns an identifier into a string.
runnableExamples:
  gen(l = Label, name, age): # produces
    var `it l` = $$it & $$l  # var nameLabel = "nameLabel"
                             # var ageLabel = "ageLabel"
  doAssert nameLabel == "nameLabel"
  doAssert ageLabel == "ageLabel"

import std / [strutils, macros]

# helper procs for gen
proc find(curNode:NimNode, match: seq[NimNode], index: int = 0): bool =
  if curNode.len - index >= match.len:
    for i in 0 ..< match.len:
      if curNode[index + i] != match[i]:
        return false
    return true
  else:
    return false 

proc replace(curNode: NimNode, match: NimNode, newNode: NimNode): NimNode =
  if curNode == match:
    return newNode
  else:
    if (curNode.kind == nnkCommand or curNode.kind == nnkCall) and (curNode[0].eqIdent("gen")):
      # gen is nested, only process the body of the gen not its arguments
      curNode[^1] = curNode[^1].replace(match, newNode)
    else:
      for i, c in curNode.pairs:
        curNode[i] = c.replace(match, newNode)

    return curNode

proc replace(curNode: NimNode, match: seq[NimNode], newNode: NimNode): NimNode =
  var i = 0
  result = curNode.copyNimNode
  # process children
  while i < curNode.len:
    if find(curNode, match, i):
      result.add newNode
      i += match.len
    else:
      result.add replace(curNode[i], match, newNode)
      inc i

macro gen*(args: varargs[untyped]): untyped =
  ## Implements the DSL
  expectMinLen args, 1
  expectKind args[^1], nnkStmtList

  # separate the named and unnamed arguments
  let stringifyOp = "$$"
  let indexOp = "%"
  var lhs, rhs, items: seq[NimNode]
  var body = args[^1].copyNimTree

  var itsName = "it"
  for arg in args[0 ..< ^1]:
    if arg.kind == nnkExprEqExpr:
      if arg[0].repr == itsName: # change its name
        itsName = arg[1].repr
      else:
        lhs.add arg[0]
        rhs.add arg[1]
    else:
      items.add arg
  
  # process the body one statement at a time
  result = nnkStmtList.newTree

  for stmt in body:
    var s = stmt

    # named args
    for i in 0 ..< lhs.len:
      s = s.replace(nnkPrefix.newTree(ident(stringifyOp), ident(lhs[i].repr)), newLit(rhs[i].repr))
      s = s.replace(ident(lhs[i].repr), rhs[i])

    # unnamed args
    if items.len == 0:
      result.add s
    else:
      var tlen = items[0].len
      var it = ident(itsName)
      var sit = nnkPrefix.newTree(ident(stringifyOp), ident(itsName))
      var it_index = nnkPrefix.newTree(ident(indexOp), ident(itsName))
      
      for index, item in items.pairs:
        var t = s.copyNimTree
        t = t.replace(sit, newLit(item.repr))
        t = t.replace(it_index, newLit(index))
        if tlen > 1:
          expectKind item, nnkTupleConstr
          for i in 0..<tlen:
            let itn = nnkBracketExpr.newTree(ident(itsName), newLit(i)) # replace outside accented quote
            let sitn = nnkPrefix.newTree(ident(stringifyOp), itn)
            t = t.replace(sitn, newLit(item[i].repr))
            t = t.replace(itn, item[i])
            let aitn = @[ident(itsName), ident("["), ident($i), ident("]")] # replace inside accented quote
            t = t.replace(aitn, item[i])
        t = t.replace(it, item)
        result.add t

  #echo result.repr