## :Author: Don-Duong Quach
## :License: MIT
## :Version: 0.4.0
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

##
## Capitalize
## ----------
## The ``^`` operator will capitalize the first letter of an identifier.
runnableExamples:
  gen red, green, blue:
    var `^it` = $$it
  
  doAssert Red == "red"
  doAssert Green == "green"
  doAssert Blue == "blue"

##
## Special Constructs
## ------------------
## Some Nim constructs like ``case`` statements and type definitions: ``objects``, ``enum``, ``tuple``, etc.
## need special handling. Some might not be implemented currently.
runnableExamples:
  gen Free, Carried, FlyingUp, FlyingBack:
    type BoxState = enum
      `bs it`
  
  doAssert ord(bsFlyingBack) == 3

  type Color = enum
    Red
    Green
    Blue
    No

  var color1 = Green
  proc getColor(color: Color): (int, int, int) =
    gen (Red, (255, 0, 0)), (Green, (0, 255, 0)), (Blue, (0, 0, 255)):
      case color:
        of `it[0]`: it[1]
        else: (0, 0, 0) 

  var index1 = getColor(color1)
  doAssert index1 == (0, 255, 0)

  gen(c = Component, red, green, blue):
    type
      RGB = object
        `it c`: float

    var color = RGB(`red c`:1.0, `green c`:0.0, `blue c`:1.0)

  doAssert color.redComponent == 1

## Enum
## ----
## Iteration over enums can be done with ``enumGen``.
runnableExamples:
  type NumberColor = enum
    none = -1
    red = 1
    green = 2
    blue = 3

  enumGen(NumberColor):
    var `^it[0]` = ($$it[0], it[1])
  
  doAssert Red == ("red", 1)

#----- implementation
import std / [strutils, macros]

const genName = "gen"
const stringifyOp = "$$"
const indexOp = "%"
const capOp = "^"

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
    if (curNode.kind == nnkCommand or curNode.kind == nnkCall) and (curNode[0].eqIdent(genName)):
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

proc isIt(node: NimNode, it:NimNode): bool = 
  # check for item identifer in identNode or wrapped in accent quotes
  if node.eqIdent(it):
    return true
  if node.kind == nnkAccQuoted:
    for c in node:
      if c.eqIdent(it):
        return true
  return false

proc hasIt(node: NimNode, it:NimNode): bool =
  # recursively check if tree has item identifier
  if isIt(node, it):
    return true
  else:
    for s in node:
      if hasIt(s, it): return true
  return false

proc replaceIt(parentNode:NimNode, curNode:NimNode, it:NimNode, items:seq[NimNode]) =
  if curNode.hasIt(it):
    let sit = nnkPrefix.newTree(ident(stringifyOp), it)
    let cit = nnkPrefix.newTree(ident(capOp), it)
    let it_index = nnkPrefix.newTree(ident(indexOp), it)
    for index, item in items.pairs:
      var t = curNode.copyNimTree
      var tlen = items[0].len
      if tlen > 1:
        # greedy match for tuple item index first
        expectKind item, nnkTupleConstr
        for i in 0..<tlen:
          let itn = nnkBracketExpr.newTree(it, newLit(i)) # replace outside accented quote
          let sitn = nnkPrefix.newTree(ident(stringifyOp), itn)
          let citn = nnkPrefix.newTree(ident(capOp), itn)

          t = t.replace(sitn, newLit(item[i].repr))
          var citemn = item[i].repr
          citemn[0] = citemn[0].toUpperAscii

          # replace inside accented quote
          let caitn = @[ident(capOp), it, ident("["), ident($i), ident("]")] 
          t = t.replace(caitn, ident(citemn))
          let aitn = @[it, ident("["), ident($i), ident("]")]
          t = t.replace(aitn, item[i])

          t = t.replace(citn, ident(citemn))
          t = t.replace(itn, item[i])

      t = t.replace(sit, newLit(item.repr))

      var citem = item.repr
      citem[0] = citem[0].toUpperAscii
      t = t.replace(cit, ident(citem))
      let cait = @[ident(capOp), it] 
      t = t.replace(cait, ident(citem))

      t = t.replace(it_index, newLit(index))
      t = t.replace(it, item)
      #echo "t: ", t.repr
      parentNode.add t
  else:
    parentNode.add curNode.copyNimTree

proc recurseIt(parentNode:NimNode, curNode:NimNode, it:NimNode, items:seq[NimNode]) =
  case curNode.kind:
    of nnkCaseStmt:
      var caseNode = curNode.copyNimNode
      parentNode.add caseNode
      for b in curNode:
        case b.kind:
        of nnkOfBranch: replaceIt(caseNode, b, it, items)
        else: caseNode.add b.copyNimTree
    of nnkVarSection, nnkLetSection, nnkConstSection:
      var section = curNode.copyNimNode
      parentNode.add section
      #check if def has it on left and/or right
      for def in curNode:
        var hasItLeft = false
        var hasItRight = false
        for n in def[0 ..< ^1]:
          if n.hasIt(it):
            hasItLeft = true
            break
        if def[^1].hasIt(it):
          hasItRight = true
        
        # handle initialization via single it expression
        if not hasItLeft and hasItRight:
          var cdef = def.copyNimTree
          cdef.del(def.len-1)
          recurseIt(cdef, def[^1], it, items)
          section.add cdef
        else:
          replaceIt(section, def, it, items)

      #for def in curNode:
        #replaceIt(section, def, it, items)
    of nnkObjectTy:
      var objType = curNode.copyNimNode
      parentNode.add objType
      objType.add curNode[0].copyNimTree
      objType.add curNode[1].copyNimTree
      # loop over nnkRecList
      var recList = curNode[2].copyNimNode
      objType.add recList
      for rec in curNode[2]:
        recurseIt(recList, rec, it, items)
    of nnkEnumTy:
      var enumNode = curNode.copyNimNode
      for c in curNode:
        replaceIt(enumNode, c, it, items)
      parentNode.add enumNode
    of nnkIdentDefs, nnkConstDef:
      replaceIt(parentNode, curNode, it, items)
    of nnkTypeSection, nnkTypeDef:
      var node = curNode.copyNimNode
      parentNode.add node
      for c in curNode:
        recurseIt(node, c, it, items)
    else:
      replaceIt(parentNode, curNode, it, items)

macro gen*(args: varargs[untyped]): untyped =
  ## Implements the DSL
  expectMinLen args, 1
  expectKind args[^1], nnkStmtList

  # separate the named and unnamed arguments
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
      var l = ident(lhs[i].repr)
      s = s.replace(nnkPrefix.newTree(ident(stringifyOp), l), newLit(rhs[i].repr))
      var capr = rhs[i].repr
      capr[0] = capr[0].toUpperAscii()
      s = s.replace(nnkPrefix.newTree(ident(capOp), l), newLit(capr))
      s = s.replace(l, rhs[i])
    
    # unnamed args
    if items.len > 0:
      recurseIt(result, s, ident(itsName), items)
    else:
      result.add s

  #echo result.astGenRepr

macro enumGen*(arg: typed, body: untyped): untyped =
  ## Unwraps the enum fields in ``arg`` and passes the values to ``gen``.

  #echo arg.astGenRepr
  expectKind arg, nnkSym
  let impl = getImpl(arg)
  expectMinLen impl, 3
  let e = impl[2]
  expectKind e, nnkEnumTy

  result = nnkCall.newTree(ident("gen"))
  for f in e:
    if f.kind == nnkEmpty: continue

    if f.kind == nnkEnumFieldDef:
      result.add nnkTupleConstr.newTree(ident(f[0].repr), f[1])
    else:
      result.add f

  result.add body

  #echo result.astGenRepr