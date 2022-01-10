## :Author: Don-Duong Quach
## :License: MIT
## :Version: 0.6.0
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
## Expansion
## ---------
## The ``~`` operator will expand an array, seq, or tuple, when in the body.
runnableExamples:
  type Color = enum
    none = 0 
    red = 1
    green = 2
    blue = 3

  let color = blue

  gen(c = [none, red, green, blue]):
    var varVal = gen(~c):
      case color:
        of it: %it

    let letVal = gen(~c):
      case varVal:
        of %it: it
        else: none 

  doAssert varVal == 3
  doAssert letVal == blue
##
## Type Fields
## -----------
## Passing in a type prefixed with the fields operator, ``+``, will pass in the 
## type's fields as unnamed items. Internally this uses ``genWith``, and
## only supports Enum and Object types.
runnableExamples:
  type ColorIndex = enum
    none = -1
    red = 1
    green = 2
    blue = 3

  gen +ColorIndex:
    var `^it[0]` = ($$it[0], it[1])
  
  doAssert None == ("none", -1)
  doAssert Red == ("red", 1)
  doAssert Green == ("green", 2)
  doAssert Blue == ("blue", 3)

  type Color = object
    r, g, b: uint8
  
  var c: Color

  gen +Color:
    c.it = 255'u8
  
  doAssert c.r == 255'u8
  doAssert c.g == 255'u8
  doAssert c.b == 255'u8
##
## Multiple Statements and Nesting
## -------------------------------
## When using multiple states in a gen macro, each statement will be produced at least 
## once even if there are no arguments. And each statement will be produced once 
## for each unnamed argument using the item identifier, unless its a special construct
## like a ``case`` statement or type definition. It's better to split the ``gen``
## calls for clarity if possible.
runnableExamples:
  gen(c = Component):
    gen(red, green, blue):
      type
        RGB = object
          `it c`: float

    var color = RGB(`red c`:1.0, `green c`:0.0, `blue c`:1.0)

  doAssert color.redComponent == 1
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

  gen(c = Component):
    gen(red, green, blue):
      type
        RGB = object
          `it c`: float

    var color = RGB(`red c`:1.0, `green c`:0.0, `blue c`:1.0)

  doAssert color.redComponent == 1
##
## Debugging with ``print``
## ------------------------
## To see what ``gen`` produces, wrap it with the ``print`` macro.
runnableExamples:
  var sum = 0
  print:
    gen(1, 2, 3):
      sum += it
    
  doAssert sum == 6

  gen(1, 2, 3):
    sum += it
  doAssert sum == 12
#
# implementation
# To extend the DSL: 
#   Capture what we're parsing in Nim to genit's Context nodes (NimNode -> Context)
#     Modify the `parse` functions, update ContextKind / Context
#     New operations need to be updated in parsePrefix and parseAccQuoted
#     Context should set `hasItem` if it is an item or has one as a descendant.
#   Once we have the entire Context tree, we can transform back to NimNodes
#     Modify tf functions for traversing the Context tree
#     Modify `tfOp` for operations

import std / [ macros, tables, strformat, genasts, strutils ]
from sequtils import anyIt

#> Debug
proc space(count: int): string =
  for i in 0..<count:
    result.add "  "

var debugFlag {.compileTime.} = false

macro debug*(body: untyped): untyped =
  debugFlag = true
  result = genast(body):
    body
    static:
      debugFlag = false

proc decho(count:int, msg:string) {.compileTime, used.} =
  if debugFlag:
    echo space(count), msg

proc decho(msg:string) {.compileTime, used.} =
  decho 0, msg

var printFlag {.compileTime.} = false
macro print*(body: untyped): untyped =
  ## This will print out what `gen` produces for debugging purposes.
  ## Wrap the ``gen`` call with print.
  printFlag = true
  result = genast(body):
    body
    static:
      printFlag = false

#< Debug

const MacroTransformerName = "gen"
const MacroTransformerWithName = "genWith"
const ItemStackName = "genItemStack"
const ItsName = "it"
const StringifyPrefix = "$$"
const ItIndexPrefix = "%"
const CapitalizePrefix = "^"
const ExpandPrefix = "~"
const FieldsPrefix = "+"
const Operators = [ StringifyPrefix, ItIndexPrefix, CapitalizePrefix ]
const AccQuotedOperators = [ ItIndexPrefix, CapitalizePrefix ] # operators that work in accQuoted

const EmptyKinds = { nnkNone, nnkEmpty, nnkNilLit }
const IntKinds = { nnkCharLit..nnkUint64Lit }
const FloatKinds = { nnkFloatLit..nnkFloat64Lit }
const StrLitKinds = { nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym }
const LitKinds = EmptyKinds + IntKinds + FloatKinds + StrLitKinds

type ContextKind = enum
    ckEmpty
    ckNode
    ckGen
    ckIt
    ckNamed
    ckAccQuoted
    ckOpTupleIndex # it[n]
    ckOpStringify # $$it = item -> "item"
    ckOpItIndex # %it => item index
    ckOpCapitalize # ^it = item -> Item
    ckVarSection
    ckCaseStmt
    ckOfBranch
    ckTypeSection
    ckTypeDef
    ckEnumTy
    ckObjectTy
    ckRecList
  
type Scope = object
    itsName: NimNode
    named: Table[string, NimNode]
    itemStack: NimNode # itemStack = array[ tuple[itsName:ident, items:array[ident], itIndex: intLit] ]

iterator itScopes(s: Scope): NimNode =
  # iterates over the itemStack in reverse
  for i in countDown(s.itemStack.len - 1, 0):
    yield s.itemStack[i]

var scope {.compileTime.}: Scope

proc itsName(itemScope: NimNode): Nimnode {.inline.} =
  itemScope[0]

proc items(itemScope: NimNode): NimNode {.inline.} =
  itemScope[1]

proc itIndex(itemScope: NimNode): NimNode {.inline.} =
  itemScope[2]

proc index(itemScope: NimNode): int {.inline.} =
  itemScope[2].intVal.int

proc item(itemScope: NimNode): NimNode {.inline.} =
  itemScope.items[itemScope.index]

type Context = object
    nk: NimNodeKind # output kind
    output: NimNode # cached output
    hasItem: bool
    children: seq[Context]
    case kind: ContextKind
      of ckGen:
        callsite: NimNode
      of ckIt:
        itsName: NimNode
      of ckOpTupleIndex:
        tupleIndex: int
      else: discard

#> Parsing functions

#> Parsing helper functions

proc propHasItem(c:var Context): Context {.discardable.} =
  for child in c.children:
    if child.hasItem:
      c.hasItem = true
      break

  if not c.hasItem:
    var n = newNimNode(c.nk)
    for child in c.children:
      n.add child.output
    c.output = n
  c

#< Parsing helper functions

proc parseNode(n: NimNode): Context

proc parseMulti(n: NimNode, ck: ContextKind = ckNode): Context =
  #decho &"parseMulti {ck} {n.kind}"
  result = Context(kind: ck, nk: n.kind)
  for child in n:
    result.children.add parseNode(child)
  result.propHasItem()


proc parseGen(n:NimNode): Context =
  #decho &"parseGen {n.repr}"
  result = Context(kind: ckGen, nk: nnkStmtList, callsite: n, hasItem: true)

proc parseIdentKind(n: NimNode): Context =
  decho "identKind"
  for itemScope in scope.itScopes:
    if n.eqIdent(itemScope.itsName):
      #decho &"--ckIt nnkIdent {n.repr}"
      return Context(nk: nnkIdent, kind: ckIt, itsName: n, hasItem: true)

  for lhs, rhs in scope.named:
    if n.eqIdent(lhs):
      #decho &"--ckNamed nnkIdent ({lhs} => {rhs.repr})"
      return Context(nk: nnkIdent, kind: ckNamed, output: rhs)

  #decho &"--ckNode {n.repr}"
  Context(kind: ckNode, nk: nnkIdent, output: n)


proc parseLitKind(n: NimNode): Context =
  result = case n.kind:
  of nnkIdent: 
    parseIdentKind(n)
  else:
    #decho &"LitKind {n.kind} {n.repr}"
    Context(nk: n.kind, kind: ckNode, output: n)


type AccQuotedState = enum
  aqRoot
  aqIt
  aqBracketOpen
  aqTupleIndex
  aqPrefix


proc addOpChainToContext(root:var Context, chain: var seq[Context]) =
  if chain.len > 0:
    var c = chain[0]
    # construct the chain and add it to root
    for i in 1..<chain.len:
      var parentC = chain[i]
      parentC.children.add c
      parentC.hasItem = c.hasItem
      c = parentC
    root.children.add c
  chain.setLen 0

proc parseAccQuoted(n: NimNode): Context =
  #decho &"parseAccQuoted"
  #decho n.treeRepr
  result = Context(kind: ckAccQuoted, nk: nnkAccQuoted)

  var chain: seq[Context] # points to the current child chain for the root
  var tupleIndex: int
  var state: AccQuotedState = aqRoot

  for id in n:
    #decho &"--> {state} '{id.kind = }' '{id.repr = }'"
    var childContext = parseNode(id)
    case state:
    of aqRoot:
      result.addOpChainToContext(chain)
      if childContext.hasItem:
        state = aqIt
        chain.add childContext
      elif AccQuotedOperators.anyIt(it in childContext.output.strVal):
        state = aqPrefix
        let op = childContext.output.strVal
        assert op.len == 1, "only one prefix operator allowed"
        if op == ItIndexPrefix:
          childContext = Context(kind: ckOpItIndex)
        elif op == CapitalizePrefix:
          childContext = Context(kind: ckOpCapitalize)
        chain.add childContext
      elif StringifyPrefix in childContext.output.strVal:
        error(&"Stringify operator, '$$', not allowed in identifier", n)
      else:
        # stay at root
        result.children.add childContext
    of aqIt:
      assert not childContext.hasItem, "item identifier appeared again?"
      if childContext.output.strVal == "[":
        state = aqBracketOpen
      else:
        result.addOpChainToContext(chain)
        result.children.add childContext
        state = aqRoot
    of aqBracketOpen:
      # only accept ints
      try:
        tupleIndex = parseInt(childContext.output.strVal)
      except ValueError:
        error(&"Tuple index must be a static int.", n)
      state = aqTupleIndex
    of aqTupleIndex:
      assert childContext.output.strVal == "]"
      var itContext = chain[0]
      var rest = chain[1..^1]
      chain.setLen 0
      chain.add( @[itContext, Context(kind: ckOpTupleIndex, tupleIndex: tupleIndex, hasItem: true)] )
      chain.add rest
      state = aqIt
    of aqPrefix:
      chain.insert(childContext, 0)
      if childContext.hasItem:
        state = aqIt
      else:
        state = aqRoot

  result.addOpChainToContext(chain)
  result.propHasItem()
  #if not result.hasItem:
    #decho &"parseAccQuoted output: {result.output.repr}"


proc parseBracketExpr(n: NimNode): Context =
  # first child:
  #   BracketExpr it[0][1]
  #   Ident array[0] # not hasItem
  #   Ident it[0] # hasItem
  # second child:
  #   IntLit it[0]
  #
  # need to parse first child and see if it has it
  #  if yes then ContextKind is ckOpTupleIndex
  #  else ckNode
  var first = parseNode(n[0])
  var second = parseNode(n[1])
  if first.hasItem:
    result = Context(kind: ckOpTupleIndex, nk: n.kind, tupleIndex: second.output.intVal.int, children: @[first])
  else:
    # array[..it..], array[expr]
    result = Context(kind: ckNode, nk: n.kind, children: @[first, second])
  result.propHasItem()

proc parsePrefix(n: NimNode): Context =
  #decho &"parsePrefix {n.repr}"
  var prefix = n[0].strVal
  var exp = n[1]
  if prefix in Operators:
    var child = parseNode(exp)
    if child.hasItem:
      case prefix:
      of StringifyPrefix:
        Context(kind: ckOpStringify, hasItem: true, children: @[child])
      of ItIndexPrefix:
        Context(kind: ckOpItIndex, hasItem: true, children: @[child])
      of CapitalizePrefix:
        Context(kind: ckOpCapitalize, hasItem: true, children: @[child])
      else: 
        Context(kind: ckEmpty)
    else:
      assert child.output.kind == nnkIdent
      case prefix:
      of StringifyPrefix:
        Context(kind: ckNode, output: newLit(child.output.repr))
      of CapitalizePrefix:
        Context(kind: ckNode, output: ident(child.output.strVal.capitalizeAscii))
      else:
        error(&"parsePrefix unexpected '{prefix}'", n)
        Context(kind: ckEmpty)
  else:
    parseMulti(n)


proc parseSection(n: NimNode): Context =
  #decho &"parseSection {n.repr}"
  if n.kind == nnkTypeSection:
    result = Context(kind: ckTypeSection, nk: n.kind)
  else:
    result = Context(kind: ckVarSection, nk: n.kind)
  for child in n:
    result.children.add parseNode(child)
  result.propHasItem()


proc parseNode(n: NimNode): Context =
  decho &"enter parseNode {n.kind}"
  result = case n.kind:
    of LitKinds: parseLitKind(n)
    of nnkCall, nnkCommand:
      if n[0].kind == nnkIdent and n[0].strVal == MacroTransformerName:
        parseGen(n)
      else:
        parseMulti(n)
    of nnkAccQuoted: parseAccQuoted(n)
    of nnkBracketExpr: parseBracketExpr(n)
    of nnkPrefix: parsePrefix(n)
    of nnkVarSection, nnkLetSection, nnkConstSection, nnkTypeSection: 
      parseSection(n)
    of nnkCaseStmt: parseMulti(n, ckCaseStmt)
    of nnkTypeDef: parseMulti(n, ckTypeDef)
    of nnkEnumTy: parseMulti(n, ckEnumTy)
    of nnkObjectTy: parseMulti(n, ckObjectTy)
    of nnkRecList: parseMulti(n, ckRecList)
    else: parseMulti(n)

  decho &"exit parseNode {result.kind} {result.nk} {result.hasItem}"


#< Parsing functions


#> AST Transformers
proc tf(c: Context): NimNode

proc tfIt(c: Context): NimNode =
  for itemScope in scope.itScopes:
    if itemScope.itsName.eqIdent(c.itsName):
      return itemScope.item
  error(&"Could not find \"{c.itsName}\" in scopes.")


proc tfInner(c: Context): seq[NimNode] =

  var itemScope = scope.itemStack[^1]
  if itemScope.items.len > 0:
    for i in 0 ..< itemScope.items.len:
      itemScope.itIndex.intVal = i
      var o = tf(c)
      if o != nil:
        result.add o
  else:
    var o = tf(c)
    if o != nil:
      result.add o


proc tfGen(c: Context): NimNode =
  # pass in named and unnamed items into the call
  result = newTree(nnkCall, ident(MacroTransformerName))
  
  for lhs, rhs in scope.named:
    result.add newTree(nnkExprEqExpr, ident(lhs), rhs)
  result.add newTree(nnkExprEqExpr, ident(ItemStackName), scope.itemStack.copyNimTree)
  result.add c.callsite[1..^1]


proc tfOp(c: Context): NimNode =
  #decho 0, "tfItem"
  var first = if c.children.len > 0 : c.children[0] else: Context(kind: ckEmpty)
  case c.kind:
  of ckOpTupleIndex:
    var n = first.tf()
    assert n.kind == nnkTupleConstr
    result = n[c.tupleIndex]
  of ckOpStringify:
    var n = first.tf()
    result = newLit(n.repr)
  of ckOpItIndex:
    assert first.kind == ckIt
    for itemScope in scope.itScopes:
      if itemScope.itsName.eqIdent(first.itsName):
        result = itemScope.itIndex
        break
  of ckOpCapitalize:
    var n = first.tf()
    result = ident(n.strVal.capitalizeAscii)
  else:
    discard


proc isOnLastItem(): bool =
  # Used to delay output if we have a container construct, e.g.: section, case, type
  if scope.itemStack[^1].len == 0: 
    return true
  else:
    scope.itemStack[^1].len - 1 == scope.itemStack[^1].index


proc tfVarSection(c: Context): NimNode =
  #decho &"tfVarSection {c.nk}"
  if isOnLastItem():
    result = newTree(c.nk)
    for defc in c.children:
      if defc.hasItem:
        result.add tfInner(defc)
      else:
        result.add defc.output


proc tfCase(c: Context): NimNode =
  # case should only return the entire case statement on the last index, of last scope item index
  if isOnLastItem():
    result = newTree(nnkCaseStmt, c.children[0].output)
    result.add tfInner(c.children[1])
    if c.children[^1].nk == nnkElse:
      result.add c.children[^1].tf()


proc tfTypeSection(c: Context): NimNode =
  #decho &"tfTypeSection {c.nk}"
  if isOnLastItem():
    result = newTree(c.nk)
    for defc in c.children:
      if defc.hasItem:
        result.add tf(defc)
      else:
        result.add defc.output

proc tfEnumTy(c: Context): NimNode =
  assert c.children.len == 2
  result = newTree(nnkEnumTy, c.children[0].output)
  var def = c.children[1]
  var itemScope = scope.itemStack[^1]
  for i in 0 ..< itemScope.items.len:
    itemScope.itIndex.intVal = i
    result.add tf(def)

proc tfObjectTy(c: Context): NimNode =
  result = newTree(nnkObjectTy, c.children[0].output, c.children[1].output) # 0: pragmas, 1: nnkOfInherit
  result.add tf(c.children[2])

proc tfRecList(c: Context): NimNode =
  result = newTree(nnkRecList)
  var itemScope = scope.itemStack[^1]
  for i in 0..<itemScope.items.len:
    itemScope.itIndex.intVal = i
    for def in c.children:
      result.add tf(def)

proc tf(c: Context): NimNode =
  decho &"tf {c.kind} {c.hasItem}"
  if c.hasItem:
    case c.kind:
    of ckIt: tfIt(c)
    of ckNamed: c.output
    of ckGen: tfGen(c)
    of ckOpTupleIndex, ckOpStringify, ckOpItIndex, ckOpCapitalize:
      tfOp(c)
    of ckVarSection: tfVarSection(c)
    of ckCaseStmt: tfCase(c)
    of ckTypeSection: tfTypeSection(c)
    of ckEnumTy: tfEnumTy(c)
    of ckObjectTy: tfObjectTy(c)
    of ckRecList: tfRecList(c)
    else:
      var n = newNimNode(c.nk)
      for child in c.children:
        let childTf = child.tf()
        if childTf != nil:
          n.add childTf
      n
  else:
    c.output
#< AST Transformers


macro gen*(va: varargs[untyped]): untyped =
  ## Implements the DSL.
  var body = va[^1]

  var args:seq[NimNode]
  if va.len > 1: 
    args = va[0..^2]

  # Check for fields operator on arguments.
  var fieldsIndex = -1
  var fieldsTy: NimNode
  var fieldsNamedArg: NimNode
  for i, a in args:
    if a.kind == nnkPrefix and a[0].strVal == FieldsPrefix:
      fieldsIndex = i
      fieldsTy = a[1]
      break
    elif a.kind == nnkExprEqExpr:
      var lhs = a[0]
      var rhs = a[1]
      if rhs.kind == nnkPrefix and rhs[0].strVal == FieldsPrefix:
        fieldsIndex = i
        fieldsTy = rhs[1]
        fieldsNamedArg = newTree(nnkExprEqExpr, lhs, fieldsTy)
        break

  if fieldsIndex > -1:
    args.del(fieldsIndex)

    result = nnkCall.newTree(
      ident(MacroTransformerWithName),
      fieldsTy)
    if fieldsNamedArg != nil: result.add fieldsNamedArg
    for a in args:
      result.add a
    result.add body
    return result


  var c = Context(kind: ckNode, nk: nnkStmtList)

  var itemScope = newTree(nnkTupleConstr, ident(ItsName), newNimNode(nnkBracket), newLit(0))
  scope = Scope(itsName: ident(ItsName))

  # process args
  for arg in args:
    if arg.kind == nnkExprEqExpr:
      var lhs = arg[0]
      var rhs = arg[1]
      decho "here"
      if lhs.kind == nnkIdent:
        if lhs.eqIdent(ItsName): # change its name
          itemScope[0] = rhs
        elif lhs.eqIdent(ItemStackName):
          scope.itemStack = rhs
        else:
          decho &"got {lhs.repr} = {rhs.repr}"
          scope.named[lhs.strVal] = rhs # todo: check if this a nim fragment
    elif arg.kind == nnkPrefix:
      # handle expansion
      var prefix = arg[0].strVal
      if prefix == ExpandPrefix:
        var group = arg[1].strVal

        if scope.named.hasKey(group):
          var xItems = scope.named[group]

          if xItems.kind == nnkPrefix and xItems[0].eqIdent("@"):
            xItems = xItems[1]
          if not(xItems.kind == nnkBracket or xItems.kind == nnkTupleConstr):
            error(&"Unexpected: {xItems.kind} for expansion, must be array, seq, or tuple", arg)
          for item in xItems:
            itemScope.items.add item
    else:
      itemScope.items.add arg

  if scope.itemStack.isNil:
    decho "new itemStack"
    scope.itemStack = newNimNode(nnkBracket)
  scope.itemStack.add itemScope
  decho "gen itemStack " & scope.itemStack.repr

  
  for s in body:
    c.children.add parseNode(s)

  c.propHasItem()


  #decho "-- transform"
  result = newNimNode(nnkStmtList)
  result.add c.tfInner()
  if printFlag:
    echo "-----"
    echo "gen ", va.repr
    echo ">>> result"
    echo result.repr
    echo ">>----"
    echo "itemStack post tf " & scope.itemStack.repr
    echo "------"

#> == it over type fields
proc isTypeDesc(n: NimNode): bool =
  var t = n.getType
  t.kind == nnkBracketExpr and t[0].kind == nnkSym and t[0].strVal == "typeDesc"


proc fieldsEnum(src, ty: NimNode): seq[NimNode] =
  # EnumTy
  for n in ty:
    case n.kind
    of nnkSym:
      result.add n
    of nnkEnumFieldDef:
      result.add nnkTupleConstr.newTree(n[0], n[1])
    of nnkEmpty: discard
    else:
      error(&"Unsupported {n.kind}", src)

proc fieldsObject(src, ty: NimNode): seq[NimNode] =
  # ObjectTy
  # Checks field accessibility.
  let isSameModule = src.lineInfoObj.filename == ty.lineInfoObj.filename

  var recList = ty[2]
  for def in recList:
    case def.kind:
    of nnkIdentDefs:
      for id in def[0 ..< ^2]:
        case id.kind:
        of nnkIdent, nnkSym:
          if isSameModule:
            result.add id
        of nnkPostFix:
          result.add id[1]
        else:
          error(&"Unexpected {id.kind = }", src)
    of nnkEmpty: discard
    else:
      error(&"Unexpected {def.kind = }", src)

macro genWith*(tySym: typed, args:varargs[untyped]): untyped =
  ## Calls ``gen`` with the fields of the enum or object type passed.
  ## Used by the fields operator, ``+``, with ``gen`` on a type.
  assert isTypeDesc(tySym)

  let body = args[^1]

  result = nnkCall.newTree(ident(MacroTransformerName))

  var fields: seq[NimNode]
  var impl = tySym.getImpl
  case impl.kind:
  of nnkTypeDef:
    var ty = impl[2]
    fields = case ty.kind:
    of nnkEnumTy: fieldsEnum(tySym, ty)
    of nnkObjectTy: fieldsObject(tySym, ty)
    else:
      error(&"Unexpected {ty.kind}", tySym)
      @[]
  else:
    error(&"Unexpected \"{impl.kind}\".", tySym)
  
  var tyArgName:NimNode
  if args.len > 1:
    for a in args[0 ..< ^1]:
      if a.kind == nnkExprEqExpr:
        var lhs = a[0]
        var rhs = a[1]
        if rhs.kind == nnkIdent and rhs.eqIdent(tySym):
          tyArgName = lhs
        else:
          result.add a
      else:
        result.add a

  if tyArgName != nil:
    var fieldsNode = newNimNode(nnkBracket)
    fieldsNode.add fields
    var namedArg = newTree(nnkExprEqExpr, tyArgName, fieldsNode)
    result.add namedArg
  else:
    result.add fields

  result.add body
  if printFlag:
    echo "genWith(", tySym.strVal, ",", args.repr, ")"
    echo ">>>"
    echo result.repr
    echo "-----"
#< == it over type fields