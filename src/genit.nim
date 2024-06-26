## :Author: Don-Duong Quach
## :License: MIT
## :Version: 0.15.0
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
## genit process nnkAccQuoted identifiers, i.e. backticks surrounding identifiers, to generate
## substitutions into blocks of code. If the resulting accQuoted identifier is valid without
## backticks, then the backticks are removed. The accQuoted identifiers are also flattened if there's nesting.
##
## Named Arguments
## ---------------
## Named arguments are local to the gen call and substituted into the body.
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
    var `it Val`: int   # var 
                        #   redVal: int
                        #   greenVal: int
                        #   blueVal: int
  doAssert typeof(redVal) is int
  doAssert typeof(greenVal) is int
  doAssert typeof(blueVal) is int
##
## Global Named Arguments
## ----------------------
## Global named arguments can be assigned with the ``:=`` operator.
runnableExamples:
  #[ 
  # runnableExamples doesn't like `:=`, but the following works
  gen(c := Component) 

  gen a:
    var `it c`: int = 1
  
  gen b:
    var `c it`: int = 2
  
  doAssert aComponent == 1
  doAssert ComponentB == 2

  gen(c := Comp)

  gen d:
    var `it c`:int = 3
  
  doAssert dComp == 3
  ]#
  discard
##
## Index
## -----
## The index for unnamed arguments can be accessed using the ``%`` operator.
runnableExamples:
  gen red, green, blue: # produces
    var it = %it        # var 
                        #   red = 0
                        #   green = 1
                        #   bluel = 2
  doAssert red == 0
  doAssert green == 1
  doAssert blue == 2
##
## Rename ``it``
## -------------
## The item identifier, ``it``, can be renamed using a named argument.
runnableExamples:
  gen(it = this, a, b): # produces:
    var this = 1        # var 
                        #   a = 1
                        #   b = 1
  doAssert a == 1
  doAssert b == 1
##
## Tuple Arguments
## ---------------
## Each unnamed argument can be a tuple, and its parts can be accessed by indexing like a regular tuple.
## If the indexed tuple is an l-value, it must be surrounded by accent quotes to be legal Nim.
##
## If an unnnamed argument is indexed but it is not a tuple, it will be "duplicated", so you can mix tuple and non-tuple arguments.
##
## If the tuple index is part of a larger expression, e.g. dot expression, accent quoting will run genit's parser on it.  Accent quoting will also interrupt multiple indexing.
runnableExamples:
  gen (first, 1), (second, 2), (third, 3): # produces:
    var `it[0]` = it[1]                    # var 
                                           #   first = 1
                                           #   second = 2
                                           #   third = 3
  doAssert first == 1
  doAssert second == 2
  doAssert third == 3

  gen w, a, s, d, (shift, run):
    let `^it[0]` = $$it[1]
  doAssert W == "w"
  doAssert Shift == "run"

  type State = object
    foo: int
    bar: int
  
  var s = State(foo: 10, bar: 100)

  gen (val, foo), bar:
    let `it[0]` = s.`it[1]` # accent quote it[1] to bypass nim parsing
  
  doAssert val == 10
  doAssert bar == 100

  # multiple tuple indexing with duplication
  var a = [1, 2, 3]
  var b = ['a', 'b', 'c']

  gen (ap, a), b:
    let `it[0] t` = it[1][0].addr

  doAssert apt[] == a
  doAssert bt[] == b

  # interrupt tuple indexing
  gen (ap, a), b:
    let `it[0] p` = `it[1]`[0].addr

  doAssert app[] == 1
  doAssert bp[] == 'a'

##
## Stringify
## ---------
## The ``$$``, stringify, operator turns an identifier into a string.
runnableExamples:
  gen(l = Label, name, age): # produces
    var `it l` = $$it & $$l  # var 
                             #   nameLabel = "nameLabel"
                             #   ageLabel = "ageLabel"
  doAssert nameLabel == "nameLabel"
  doAssert ageLabel == "ageLabel"

##
## Lowercase
## ---------
## The ``-`` operator will lowercase the first letter of an identifier.
runnableExamples:
  gen Red, Green, Blue:
    var it = $$ -it
  
  doAssert Red == "red"
  doAssert Green == "green"
  doAssert Blue == "blue"

##
## Uppercase
## ----------------------
## The ``/`` operator will capitalize all the letters of an identifier.
runnableExamples:
  gen red, green, blue:
    var `^it` = $$ /it
  
  doAssert Red == "RED"
  doAssert Green == "GREEN"
  doAssert Blue == "BLUE"

##
## Capitalize
## ----------------------
## The ``^`` operator will capitalize the first letter of an identifier.
runnableExamples:
  gen red, green, blue:
    var `it` = $$ ^it

  doAssert red == "Red"
  doAssert green == "Green"
  doAssert blue == "Blue"

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
## Passing in a type or symbol prefixed with the fields operator, ``+``, will pass in the 
## type's fields as unnamed items. Internally this uses ``genWith`` and
## supports Enum, Object, Tuple, Array, and Range types.
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

  type
    Vec3 = tuple[x, y: float32, z: float32]
  var l = (10f, 20f, 30f) # tuple with no named fields
  var r:Vec3 = (0f, 0f, 0f)

  gen +l: # tuple constructor returns 0, 1, 2
    r[it] = l[it]

  doAssert r.x == 10f
  doAssert r.z == 30f

  r.x = 1f
  r.z = 3f

  var res:Vec3
  gen +r: # Vec3
    res.it = r.it

  doAssert res.x == 1f
  doAssert res.z == 3f

  var v4:array[4, float32] = [1'f32, 0.5, 0.25, 1]
  var v3:array[1..3, byte]

  gen +v3:
    v3[it] = (v4[it - 1] * 255.99f).byte

  doAssert v3[1] == 255
  doAssert v3[2] == 127
  doAssert v3[3] == 63
##
## Multiple Statements and Nesting
## -------------------------------
## When using multiple statements in a gen macro, each statement will be produced at least 
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

import std / [ macros, tables, strformat, genasts, strutils, options, sets ]
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
  echo body.treeRepr
  ## This will print out what `gen` produces for debugging purposes.
  ## Wrap the ``gen`` call with print.
  printFlag = true
  result = genast(body):
    body
    static:
      printFlag = false

#< Debug

# Used to check if a flattened, accQuoted identifier is valid
const Keywords = ["addr", "and", "as", "asm", 
"bind", "block", "break", 
"case", "cast", "concept", "const", "continue", "converter",
"defer", "discard", "distinct", "div", "do",
"elif", "else", "end", "enum", "except", "export",
"finally", "for", "from", "func", "if",
"import", "in", "include", "interface", "is", "isnot", "iterator",
"let",
"macro", "method", "mixin", "mod",
"nil", "not", "notin",
"object", "of", "or", "out",
"proc", "ptr",
"raise", "ref", "return",
"shl", "shr", "static",
"template", "try", "tuple", "type",
"using",
"var",
"when", "while",
"xor",
"yield"
]
const KeywordsSet = toHashSet(Keywords)

const MacroTransformerName = "gen"
const MacroTransformerWithName = "genWith"
const ItemStackName = "genItemStack"
const ItsName = "it"
const StringifyPrefix = "$$"
const ItIndexPrefix = "%"
const LowercasePrefix = "-"
const UppercasePrefix = "/"
const CapitalizePrefix = "^"
const ExpandPrefix = "~"
const FieldsPrefix = "+"
const GlobalInfix = ":="
const Operators = [ StringifyPrefix, ItIndexPrefix, LowercasePrefix, UppercasePrefix, CapitalizePrefix ]
const AccQuotedOperators = [ ItIndexPrefix, LowercasePrefix, UppercasePrefix, CapitalizePrefix ] # operators that work in accQuoted

var globalNamed {.compileTime.}: Table[string, NimNode]

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
    ckOpLowercase # -it = Item -> item
    ckOpUppercase # ^it = item -> ITEM
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
    output: Option[NimNode] # cached output
    hasItem: bool
    isAccQuoted: bool # used to distinguish DSL operations from Nim syntax
    children: seq[Context]
    case kind: ContextKind
      of ckGen:
        callsite: NimNode
      of ckIt:
        itsName: NimNode
      of ckOpTupleIndex:
        tupleIndex: int
      of ckTypeDef:
        isIt: bool
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
      if child.output.isSome:
        n.add child.output.get
    c.output = some(n)
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
  #decho "identKind"
  for itemScope in scope.itScopes:
    if n.eqIdent(itemScope.itsName):
      #decho &"--ckIt nnkIdent {n.repr}"
      return Context(nk: nnkIdent, kind: ckIt, itsName: n, hasItem: true)

  for lhs, rhs in scope.named:
    if n.eqIdent(lhs):
      #decho  &"--ckNamed nnkIdent ({lhs} => {rhs.repr})"
      return Context(nk: nnkIdent, kind: ckNamed, output: some(rhs))

  for lhs, rhs in globalNamed:
    if n.eqIdent(lhs):
      #decho &"--ckNamed nnkIdent global ({lhs} => {rhs.repr})"
      return Context(nk: nnkIdent, kind: ckNamed, output: some(rhs))

  #decho &"--ckNode {n.repr}"
  Context(kind: ckNode, nk: nnkIdent, output: some(n))


proc parseLitKind(n: NimNode): Context =
  result = case n.kind:
  of nnkIdent: 
    parseIdentKind(n)
  else:
    #decho &"LitKind {n.kind} {n.repr}"
    if n.isNil: # vargargs[untyped] for some reason makes this n.isNil true, if nil was passed without varargs n.isNil would be false
      Context(nk: n.kind, kind: ckNode, output: some(newNimNode(nnkNilLit)))
    else:
      Context(nk: n.kind, kind: ckNode, output: some(n))


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
  result = Context(kind: ckAccQuoted, nk: nnkAccQuoted, isAccQuoted: true)

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
      elif AccQuotedOperators.anyIt(it in childContext.output.get.repr):
        state = aqPrefix
        let op = childContext.output.get.repr
        #assert op.len == 1, "only one prefix operator allowed"
        if op == ItIndexPrefix:
          childContext = Context(kind: ckOpItIndex)
        elif op == LowercasePrefix:
          childContext = Context(kind: ckOpLowercase)
        elif op == UppercasePrefix:
          childContext = Context(kind: ckOpUppercase)
        elif op == CapitalizePrefix:
          childContext = Context(kind: ckOpCapitalize)
        chain.add childContext
      elif StringifyPrefix in childContext.output.get.strVal:
        error(&"Stringify operator, '$$', not allowed in identifier", n)
      else:
        # stay at root
        result.children.add childContext
    of aqIt:
      assert not childContext.hasItem, "item identifier appeared again?"
      if childContext.output.get.strVal == "[":
        state = aqBracketOpen
      else:
        result.addOpChainToContext(chain)
        result.children.add childContext
        state = aqRoot
    of aqBracketOpen:
      # only accept ints
      try:
        tupleIndex = parseInt(childContext.output.get.strVal)
      except ValueError:
        error(&"Tuple index must be a static int.", n)
      state = aqTupleIndex
    of aqTupleIndex:
      assert childContext.output.get.strVal == "]"
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
    #decho &"parseAccQuoted output: {result.output.get.repr}"


proc parseBracketExpr(n: NimNode): Context =
  # first child:
  #   BracketExpr it[0][1]
  #   Ident array[0] # not hasItem
  #   Ident it[0] # hasItem
  # second child:
  #   IntLit it[0]
  # third child: Type for array definition
  #   Ident, TupleConst, etc.  array[0, (string, string)]
  #
  # need to parse first child and see if it has it
  #  if yes then ContextKind is ckOpTupleIndex
  #  else ckNode
  var first = parseNode(n[0])
  var second = parseNode(n[1])
  
  if first.hasItem and not first.isAccQuoted:
    result = Context(kind: ckOpTupleIndex, nk: n.kind, tupleIndex: second.output.get.intVal.int, children: @[first])
  else:
    # array[..it..], array[expr], array[IntLit, T]
    # `it[1]`[0] # first is accQuoted, so we ignore the following indexing
    result = parseMulti(n)
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
      of LowercasePrefix:
        Context(kind: ckOpLowercase, hasItem: true, children: @[child])
      of UppercasePrefix:
        Context(kind: ckOpUppercase, hasItem: true, children: @[child])
      of CapitalizePrefix:
        Context(kind: ckOpCapitalize, hasItem: true, children: @[child])
      else: 
        Context(kind: ckEmpty)
    else:
      assert child.output.get.kind == nnkIdent
      case prefix:
      of StringifyPrefix:
        Context(kind: ckNode, output: some(newLit(child.output.get.repr)))
      of LowercasePrefix:
        Context(kind: ckNode, output: some(ident(child.output.get.strVal.toLowerAscii)))
      of UppercasePrefix:
        Context(kind: ckNode, output: some(ident(child.output.get.strVal.toUpperAscii)))
      else:
        if true:
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


proc parseTypeDef(n: NimNode): Context =
  #decho &"parseTypeDef"
  result = Context(kind: ckTypeDef, nk: n.kind)
  for child in n:
    var childContext = parseNode(child)
    result.children.add childContext
    if childContext.hasItem and (childContext.kind == ckAccQuoted or childContext.kind == ckIt):
      result.isIt = true
  result.propHasItem()


proc parseNode(n: NimNode): Context =
  #decho &"enter parseNode {n.kind} {n.repr}"
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
    of nnkCaseStmt, nnkRecCase: parseMulti(n, ckCaseStmt)
    of nnkTypeDef: parseTypeDef(n)
    of nnkEnumTy: parseMulti(n, ckEnumTy)
    of nnkObjectTy: parseMulti(n, ckObjectTy)
    of nnkRecList: parseMulti(n, ckRecList)
    else: parseMulti(n)

  #decho &"exit parseNode {result.kind} {result.nk} {result.hasItem}"


#< Parsing functions


#> AST Transformers
proc tf(c: Context): Option[NimNode]

proc tfIt(c: Context): Option[NimNode] =
  for itemScope in scope.itScopes:
    if itemScope.itsName.eqIdent(c.itsName):
      return some(itemScope.item)
  error(&"Could not find \"{c.itsName}\" in scopes.")


proc tfInner(c: Context): seq[Option[NimNode]] =
  var itemScope = scope.itemStack[^1]
  if itemScope.items.len > 0:
    for i in 0 ..< itemScope.items.len:
      itemScope.itIndex.intVal = i
      var o = tf(c)
      if o.isSome:
        result.add o
        #decho &"tfInner {itemScope.repr = }\n--- {result.repr = }"
  else:
    var o = tf(c)
    if o.isSome:
      result.add o
      #decho &"tfInner {result.repr = }"

proc tfAccQuoted(c: Context): Option[NimNode] =
  var n = newNimNode(c.nk)
  var identStr: string
  for child in c.children:
    let childTfO = child.tf()
    if childTfO.isSome:
      let childTF: NimNode = childTfO.get
      # check if child is accQuoted
      if childTf.kind == nnkAccQuoted:
        # flatten nnkAccQuoted
        for child in childTf.children:
          n.add child
          identStr &= child.repr
        #if true:
          #error("Debugging tfAccQuoted child is nnkAccQuoted " & n.astGenRepr)
      else:
        n.add childTf
        identStr &= childTf.repr
  if identStr.validIdentifier and identStr.notin KeywordsSet:
    #if true:
    #  error("Debugging tfAccQuoted valid identifier " & identStr)
    some(newIdentNode(identStr))
  else:
    #if true:
    #  error("Debugging tfAccQuoted not valid identifier " & identStr)
    some(n)

proc tfGen(c: Context): Option[NimNode] =
  # pass in named and unnamed items into the call
  let res = newTree(nnkCall, ident(MacroTransformerName))
  result = some(res)
  
  for lhs, rhs in scope.named:
    res.add newTree(nnkExprEqExpr, ident(lhs), rhs)
  res.add newTree(nnkExprEqExpr, ident(ItemStackName), scope.itemStack.copyNimTree)
  res.add c.callsite[1..^1]


proc tfOp(c: Context): Option[NimNode] =
  #decho 0, "tfItem"
  var first = if c.children.len > 0 : c.children[0] else: Context(kind: ckEmpty)
  case c.kind:
  of ckOpTupleIndex:
    var n = first.tf().get
    if n.kind == nnkTupleConstr:
      result = some(n[c.tupleIndex])
    else: # "duplicate" n as if it were part of a tuple
      result = some(n)
  of ckOpStringify:
    var n = first.tf().get
    result = some(newLit(n.repr))
  of ckOpItIndex:
    assert first.kind == ckIt
    for itemScope in scope.itScopes:
      if itemScope.itsName.eqIdent(first.itsName):
        result = some(itemScope.itIndex.copyNimNode)
        break
  of ckOpLowercase:
    var n = first.tf().get
    result = some(ident(n.repr.toLowerAscii))
  of ckOpUppercase:
    var n = first.tf().get
    result = some(ident(n.repr.toUpperAscii))
  of ckOpCapitalize:
    var n = first.tf().get
    result = some(ident(n.repr.capitalizeAscii))
  else:
    discard


proc isOnLastItem(): bool =
  # Used to delay output if we have a container construct, e.g.: section, case, type
  if scope.itemStack[^1].items.len == 0: 
    return true
  else:
    scope.itemStack[^1].items.len - 1 == scope.itemStack[^1].index


proc tfVarSection(c: Context): Option[NimNode] =
  #decho &"tfVarSection {c.nk} {isOnLastItem() = }"
  if isOnLastItem():
    let res = newTree(c.nk)
    result = some(res)
    for defc in c.children:
      if defc.hasItem:
        for n in tfInner(defc):
          if n.isSome:
            res.add n.get
      else:
        res.add defc.output.get


proc tfCase(c: Context): Option[NimNode] =
  # case should only return the entire case statement on the last index, of last scope item index
  if isOnLastItem():
    let res = newTree(c.nk, c.children[0].output.get)
    result = some(res)
    for n in tfInner(c.children[1]):
      if n.isSome:
        res.add n.get
    if c.children[^1].nk == nnkElse:
      res.add c.children[^1].tf().get


proc tfTypeSection(c: Context): Option[NimNode] =
  if isOnLastItem():
    #decho &"tfTypeSection {c.nk}"
    let res = newTree(c.nk)
    result = some(res)
    var itemScope = scope.itemStack[^1]
    for i in 0 ..< itemScope.items.len:
      itemScope.itIndex.intVal = i
      for defc in c.children:
        if defc.hasItem:
          var cres = tf(defc)
          if cres.isSome:
            res.add cres.get
        else:
          res.add defc.output.get


proc tfTypeDef(c: Context): Option[NimNode] =
  #decho "tfTypeDef"
  if c.isIt or isOnLastItem():
    let res = newTree(c.nk)
    result = some(res)
    for child in c.children:
      res.add child.tf().get


proc tfEnumTy(c: Context): Option[NimNode] =
  assert c.children.len == 2
  let res = newTree(nnkEnumTy, c.children[0].output.get)
  result = some(res)
  var def = c.children[1]
  var itemScope = scope.itemStack[^1]
  for i in 0 ..< itemScope.items.len:
    itemScope.itIndex.intVal = i
    res.add tf(def).get

proc tfObjectTy(c: Context): Option[NimNode] =
  let res = newTree(nnkObjectTy, c.children[0].output.get, c.children[1].output.get) # 0: pragmas, 1: nnkOfInherit
  result = some(res)
  res.add tf(c.children[2]).get

proc tfRecList(c: Context): Option[NimNode] =
  let res = newTree(nnkRecList)
  result = some(res)
  var itemScope = scope.itemStack[^1]
  for i in 0..<itemScope.items.len:
    itemScope.itIndex.intVal = i
    for def in c.children:
      var tfdef = tf(def)
      if tfdef.isSome:
        res.add tfdef.get

proc tf(c: Context): Option[NimNode] =
  #decho &"tf {c.kind} {c.hasItem}"
  result = if c.hasItem:
    case c.kind:
    of ckIt: tfIt(c)
    of ckNamed: c.output
    of ckAccQuoted: tfAccQuoted(c)
    of ckGen: tfGen(c)
    of ckOpTupleIndex..ckOpCapitalize:
      tfOp(c)
    of ckVarSection: tfVarSection(c)
    of ckCaseStmt: tfCase(c)
    of ckTypeSection: tfTypeSection(c)
    of ckTypeDef: tfTypeDef(c)
    of ckEnumTy: tfEnumTy(c)
    of ckObjectTy: tfObjectTy(c)
    of ckRecList: tfRecList(c)
    else:
      var n = newNimNode(c.nk)
      for child in c.children:
        let childTf = child.tf()
        if childTf.isSome:
          n.add childTf.get
      some(n)
  else:
    case c.kind:
      of ckAccQuoted: tfAccQuoted(c)
      else:
        c.output
  #decho &"tf {result.treerepr}"
#< AST Transformers

macro gen*(va: varargs[untyped]): untyped =
  ## Implements the DSL.
  var body = if va[^1].kind == nnkStmtList: va[^1] else: nil

  var args:seq[NimNode] = if body.isNil: va[0..^1] else: va[0..^2]

  # Check for field operator on arguments.
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
    if not body.isNil:
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
      if lhs.kind == nnkIdent:
        if lhs.eqIdent(ItsName): # change its name
          itemScope[0] = rhs
        elif lhs.eqIdent(ItemStackName):
          scope.itemStack = rhs
        else:
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
    elif arg.kind == nnkInfix and arg[0].eqIdent(GlobalInfix):
      var lhs = arg[1]
      var rhs = arg[2]
      expectKind lhs, nnkIdent
      globalNamed[lhs.strVal] = rhs
    else:
      itemScope.items.add arg

  if scope.itemStack.isNil:
    #decho "new itemStack"
    scope.itemStack = newNimNode(nnkBracket)
  scope.itemStack.add itemScope
  #decho "gen itemStack " & scope.itemStack.repr

  if body.isNil:
    return

  for s in body:
    c.children.add parseNode(s)

  c.propHasItem()


  #decho "-- transform"
  result = newNimNode(nnkStmtList)
  for n in c.tfInner():
    if n.isSome:
      result.add n.get
  if printFlag:
    echo "-----"
    echo "gen ", va.repr
    echo "----- body tree"
    echo body.treeRepr
    echo ">>> result"
    echo result.repr
    echo ">>----"
    echo result.treerepr
    echo ">>----"
    echo "itemStack post tf " & scope.itemStack.repr
    echo "------"

#> == it over type fields
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

proc fieldsTuple(src, ty: NimNode): seq[NimNode] =
  for def in ty:
    for id in def[0..^3]:
      result.add id

proc fieldsTupleConstr(src, ty: NimNode): seq[NimNode] =
  for i, sym in ty:
    result.add newLit(i)

proc fieldsBracket(src, ty: NimNode): seq[NimNode] =
  var l = ty[1][1].intVal
  var r = ty[1][2].intVal
  for i in l..r:
    result.add newlit(i)

proc isTypeDesc(n: NimNode): bool =
  var t = n.getType
  t.kind == nnkBracketExpr and t[0].kind == nnkSym and t[0].strVal == "typeDesc"

macro genWith*(x: typed, args:varargs[untyped]): untyped =
  ## Calls ``gen`` with the fields of the variable or type passed in; supports enum, object, and tuple.
  ## Used by the fields operator, ``+``, with ``gen`` on a symbol.
  var ty:NimNode = if isTypeDesc(x): 
      if x.kind == nnkSym:
        x.getImpl()[2] # get type from typedef
      else:
        x # range, array are types
    else:
      x.getTypeImpl

  let body = args[^1]

  result = nnkCall.newTree(ident(MacroTransformerName))

  var fields: seq[NimNode] = case ty.kind:
  of nnkEnumTy: fieldsEnum(x, ty)
  of nnkObjectTy: fieldsObject(x, ty)
  of nnkTupleTy: fieldsTuple(x, ty)
  of nnkTupleConstr: fieldsTupleConstr(x, ty)
  of nnkBracketExpr: fieldsBracket(x, ty)
  else:
    if true:
      error(&"Unexpected {ty.kind}\n{ty.treerepr}", x)
    @[]

  var tyArgName:NimNode
  if args.len > 1:
    for a in args[0 ..< ^1]:
      if a.kind == nnkExprEqExpr:
        var lhs = a[0]
        var rhs = a[1]
        if rhs.kind == nnkIdent and rhs.eqIdent(x):
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
    echo "genWith(", x.strVal, ",", args.repr, ")"
    echo ">>>"
    echo result.repr
    echo "-----"
#< == it over type fields