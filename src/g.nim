## A DSL for generating repetitive code.

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

const MacroTransformerName = "gen"
const MacroTransformerExName = "genx"
const ItsName = "it"
const StringifyPrefix = "$$"
const ItIndexPrefix = "%"
const CapitalizePrefix = "^"
const ExpandPrefix = "~"
const Operators = [ StringifyPrefix, ItIndexPrefix, CapitalizePrefix ]
const AccQuotedOperators = [ ItIndexPrefix, CapitalizePrefix ] # operators that work in accQuoted

const EmptyKinds* = { nnkNone, nnkEmpty, nnkNilLit }
const IntKinds* = { nnkCharLit..nnkUint64Lit }
const FloatKinds* = { nnkFloatLit..nnkFloat64Lit }
const StrLitKinds* = { nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym }
const LitKinds* = EmptyKinds + IntKinds + FloatKinds + StrLitKinds

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
    ckTypeSection
    ckTypeDef
    ckEnumTy
    ckObjectTy
    ckRecList
  
type Scope = ref object
    parent: Scope
    depth: int
    itsName: string
    named: Table[string, NimNode]
    items: seq[NimNode]
  
type Context = object
    nk: NimNodeKind # output kind
    output: NimNode # cached output
    hasItem: bool
    children: seq[Context]
    case kind: ContextKind
      of ckGen:
        scope: Scope
        body: NimNode
      of ckIt: 
        itsName: string
      of ckOpTupleIndex: 
        tupleIndex: int
      of ckVarSection: discard
      of ckCaseStmt: discard
      of ckTypeSection: discard
      of ckTypeDef: discard
      of ckEnumTy: discard
      of ckObjectTy: discard
      of ckRecList: discard
      else: discard

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
      debugOff()

proc debugOff*() {.compileTime.} =
  debugFlag = false

proc decho(count:int, msg:string) {.compileTime.} =
  if debugFlag:
    echo space(count), msg

proc decho(msg:string) {.compileTime.} =
  decho 0, msg
#< Debug

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

proc parseNode(n: NimNode, scope: Scope): Context

proc parseMulti(n: NimNode, scope: Scope, ck: ContextKind = ckNode): Context =
  #decho scope.depth, &"parseMulti {ck} {n.kind}"
  result = Context(kind: ck, nk: n.kind)
  for child in n:
    result.children.add parseNode(child, scope)
  result.propHasItem()


proc parseGen(n:NimNode, parentScope: Scope): Context =
  var scope = Scope(parent: parentScope, itsName: ItsName)
  if parentScope != nil:
    scope.depth = parentScope.depth
  var c = Context(kind: ckGen, nk: nnkStmtList, body: n[^1], scope: scope)

  var args:seq[NimNode]
  if n.len > 2: 
    args = n[1..^2]

  #decho scope.depth, &"parseGen {args.repr}"
  
  for arg in args:
    if arg.kind == nnkExprEqExpr:
      var lhs = arg[0]
      var rhs = arg[1]
      if lhs.kind == nnkIdent:
        if lhs.strVal == ItsName: # change its name
          scope.itsName = rhs.strVal
        else:
          scope.named[lhs.strVal] = rhs # todo: check if this a nim fragment
    elif arg.kind == nnkPrefix:
      # handle expansion
      var prefix = arg[0].strVal
      var group = arg[1].strVal
      if prefix == ExpandPrefix and parentScope.named.hasKey(group):
        var items = parentScope.named[group]
        assert items.kind == nnkBracket
        for item in items:
          scope.items.add item
    else:
      scope.items.add arg

  for s in c.body:
    c.children.add parseNode(s, scope)

  c.propHasItem()


proc parseIdentKind(n: NimNode, scope: Scope): Context =
  #decho scope.depth, "identKind"
  result = Context(kind: ckNode, nk: nnkIdent, output: n)
  block transform:
    var curScope = scope
    while curScope != nil: 
      for lhs, rhs in curScope.named:
        if n.strVal == lhs:
          #decho scope.depth, &"ckNamed nnkIdent ({lhs} => {rhs.repr})"
          result = Context(nk: nnkIdent, kind: ckNamed, output: rhs)
          break transform

      if n.strVal == curScope.itsName:
        #decho scope.depth, &"ckIt nnkIdent {curScope.itsName}"
        result = Context(nk: nnkIdent, kind: ckIt, itsName: curScope.itsName, hasItem: true)
        break transform

      curScope = curScope.parent


proc parseLitKind(n: NimNode, scope: Scope): Context =
  result = case n.kind:
  of nnkIdent: 
    parseIdentKind(n, scope)
  else:
    #decho scope.depth, &"LitKind {n.kind} {n.repr}"
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

proc parseAccQuoted(n: NimNode, scope: Scope): Context =
  #decho scope.depth, &"parseAccQuoted"
  #decho 0, n.treeRepr
  result = Context(kind: ckAccQuoted, nk: nnkAccQuoted)

  var chain: seq[Context] # points to the current child chain for the root
  var tupleIndex: int
  var state: AccQuotedState = aqRoot

  for id in n:
    #decho scope.depth, &"--> {state} '{id.kind = }' '{id.repr = }'"
    var childContext = parseNode(id, scope)
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
    #decho scope.depth, &"parseAccQuoted output: {result.output.repr}"


proc parseBracketExpr(n: NimNode, scope: Scope): Context =
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
  var first = parseNode(n[0], scope)
  var second = parseNode(n[1], scope)
  if first.hasItem:
    result = Context(kind: ckOpTupleIndex, nk: n.kind, tupleIndex: second.output.intVal.int, children: @[first])
  else:
    # array[..it..], array[expr]
    result = Context(kind: ckNode, nk: n.kind, children: @[first, second])
  result.propHasItem()

proc parsePrefix(n: NimNode, scope: Scope): Context =
  #decho scope.depth, &"parsePrefix {n.repr}"
  var prefix = n[0].strVal
  var exp = n[1]
  if prefix in Operators:
    var child = parseNode(exp, scope)
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
    parseMulti(n, scope)


proc parseSection(n: NimNode, scope:Scope): Context =
  #decho scope.depth, &"parseSection {n.repr}"
  if n.kind == nnkTypeSection:
    result = Context(kind: ckTypeSection, nk: n.kind)
  else:
    result = Context(kind: ckVarSection, nk: n.kind)
  for child in n:
    result.children.add parseNode(child, scope)
  result.propHasItem()


proc parseNode(n: NimNode, scope: Scope): Context =
  #decho scope.depth, &"enter parseNode {n.kind}"
  result = case n.kind:
    of LitKinds: parseLitKind(n, scope)
    of nnkCall, nnkCommand:
      if n[0].kind == nnkIdent and n[0].strVal == MacroTransformerName:
        parseGen(n, scope)
      else:
        parseMulti(n, scope)
    of nnkAccQuoted: parseAccQuoted(n, scope)
    of nnkBracketExpr: parseBracketExpr(n, scope)
    of nnkPrefix: parsePrefix(n, scope)
    of nnkVarSection, nnkLetSection, nnkConstSection, nnkTypeSection: 
      parseSection(n, scope)
    of nnkCaseStmt: parseMulti(n, scope, ckCaseStmt)
    of nnkTypeDef: parseMulti(n, scope, ckTypeDef)
    of nnkEnumTy: parseMulti(n, scope, ckEnumTy)
    of nnkObjectTy: parseMulti(n, scope, ckObjectTy)
    of nnkRecList: parseMulti(n, scope, ckRecList)
    else: parseMulti(n, scope)

  #decho scope.depth, &"exit parseNode {result.kind} {result.nk} {result.hasItem}"


#< Parsing functions


#> AST Transformers
type ScopeIndex = tuple[scope: Scope, index: int]
type ScopeIndexStack = seq[ScopeIndex]

proc tf(c: Context, s: var ScopeIndexStack): NimNode

proc tfIt(c: Context, s: var ScopeIndexStack): NimNode =
  for i in countDown(s.len - 1, 0):
    if s[i].scope.itsName == c.itsName:
      return s[i].scope.items[ s[i].index ]
  error(&"Could not find \"{c.itsName}\" in scopes.")


proc tfInner(c: Context, s: var ScopeIndexStack, n: NimNode) =
  if s[^1].scope.items.len > 0:
    for i in 0..<s[^1].scope.items.len:
      s[^1].index = i
      var o = tf(c, s)
      if o != nil:
        n.add o
  else:
    var o = tf(c, s)
    if o != nil:
      n.add o


proc tfGen(c: Context, s: var ScopeIndexStack): NimNode =
  result = newNimNode(nnkStmtList)
  #decho "tfGen"
  s.add (c.scope, 0)
  for child in c.children:
    tfInner(child, s, result)
  discard s.pop()


proc tfOp(c: Context, s: var ScopeIndexStack): NimNode =
  #decho 0, "tfItem"
  var first = if c.children.len > 0 : c.children[0] else: Context(kind: ckEmpty)
  case c.kind:
  of ckOpTupleIndex:
    var n = first.tf(s)
    assert n.kind == nnkTupleConstr
    result = n[c.tupleIndex]
  of ckOpStringify:
    var n = first.tf(s)
    result = newLit(n.repr)
  of ckOpItIndex:
    assert first.kind == ckIt
    for i in countDown(s.len-1, 0):
      if s[i].scope.itsName == first.itsName:
        result = newLit(s[i].index)
        break
  of ckOpCapitalize:
    var n = first.tf(s)
    result = ident(n.strVal.capitalizeAscii)
  else:
    discard


proc isOnLastItem(s: ScopeIndexStack): bool =
  # Used to delay output if we have a container construct, e.g.: section, case, type
  if s[^1].scope.items.len == 0: 
    return true
  else:
    s[^1].scope.items.len - 1 == s[^1].index


proc tfVarSection(c: Context, s: var ScopeIndexStack): NimNode =
  #decho &"tfVarSection {c.nk}"
  if isOnLastItem(s):
    result = newTree(c.nk)
    for defc in c.children:
      if defc.hasItem:
        tfInner(defc, s, result)
      else:
        result.add defc.output


proc tfCase(c: Context, s: var ScopeIndexStack): NimNode =
  # case should only return the entire case statement on the last index, of last scope item index
  if isOnLastItem(s):
    result = newTree(nnkCaseStmt, c.children[0].output)
    tfInner(c.children[1], s, result)
    if c.children[^1].nk == nnkElse:
      result.add c.children[^1].tf(s)


proc tfTypeSection(c: Context, s: var ScopeIndexStack): NimNode =
  #decho &"tfTypeSection {c.nk}"
  if isOnLastItem(s):
    result = newTree(c.nk)
    for defc in c.children:
      if defc.hasItem:
        result.add tf(defc, s)
      else:
        result.add defc.output

proc tfEnumTy(c: Context, s: var ScopeIndexStack): NimNode =
  assert c.children.len == 2
  result = newTree(nnkEnumTy, c.children[0].output)
  var def = c.children[1]
  for i in 0..<s[^1].scope.items.len:
    s[^1].index = i
    result.add tf(def, s)

proc tfObjectTy(c: Context, s: var ScopeIndexStack): NimNode =
  result = newTree(nnkObjectTy, c.children[0].output, c.children[1].output)
  result.add tf(c.children[2], s)

proc tfRecList(c: Context, s: var ScopeIndexStack): NimNode =
  result = newTree(nnkRecList)
  for i in 0..<s[^1].scope.items.len:
    s[^1].index = i
    for def in c.children:
      result.add tf(def, s)

proc tf(c: Context, s: var ScopeIndexStack): NimNode =
  #decho &"tf {c.kind} {c.hasItem}"
  if c.hasItem:
    case c.kind:
    of ckIt: tfIt(c, s)
    of ckNamed: c.output
    of ckGen: tfGen(c, s)
    of ckOpTupleIndex, ckOpStringify, ckOpItIndex, ckOpCapitalize:
      tfOp(c, s)
    of ckVarSection: tfVarSection(c, s)
    of ckCaseStmt: tfCase(c, s)
    of ckTypeSection: tfTypeSection(c, s)
    of ckEnumTy: tfEnumTy(c, s)
    of ckObjectTy: tfObjectTy(c, s)
    of ckRecList: tfRecList(c, s)
    else:
      var n = newNimNode(c.nk)
      for child in c.children:
        n.add child.tf(s)
      n
  else:
    c.output
#< AST Transformers

macro gen*(args: varargs[untyped]): untyped =
  var gNode = newNimNode(nnkCall)
  gNode.add ident(MacroTransformerName)
  for a in args:
    gNode.add a
  #decho "-- AST"
  #decho gNode.treeRepr

  var c:Context = parseGen(gNode, nil)
  
  #decho "-- transform"
  var s:ScopeIndexStack
  c.output = c.tfGen(s)
  #[
  if c.output != nil:
    decho c.output.treeRepr
    decho c.output.repr
  else:
    decho "invalid"
    ]#

  result = c.output

#> == it over type fields
proc isTypeDesc(n: NimNode): bool =
  var t = n.getType
  t.kind == nnkBracketExpr and t[0].kind == nnkSym and t[0].strVal == "typeDesc"


proc fieldsEnum(src, ty, dst: NimNode) =
  # EnumTy
  for n in ty:
    case n.kind
    of nnkSym:
      dst.add n
    of nnkEnumFieldDef:
      dst.add nnkTupleConstr.newTree(n[0], n[1])
    of nnkEmpty: discard
    else:
      error(&"Unsupported {n.kind}", src)

proc fieldsObject(src, ty, dst: NimNode) =
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
            dst.add id
        of nnkPostFix:
          dst.add id[1]
        else:
          error(&"Unexpected {id.kind = }", src)
    of nnkEmpty: discard
    else:
      error(&"Unexpected {def.kind = }", src)


macro genx*(arg: typed, body: untyped): untyped =
  expectKind arg, nnkSym
  assert isTypeDesc(arg)

  result = nnkCall.newTree(ident(MacroTransformerName))

  var impl = arg.getImpl
  case impl.kind:
  of nnkTypeDef:
    var ty = impl[2]
    case ty.kind:
    of nnkEnumTy: fieldsEnum(arg, ty, result)
    of nnkObjectTy: fieldsObject(arg, ty, result)
    else: error(&"Unexpected {ty.kind}", arg)
  else:
    error(&"Unexpected \"{impl.kind}\".", arg)

  result.add body
#< == it over type fields