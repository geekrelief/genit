## A DSL for generating repetitive code.

# To extend the DSL: 
#   Capture what we're parsing in Nim to genit's Context nodes (NimNode -> Context)
#     Modify the `parse` functions, update ContextKind / Context
#   Once we have the entire Context tree, we can transform back to NimNodes
#     Modify morph functions for traversing the Context tree
#     Modify `morphItem` for operations

import std / [ macros, tables, strformat, genasts, strutils ]

const MacroName = "g"
const ItsName = "it"
const StringifyPrefix = "$$"
const ItIndexPrefix = "%"

const EmptyKinds* = { nnkNone, nnkEmpty, nnkNilLit }
const IntKinds* = { nnkCharLit..nnkUint64Lit }
const FloatKinds* = { nnkFloatLit..nnkFloat64Lit }
const StrLitKinds* = { nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym }
const LitKinds* = EmptyKinds + IntKinds + FloatKinds + StrLitKinds

type ContextKind = enum
    ckNode
    ckGen
    ckIt
    ckNamed
    ckAccQuoted
    ckOpTupleIndex # it[n]
    ckOpStringify # $$it = item -> "item"
    ckOpItIndex # %it => item index
    ckOpCapitalize # ^it = item -> Item
    ckSection
    ckCase
  
type Scope = ref object
    parent: Scope
    itsName: string
    named: Table[string, NimNode]
    items: seq[NimNode]
  
type Context = ref object
    nk: NimNodeKind
    output: NimNode
    hasIt: bool
    children: seq[Context]
    case kind: ContextKind
      of ckGen:
        scope: Scope
        body: NimNode
      of ckIt: 
        name: string
      of ckOpTupleIndex: 
        tupleIndex: int
      of ckSection: 
        identDefs: seq[Context]
      of ckCase:
        ofBranch: Context
        elseBranch: Context
      else: discard

var debugFlag {.compileTime.} = false

macro debug*(body: untyped): untyped =
  debugFlag = true
  result = genast(body):
    body
    static:
      debugOff()

proc debugOff*() {.compileTime.} =
  debugFlag = false

proc decho(msg:string) {.compileTime.} =
  if debugFlag:
    echo msg

proc add(parent: Context, child: Context): Context {.discardable.} =
  parent.children.add child
  parent

proc space(count: int): string =
  for i in 0..<count:
    result.add "  "

#> Parsing functions
proc parseNode(depth: int, scope: Scope, n: NimNode): Context
proc parseGen(depth: int, parentScope: Scope, n: NimNode): Context

proc otherNode(depth: int, scope: Scope, n:NimNode): Context =
  decho &"{space(depth)} otherNode ckNode {n.kind}"
  var c = Context(nk: n.kind, kind: ckNode)
  for child in n:
    var childContext = parseNode(depth, scope, child)
    c.add childContext
    if childContext.hasIt:
      c.hasIt = true
  if not c.hasIt:
    c.output = newNimNode(c.nk)
    for child in c.children:
      c.output.add child.output
  c

proc identKind(depth: int, scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} identKind"
  var c:Context
  block transform:
    var curScope = scope
    while curScope != nil: 
      for lhs, rhs in curScope.named:
        if n.strVal == lhs:
          decho &"{space(depth)} ckNamed nnkIdent ({lhs} => {rhs.repr})"
          c = Context(nk: nnkIdent, kind: ckNamed, output: rhs, hasIt: true)
          break transform

      if n.strVal == curScope.itsName:
        decho &"{space(depth)} ckIt nnkIdent {curScope.itsName}"
        c = Context(nk: nnkIdent, kind: ckIt, name: curScope.itsName, hasIt: true)
        break transform

      curScope = curScope.parent

  if c.isNil:
    # some other identifier
    decho &"{space(depth)} ckNode nnkIdent {n.repr}"
    c = Context(kind: ckNode, output: n)
  result = c


proc litKind(depth: int, scope: Scope, n: NimNode): Context =
  result = case n.kind:
  of nnkIdent: 
    identKind(depth, scope, n)
  else:
    Context(nk: n.kind, kind: ckNode, output: n)

type AccQuotedStateKind = enum
    aqkRoot
    aqkIt
    aqkBracketOpen
    aqkTupleIndex
  
type AccQuotedState = object
    case kind: AccQuotedStateKind
    of aqkTupleIndex:
      tupleIndex: int
    else:
      discard

proc parseAccQuoted(depth: int, scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} parseAccQuoted"
  decho n.treeRepr
  var c = Context(kind: ckAccQuoted, nk: nnkAccQuoted)
  var state = AccQuotedState(kind: aqkRoot)
  for id in n:
    decho &"{space(depth)} {state.kind} '{id.kind = }' '{id.repr = }'"
    var childContext = parseNode(depth, scope, id)
    case state.kind:
    of aqkRoot:
      c.children.add childContext
      if childContext.hasIt:
        c.hasIt = true
        state = AccQuotedState(kind: aqkIt)
    of aqkIt:
      if childContext.output.strVal == "[":
        state = AccQuotedState(kind: aqkBracketOpen)
      else:
        c.children.add childContext
        state = AccQuotedState(kind: aqkRoot)
    of aqkBracketOpen:
      assert childContext.kind == ckNode
      state = AccQuotedState(kind: aqkTupleIndex, tupleIndex: parseInt(childContext.output.strVal))
    of aqkTupleIndex:
      assert childContext.kind == ckNode 
      assert childContext.output.strVal == "]"
      var tupleIndexContext = Context(kind: ckOpTupleIndex, tupleIndex: state.tupleIndex, hasIt: true)
      var lastChildContext = c.children.pop()
      tupleIndexContext.add lastChildContext
      c.children.add tupleIndexContext
      state = AccQuotedState(kind: aqkIt)
  decho &"{space(depth)} {c.children.len = }"
  c

proc parseBracketExpr(depth: int, scope: Scope, n: NimNode): Context =
  # first child:
  #   BracketExpr it[0][1]
  #   Ident array[0] # not hasIt
  #   Ident it[0] # hasIt
  # second child:
  #   IntLit it[0]
  # need to parse first child and see if it has it
  #  if yes then ContextKind is ckOpTupleIndex
  #  else ckNode
  var first = parseNode(depth, scope, n[0])
  var second = parseNode(depth, scope, n[1])
  if first.hasIt:
    result = Context(kind: ckOpTupleIndex, tupleIndex: second.output.intVal.int, hasIt: true, children: @[first])
  else:
    result = Context(kind: ckNode, nk: n.kind, children: @[first, second])

proc parsePrefix(depth: int , scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} parsePrefix {n.repr}"
  var prefix = n[0].strVal
  var exp = n[1]
  case prefix:
    of StringifyPrefix:
      var child = parseNode(depth, scope, exp)
      assert child.hasIt
      Context(kind: ckOpStringify, hasIt: true, children: @[child])
    of ItIndexPrefix:
      var child = parseNode(depth, scope, exp)
      assert child.hasIt
      Context(kind: ckOpItIndex, hasIt: true, children: @[child])
    else:
      otherNode(depth, scope, n)

proc parseNode(depth: int, scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} parseNode"
  result = case n.kind:
    of LitKinds:
      litKind(depth, scope, n)
    of nnkCall, nnkCommand:
      if n[0].kind == nnkIdent and n[0].strVal == MacroName:
        parseGen(depth + 1, scope, n)
      else:
        otherNode(depth, scope, n)
    of nnkAccQuoted:
      parseAccQuoted(depth, scope, n)
    of nnkBracketExpr:
      parseBracketExpr(depth, scope, n)
    of nnkPrefix:
      parsePrefix(depth, scope, n)
    else:
      otherNode(depth, scope, n)

proc parseGen(depth: int, parentScope: Scope, n: NimNode): Context =
  var scope = Scope(parent: parentScope, itsName: ItsName)
  var c = Context(kind: ckGen, nk: n.kind, body: n[^1], scope: scope)

  var args = n[1..^2]

  decho &"{space(depth)} parseGen {args.repr}"

  for arg in args:
    if arg.kind == nnkExprEqExpr:
      if arg[0].repr == ItsName: # change its name
        scope.itsName = arg[1].repr
      else:
        assert arg[0].kind == nnkIdent
        let
          key = arg[0].strVal
          value = arg[1]
        scope.named[key] = value # todo: check if this a nim fragment
    else:
      scope.items.add arg

  for s in c.body:
    var childContext = parseNode(depth + 1, scope, s)
    c.children.add childContext
    if childContext.hasIt:
      c.hasIt = true

  result = c

#< Parsing functions

type ItemTableStack = seq[Table[string, NimNode]]

#> Mighty Morph'in Power functions
proc morph(c: Context, itemTableStack: var ItemTableStack): NimNode

proc morphIt(c: Context, itemTableStack: var ItemTableStack): NimNode =
  for i in countDown(itemTableStack.len-1, 0):
    if itemTableStack[i].hasKey(c.name):
      result = itemTableStack[i][c.name]
      break

proc morphGen(c: Context, itemTableStack: var ItemTableStack): NimNode =
  result = newNimNode(nnkStmtList)
  itemTableStack.add initTable[string, NimNode]()
  for i, item in pairs(c.scope.items):
    itemTableStack[^1][c.scope.itsName] = item
    itemTableStack[^1][ItIndexPrefix & c.scope.itsName] = newLit(i)
    for child in c.children:
      result.add child.morph(itemTableStack)
  discard itemTableStack.pop()

proc morphOp(c: Context, itemTableStack: var ItemTableStack): NimNode =
  decho "morphItem"
  var first = if c.children.len > 0 : c.children[0] else: nil
  case c.kind:
  of ckOpTupleIndex:
    var n = first.morph(itemTableStack)
    assert n.kind == nnkTupleConstr
    result = n[c.tupleIndex]
  of ckOpStringify:
    var n = first.morph(itemTableStack)
    decho &"  ckOpStringify {n.repr}"
    result = newLit(n.repr)
  of ckOpItIndex:
    assert first.kind == ckIt
    var n = first.morph(itemTableStack)
    var key = ItIndexPrefix & first.name
    for i in countDown(itemTableStack.len-1, 0):
      if itemTableStack[i].hasKey(key):
        result = itemTableStack[i][key]
        break
  else:
    discard

proc morph(c: Context, itemTableStack: var ItemTableStack): NimNode =
  decho &"morph {c.kind}"
  if c.hasIt:
    case c.kind:
    of ckIt:
      morphIt(c, itemTableStack)
    of ckNamed:
      c.output
    of ckGen:
      morphGen(c, itemTableStack)
    of ckOpTupleIndex, ckOpStringify, ckOpItIndex:
      c.morphOp(itemTableStack)
    else:
      var n = newNimNode(c.nk)
      for child in c.children:
        n.add child.morph(itemTableStack)
      n
  else:
    c.output
#< Mighty Morph'in Power functions

macro g*(args: varargs[untyped]): untyped =
  var gNode = newNimNode(nnkCall)
  gNode.add ident(MacroName)
  for a in args:
    gNode.add a
  decho "-- AST"
  decho gNode.treeRepr

  var c:Context = parseGen(0, nil, gNode)
  
  decho "-- transform"
  var itemTableStack = @[initTable[string, NimNode]()]
  c.output = c.morph(itemTableStack)
  if c.output != nil:
    decho c.output.treeRepr
  else:
    decho "invalid"
  result = c.output