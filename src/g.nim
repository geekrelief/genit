import std / [ macros, tables, strformat, genasts, strutils ]
#import macronimics

const 
  MacroName = "g"
  ItsName = "it"

  EmptyKinds* = { nnkNone, nnkEmpty, nnkNilLit }
  IntKinds* = { nnkCharLit..nnkUint64Lit }
  FloatKinds* = { nnkFloatLit..nnkFloat64Lit } 
  StrLitKinds* = { nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym }
  LitKinds* = EmptyKinds + IntKinds + FloatKinds + StrLitKinds

type
  #[
  OpKind = enum
    ## Specifies the operations
    opStringify # $$it = item -> "item"
    opCapitalize # ^it = item -> Item
    opTupleIndex # it[n]
    opItem
    opNamed
  ]#

  ContextKind = enum
    ckNode
    ckGen
    ckIt
    ckAccQuoted
    ckOpTupleIndex # it[n]
    #Stringify # $$it = item -> "item"
    #Capitalize # ^it = item -> Item
    #Named
    ckSection
    ckCase
  
  Scope = ref object
    parent: Scope
    itsName: string
    named: Table[string, NimNode]
    items: seq[NimNode]
  
  Context = ref object
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
          decho &"{space(depth)} ckNode nnkIdent ({lhs} => {rhs.repr})"
          c = Context(nk: nnkIdent, kind: ckNode, output: rhs)
          break transform

      if n.strVal == curScope.itsName:
        decho &"{space(depth)} ckIt nnkIdent {curScope.itsName}"
        c = Context(nk: nnkIdent, kind: ckIt, name: curScope.itsName, hasIt: true)
        break transform

      curScope = curScope.parent

  if c.isNil:
    decho &"{space(depth)} ckNode nnkIdent {n.repr}"
    c = Context(kind: ckNode, output: n)
  result = c


proc litKind(depth: int, scope: Scope, n: NimNode): Context =
  result = case n.kind:
  of nnkIdent: 
    identKind(depth, scope, n)
  else:
    Context(nk: n.kind, kind: ckNode, output: n)

type 
  AccQuotedStateKind = enum
    aqkRoot
    aqkIt
    aqkBracketOpen
    aqkTupleIndex
  
  AccQuotedState = object
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
      assert childContext.kind == ckNode
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
  if first.hasIt:
    var second = parseNode(depth, scope, n[1])
    result = Context(kind: ckOpTupleIndex, tupleIndex: second.output.intVal.int, hasIt: true)
    result.add first
  else:
    result = Context(kind: ckNode, nk: n.kind)
    result.children.add first
    result.children.add parseNode(depth, scope, n[1])

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

proc toNimNode(c: Context, itemTableStack: var seq[Table[string, NimNode]]): NimNode

proc transformIt(c: Context, itemTableStack: var seq[Table[string, NimNode]]): NimNode =
  case c.kind:
  of ckIt:
    result = c.toNimNode(itemTableStack)
  of ckOpTupleIndex:
    var tn = c.children[0].transformIt(itemTableStack)
    assert tn.kind == nnkTupleConstr
    result = tn[c.tupleIndex]
  else:
    discard

proc toNimNodeFromAccQuoted(c: Context, itemTableStack: var seq[Table[string, NimNode]]): NimNode =
  result = newNimNode(nnkAccQuoted)
  var transform: seq[Context]
  for child in c.children:
    case child.kind:
    of ckIt, ckNode:
      result.add child.toNimNode(itemTableStack)
    of ckOpTupleIndex:
      result.add child.transformIt(itemTableStack)
    else: discard

proc toNimNode(c: Context, itemTableStack: var seq[Table[string, NimNode]]): NimNode =
  if c.hasIt:
    case c.kind:
    of ckIt:
      for i in countDown(itemTableStack.len-1, 0):
        if itemTableStack[i].hasKey(c.name):
          result = itemTableStack[i][c.name]
          break
      result
    of ckGen:
      if c.hasIt:
        var n = newNimNode(nnkStmtList)
        itemTableStack.add initTable[string, NimNode]()
        for item in c.scope.items:
          itemTableStack[^1][c.scope.itsName] = item
          for child in c.children:
            n.add child.toNimNode(itemTableStack)
        discard itemTableStack.pop()
        n
      else:
        c.output
    of ckAccQuoted:
      toNimNodeFromAccQuoted(c, itemTableStack)
    of ckOpTupleIndex:
      c.transformIt(itemTableStack)
    else:
      var n = newNimNode(c.nk)
      for child in c.children:
        n.add child.toNimNode(itemTableStack)
      n
  else:
    c.output

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
  c.output = c.toNimNode(itemTableStack)
  if c.output != nil:
    decho c.output.treeRepr
  else:
    decho "invalid"
  result = c.output