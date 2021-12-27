import std / [ macros, tables, strformat ]
#import macronimics

const 
  debug = false
  MacroName = "g"
  ItsName = "it"

  EmptyKinds* = { nnkNone, nnkEmpty, nnkNilLit }
  IntKinds* = { nnkCharLit..nnkUint64Lit }
  FloatKinds* = { nnkFloatLit..nnkFloat64Lit } 
  StrLitKinds* = { nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym }
  LitKinds* = EmptyKinds + IntKinds + FloatKinds + StrLitKinds

type
  OpKind = enum
    ## Specifies the operations
    Stringify # $$it = item -> "item"
    Capitalize # ^it = item -> Item
    TupleIndex # it[n]
    Item
    Named
  
  ContextKind = enum
    ckNode
    ckGen
    ckIt
    ckOp
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

proc decho(msg:string) {.inline.} =
  when debug:
    echo msg
  else:
    discard

proc add(parent: Context, child: Context): Context {.discardable.} =
  parent.children.add child
  parent

proc space(count: int): string =
  for i in 0..<count:
    result.add "  "

proc parseNode(depth: int, scope: Scope, n: NimNode): Context
proc parseGen(depth: int, parentScope: Scope, n: NimNode): Context

proc identKind(depth: int, scope: Scope, n: NimNode): Context =
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

proc otherNode(depth: int, scope: Scope, n:NimNode): Context =
  decho &"{space(depth)} ckNode {n.kind}"
  var c = Context(nk: n.kind, kind: ckNode)
  for child in n:
    var childContext = parseNode(depth + 1, scope, child)
    c.add childContext
    if childContext.hasIt:
      c.hasIt = true
  if not c.hasIt:
    c.output = newNimNode(c.nk)
    for child in c.children:
      c.output.add child.output
  c

proc parseNode(depth: int, scope: Scope, n: NimNode): Context =
  result = case n.kind:
    of LitKinds:
      litKind(depth, scope, n)
    of nnkCall, nnkCommand:
      if n[0].kind == nnkIdent and n[0].strVal == MacroName:
        parseGen(depth + 1, scope, n)
      else:
        otherNode(depth, scope, n)
    else:
      otherNode(depth, scope, n)

proc toNimNode(c: Context, itemTable: var Table[string, NimNode]): NimNode =
  if c.hasIt:
    case c.kind:
    of ckIt:
      itemTable[c.name]
    of ckGen:
      if c.hasIt:
        var n = newNimNode(nnkStmtList)
        for item in c.scope.items:
          itemTable[c.scope.itsName] = item
          for child in c.children:
            n.add child.toNimNode(itemTable)

        itemTable.del(c.scope.itsName)
        n
      else:
        c.output
    else:
      var n = newNimNode(c.nk)
      for child in c.children:
        n.add child.toNimNode(itemTable)
      n
  else:
    c.output

proc parseGen(depth: int, parentScope: Scope, n: NimNode): Context =
  var scope = Scope(parent: parentScope, itsName: ItsName)
  var c = Context(nk: n.kind, kind: ckGen, body: n[^1], scope: scope)

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

macro g*(args: varargs[untyped]): untyped =
  var gNode = newNimNode(nnkCall)
  gNode.add ident(MacroName)
  for a in args:
    gNode.add a
  var c:Context = parseGen(0, nil, gNode)
  
  decho "-- transform"
  var itemTable = initTable[string, NimNode]()
  c.output = c.toNimNode(itemTable)
  if c.output != nil:
    decho c.output.treeRepr
  else:
    decho "invalid"
  result = c.output