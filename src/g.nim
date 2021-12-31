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

const MacroName = "g"
const ItsName = "it"
const StringifyPrefix = "$$"
const ItIndexPrefix = "%"
const CapitalizePrefix = "^"
const Operators = [ StringifyPrefix, ItIndexPrefix, CapitalizePrefix ]
const AccQuotedOperators = [ ItIndexPrefix, CapitalizePrefix ] # operators that work in accQuoted

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
    ckCaseStmt
  
type Scope = ref object
    parent: Scope
    itsName: string
    named: Table[string, NimNode]
    items: seq[NimNode]
  
type Context = ref object
    nk: NimNodeKind
    output: NimNode
    hasItem: bool
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
      of ckCaseStmt:
        xpr: Context
        ofBranch: Context
        elseBranch: Context
      else: discard

#> Debug
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

proc space(count: int): string =
  for i in 0..<count:
    result.add "  "
#< Debug

#> Parsing functions
proc parseNode(depth: int, scope: Scope, n: NimNode): Context
proc parseGen(depth: int, parentScope: Scope, n: NimNode): Context

proc parseOtherNode(depth: int, scope: Scope, n:NimNode): Context =
  decho &"{space(depth)} otherNode ckNode {n.kind}"
  var c = Context(nk: n.kind, kind: ckNode)
  for child in n:
    var childContext = parseNode(depth, scope, child)
    c.children.add childContext
    if childContext.hasItem:
      c.hasItem = true
  if not c.hasItem:
    c.output = newNimNode(c.nk)
    for child in c.children:
      c.output.add child.output
  c

proc parseIdentKind(depth: int, scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} identKind"
  var c:Context
  block transform:
    var curScope = scope
    while curScope != nil: 
      for lhs, rhs in curScope.named:
        if n.strVal == lhs:
          decho &"{space(depth)} ckNamed nnkIdent ({lhs} => {rhs.repr})"
          c = Context(nk: nnkIdent, kind: ckNamed, output: rhs)
          break transform

      if n.strVal == curScope.itsName:
        decho &"{space(depth)} ckIt nnkIdent {curScope.itsName}"
        c = Context(nk: nnkIdent, kind: ckIt, name: curScope.itsName, hasItem: true)
        break transform

      curScope = curScope.parent

  if c.isNil:
    # some other identifier
    decho &"{space(depth)} ckNode nnkIdent {n.repr}"
    c = Context(kind: ckNode, nk: nnkIdent, output: n)
  result = c


proc parseLitKind(depth: int, scope: Scope, n: NimNode): Context =
  result = case n.kind:
  of nnkIdent: 
    parseIdentKind(depth, scope, n)
  else:
    Context(nk: n.kind, kind: ckNode, output: n)

type AccQuotedStateKind = enum
    aqkRoot
    aqkIt
    aqkBracketOpen
    aqkTupleIndex
    aqkPrefix
  
type AccQuotedState = object
    case kind: AccQuotedStateKind
    of aqkTupleIndex:
      tupleIndex: int
    of aqkPrefix:
      head: Context
      tail: Context
    else:
      discard

proc parseAccQuoted(depth: int, scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} parseAccQuoted"
  decho n.treeRepr
  var c = Context(kind: ckAccQuoted, nk: nnkAccQuoted)
  result = c
  var state = AccQuotedState(kind: aqkRoot)
  for id in n:
    decho &"{space(depth)} {state.kind} '{id.kind = }' '{id.repr = }'"
    var childContext = parseNode(depth, scope, id)
    case state.kind:
    of aqkRoot:
      if childContext.hasItem:
        c.hasItem = true
        state = AccQuotedState(kind: aqkIt)
      else:
        let op = childContext.output.strVal
        if op in AccQuotedOperators:
          # when done, Context should have hasItem true
          if op == ItIndexPrefix:
            childContext = Context(kind: ckOpItIndex)
          elif op == CapitalizePrefix:
            childContext = Context(kind: ckOpCapitalize)
          state = AccQuotedState(kind: aqkPrefix, head: childContext, tail: childContext)
        #
      # if
      c.children.add childContext
    of aqkIt:
      if childContext.output.strVal == "[":
        state = AccQuotedState(kind: aqkBracketOpen)
      else:
        c.children.add childContext
        state = AccQuotedState(kind: aqkRoot)
    of aqkBracketOpen:
      assert childContext.nk == nnkIdent
      state = AccQuotedState(kind: aqkTupleIndex, tupleIndex: parseInt(childContext.output.strVal))
    of aqkTupleIndex:
      assert childContext.kind == ckNode 
      assert childContext.output.strVal == "]"
      var tupleIndexContext = Context(kind: ckOpTupleIndex, tupleIndex: state.tupleIndex, hasItem: true)
      var containerContext = c.children.pop()
      tupleIndexContext.children.add containerContext
      c.children.add tupleIndexContext
      state = AccQuotedState(kind: aqkIt)
    of aqkPrefix:
      assert childContext.hasItem
      c.hasItem = true
      state.head.hasItem = true
      state.tail.children.add childContext
      state = AccQuotedState(kind: aqkRoot)

  if not c.hasItem:
    c.output = newNimNode(c.nk)
    for child in c.children:
      c.output.add child.output
    decho &"parseAccQuoted output: {c.output.repr}"


proc parseBracketExpr(depth: int, scope: Scope, n: NimNode): Context =
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
  var first = parseNode(depth, scope, n[0])
  var second = parseNode(depth, scope, n[1])
  if first.hasItem:
    result = Context(kind: ckOpTupleIndex, tupleIndex: second.output.intVal.int, hasItem: true, children: @[first])
  elif second.hasItem:
      # array[..it..]
      result = Context(kind: ckNode, nk: n.kind, hasItem: true, children: @[first, second])
  else:
    result = Context(kind: ckNode, nk: n.kind, output: n)

proc parsePrefix(depth: int , scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} parsePrefix {n.repr}"
  var prefix = n[0].strVal
  var exp = n[1]
  if prefix in Operators:
    var child = parseNode(depth, scope, exp)
    if child.hasItem:
      case prefix:
      of StringifyPrefix:
        Context(kind: ckOpStringify, hasItem: true, children: @[child])
      of ItIndexPrefix:
        Context(kind: ckOpItIndex, hasItem: true, children: @[child])
      of CapitalizePrefix:
        Context(kind: ckOpCapitalize, hasItem: true, children: @[child])
      else: 
        nil
    else:
      assert child.output.kind == nnkIdent
      case prefix:
      of StringifyPrefix:
        Context(kind: ckNode, output: newLit(child.output.repr))
      of CapitalizePrefix:
        Context(kind: ckNode, output: ident(child.output.strVal.capitalizeAscii))
      else:
        raiseAssert &"parsePrefix unexpected '{prefix}' in \"{n.repr}\" "
  else:
    parseOtherNode(depth, scope, n)


proc parseCaseStmt(depth: int, scope: Scope, n: NimNode): Context = 
  result = Context(kind: ckCaseStmt, nk: n.kind, hasItem: true)
  for child in n:
    case child.kind:
    of nnkOfBranch:
      result.ofBranch = parseNode(depth, scope, child)
      assert result.ofBranch.hasItem
    of nnkElse:
      result.elseBranch = parseNode(depth, scope, child)
      assert not result.elseBranch.hasItem
    else:
      result.xpr = parseNode(depth, scope, child)
      assert not result.xpr.hasItem

proc parseNode(depth: int, scope: Scope, n: NimNode): Context =
  decho &"{space(depth)} parseNode"
  result = case n.kind:
    of LitKinds:
      parseLitKind(depth, scope, n)
    of nnkCall, nnkCommand:
      if n[0].kind == nnkIdent and n[0].strVal == MacroName:
        parseGen(depth + 1, scope, n)
      else:
        parseOtherNode(depth, scope, n)
    of nnkAccQuoted:
      parseAccQuoted(depth, scope, n)
    of nnkBracketExpr:
      parseBracketExpr(depth, scope, n)
    of nnkPrefix:
      parsePrefix(depth, scope, n)
    of nnkCaseStmt:
      parseCaseStmt(depth, scope, n)
    else:
      parseOtherNode(depth, scope, n)

proc parseGen(depth: int, parentScope: Scope, n: NimNode): Context =
  var scope = Scope(parent: parentScope, itsName: ItsName)
  var c = Context(kind: ckGen, nk: n.kind, body: n[^1], scope: scope)

  var args:seq[NimNode]
  if n.len > 2: 
    args = n[1..^2]

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
    var childContext = parseNode(depth, scope, s)
    c.children.add childContext
    if childContext.hasItem:
      c.hasItem = true

  if not c.hasItem:
    c.output = newNimNode(nnkStmtList)
    for child in c.children:
      c.output.add child.output

  result = c

#< Parsing functions


#> AST Transformers
type ItemTableStack = seq[Table[string, NimNode]]
type ScopeIndex = tuple[scope: Scope, index: int]
type ScopeIndexStack = seq[ScopeIndex]

proc tf(c: Context, s: var ScopeIndexStack): NimNode

proc tfIt(c: Context, s: var ScopeIndexStack): NimNode =
  for i in countDown(s.len - 1, 0):
    if s[i].scope.itsName == c.name:
      return s[i].scope.items[ s[i].index ]
  raiseAssert &"Could not find \"{c.name}\" in scopes."

proc tfGen(c: Context, s: var ScopeIndexStack): NimNode =
  result = newNimNode(nnkStmtList)
  s.add (c.scope, 0)
  
  for i in 0..<c.scope.items.len:
    s[^1].index = i
    for child in c.children:
      var o = child.tf(s)
      if o != nil:
        result.add o
  discard s.pop()

proc tfOp(c: Context, s: var ScopeIndexStack): NimNode =
  decho "tfItem"
  var first = if c.children.len > 0 : c.children[0] else: nil
  case c.kind:
  of ckOpTupleIndex:
    var n = first.tf(s)
    assert n.kind == nnkTupleConstr
    result = n[c.tupleIndex]
  of ckOpStringify:
    var n = first.tf(s)
    decho &"  ckOpStringify {n.repr}"
    result = newLit(n.repr)
  of ckOpItIndex:
    assert first.kind == ckIt
    for i in countDown(s.len-1, 0):
      if s[i].scope.itsName == first.name:
        result = newLit(s[i].index)
        break
  of ckOpCapitalize:
    var n = first.tf(s)
    assert first.nk == nnkIdent
    result = ident(n.strVal.capitalizeAscii)
  else:
    discard

proc tfCase(c: Context, s: var ScopeIndexStack): NimNode =
  # case should only return the entire case statement on the last index, of last scope item index
  if s[^1].scope.items.len - 1 == s[^1].index:
    result = newTree(nnkCaseStmt, c.xpr.output)
    for i in 0..<s[^1].scope.items.len: # redo the indices for case's ofBranch
      s[^1].index = i 
      result.add c.ofBranch.tf(s)
    if c.elseBranch != nil:
      result.add c.elseBranch.output


proc tf(c: Context, s: var ScopeIndexStack): NimNode =
  decho &"tf {c.kind} {c.hasItem}"
  if c.hasItem:
    case c.kind:
    of ckIt:
      tfIt(c, s)
    of ckNamed:
      c.output
    of ckGen:
      tfGen(c, s)
    of ckOpTupleIndex, ckOpStringify, ckOpItIndex, ckOpCapitalize:
      tfOp(c, s)
    of ckCaseStmt:
      tfCase(c, s)
    else:
      var n = newNimNode(c.nk)
      for child in c.children:
        n.add child.tf(s)
      n
  else:
    c.output
#< AST Transformers

macro g*(args: varargs[untyped]): untyped =
  var gNode = newNimNode(nnkCall)
  gNode.add ident(MacroName)
  for a in args:
    gNode.add a
  decho "-- AST"
  decho gNode.treeRepr

  var c:Context = parseGen(0, nil, gNode)
  
  decho "-- transform"
  if c.output == nil:
    var s:ScopeIndexStack
    c.output = c.tfGen(s)
  if c.output != nil:
    decho c.output.treeRepr
  else:
    decho "invalid"

  result = c.output