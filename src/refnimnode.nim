import std / [macros {.all.}, strformat]

const 
  EmptyKinds* = { nnkNone, nnkEmpty, nnkNilLit }
  IntKinds* = { nnkCharLit..nnkUint64Lit }
  FloatKinds* = { nnkFloatLit..nnkFloat64Lit } 
  StrLitKinds* = { nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym }
  LitKinds* = EmptyKinds + IntKinds + FloatKinds + StrLitKinds


# RefNimNode mirrors NimNode, but uses refs to make it mutable
type
  NoChildrenDefect* = object of Defect

  RefNimNode* {.inheritable.} = ref object
    parent*: RefNimNode # if nil, index is ignored
    case kind*: NimNodeKind
    of EmptyKinds: discard
    of IntKinds:
      intVal*: BiggestInt
    of FloatKinds:
      floatVal*: BiggestFloat
    of StrLitKinds:
      strVal*: string
    else:
      children*: seq[RefNimNode]

proc treeRepr*(n: RefNimNode): string

proc `!===`*(a, b: RefNimNode): bool
proc `===`*(a, b: RefNimNode): bool =
  ## Compare two RefNimNodes. Return true if nodes are structurally
  ## equivalent. This means two independently created nodes can be equal.
  if a.kind != b.kind:
    false
  else:
    case a.kind:
    of nnkNone, nnkEmpty, nnkNilLit: true
    of nnkCharLit..nnkUint64Lit:
      a.intVal == b.intVal
    of nnkFloatLit..nnkFloat64Lit:
      a.floatVal == b.floatVal
    of nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
      a.strVal == b.strVal
    else:
      if a.children.len != b.children.len: 
        false
      else:
        for i in 0..<a.children.len:
          if a.children[i] !=== b.children[i]: return false
        true

proc `!===`*(a, b: RefNimNode): bool =
  not (a === b)

#[
proc sameType*(a, b: NimNode): bool {.magic: "SameNodeType", noSideEffect.} =
  ## Compares two Nim nodes' types. Return true if the types are the same,
  ## e.g. true when comparing alias with original type.
  discard
]#

proc len*(n: RefNimNode): int =
  ## Returns the number of children of `n`.
  case n.kind:
  of LitKinds:
    0
  else:
    n.children.len


proc index*(n: RefNimNode): int =
  ## Finds its index inside of its parent. Returns -1 if its parent is nil.
  if n.parent != nil:
    n.parent.children.find(n)
  else:
    -1


proc `[]`*(n: RefNimNode, i: int): var RefNimNode =
  ## Get `n`'s `i`'th child.
  result = n.children[i]

proc `[]`*(n: RefNimNode, i: BackwardsIndex): var RefNimNode = n[n.len - i.int]
  ## Get `n`'s `i`'th child.


template `^^`(n: RefNimNode, i: untyped): untyped =
  (when i is BackwardsIndex: n.len - int(i) else: int(i))


proc `[]`*[T, U: Ordinal](n: RefNimNode, x: HSlice[T, U]): seq[RefNimNode] =
  ## Slice operation for NimNode.
  ## Returns a seq of child of `n` who inclusive range [n[x.a], n[x.b]].
  let xa = n ^^ x.a
  let L = (n ^^ x.b) - xa + 1
  result = newSeq[RefNimNode](L)
  for i in 0..<L:
    result[i] = n[i + xa]

proc `[]=`*(n: RefNimNode, i: int, child: RefNimNode) =
  ## Set `n`'s `i`'th child to `child`.
  n.children[i].parent = nil
  child.parent = n
  n.children[i] = child

proc `[]=`*(n: RefNimNode, i: BackwardsIndex, child: RefNimNode) =
  ## Set `n`'s `i`'th child to `child`.
  n[n.len - i.int] = child

proc `:=`*(l: var RefNimNode, r: RefNimNode) =
  ## Replaces `l` with `r` in `l`'s tree.
  if r != nil and l.parent != nil:
      r.parent = l.parent
      l.parent.children[l.index] = r
  l = r


#[
template `or`*(x, y: NimNode): NimNode =
  ## Evaluate `x` and when it is not an empty node, return
  ## it. Otherwise evaluate to `y`. Can be used to chain several
  ## expressions to get the first expression that is not empty.
  ##
  ## .. code-block:: nim
  ##
  ##   let node = mightBeEmpty() or mightAlsoBeEmpty() or fallbackNode

  let arg = x
  if arg != nil and arg.kind != nnkEmpty:
    arg
  else:
    y
]#

proc add*(parent, child: RefNimNode): RefNimNode {.discardable.} =
  ## Adds the `child` to the `parent` node. Returns the
  ## parent node so that calls can be nested.
  child.parent = parent
  parent.children.add child
  parent

#[
proc add*(parent: RefNimNode, children: varargs[RefNimNode]): RefNimNode {.discardable.} =
  ## Adds each child of `children` to the `parent` node.
  ## Returns the `parent` node so that calls can be nested.
  for c in children:
    parent.add(c)
  parent
]#

proc del*(parent: RefNimNode, idx = 0, n = 1) =
  ## Deletes `n` children of `parent` starting at index `idx`.
  if parent.len == 0:
    raise newException(NoChildrenDefect, &"{parent.treeRepr}: has no children")

  if idx == parent.len - 1 and n >= 1:
    # delete last item
    parent.children.del(idx)
  else:
    # move items forward
    var ns = parent[(idx + n)..^1]
    for i in 0..<ns.len:
      parent[idx + i] = ns[i]
    parent.children.setLen(idx + ns.len)

proc eqIdent*(a: RefNimNode; b: string): bool =
  ## Style insensitive comparison.  `a` can be an identifier or a
  ## symbol. `a` may be wrapped in an export marker
  ## (`nnkPostfix`) or quoted with backticks (`nnkAccQuoted`),
  ## these nodes will be unwrapped.
  assert (a.kind == nnkIdent or a.kind == nnkSym)
  eqIdent(a.strVal, b)

proc eqIdent*(a: string; b: RefNimNode): bool =
  ## Style insensitive comparison.  `b` can be an identifier or a
  ## symbol. `b` may be wrapped in an export marker
  ## (`nnkPostfix`) or quoted with backticks (`nnkAccQuoted`),
  ## these nodes will be unwrapped.
  assert (b.kind == nnkIdent or b.kind == nnkSym)
  eqIdent(a, b.strVal)

proc eqIdent*(a: RefNimNode; b: RefNimNode): bool =
  ## Style insensitive comparison.  `a` and `b` can be an
  ## identifier or a symbol. Both may be wrapped in an export marker
  ## (`nnkPostfix`) or quoted with backticks (`nnkAccQuoted`),
  ## these nodes will be unwrapped.
  assert (a.kind == nnkIdent or a.kind == nnkSym) and (b.kind == nnkIdent or b.kind == nnkSym)
  eqIdent(a.strVal, b.strVal)

proc treeTraverse(n: RefNimNode; res: var string; level = 0; isLisp = false, indented = false) =
  if level > 0:
    if indented:
      res.add("\n")
      for i in 0 .. level-1:
        if isLisp:
          res.add(" ")          # dumpLisp indentation
        else:
          res.add("  ")         # dumpTree indentation
    else:
      res.add(" ")

  if isLisp:
    res.add("(")
  res.add(($n.kind).substr(3))

  case n.kind
  of nnkEmpty, nnkNilLit:
    discard # same as nil node in this representation
  of nnkCharLit .. nnkInt64Lit:
    res.add(" " & $n.intVal)
  of nnkFloatLit .. nnkFloat64Lit:
    res.add(" " & $n.floatVal)
  of nnkStrLit .. nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
    res.add(" " & $n.strVal.newLit.repr)
  of nnkNone:
    assert false
  elif n.kind in {nnkOpenSymChoice, nnkClosedSymChoice} and collapseSymChoice:
    res.add(" " & $n.len)
    if n.len > 0:
      var allSameSymName = true
      for i in 0..<n.len:
        if n[i].kind != nnkSym or not eqIdent(n[i], n[0]):
          allSameSymName = false
          break
      if allSameSymName:
        res.add(" " & $n[0].strVal.newLit.repr)
      else:
        for j in 0 ..< n.len:
          n[j].treeTraverse(res, level+1, isLisp, indented)
  else:
    for j in 0 ..< n.len:
      n[j].treeTraverse(res, level+1, isLisp, indented)

  if isLisp:
    res.add(")")

proc treeRepr*(n: RefNimNode): string =
  ## Convert the AST `n` to a human-readable tree-like string.
  ##
  ## See also `repr`, `lispRepr`, and `astGenRepr`.
  result = ""
  n.treeTraverse(result, isLisp = false, indented = true)

iterator items*(n: RefNimNode): RefNimNode =
  ## Iterates over the children of RefNimNode 'n'.
  for i in 0..<n.len:
    yield n[i]

iterator pairs*(n: NimNode): (int, NimNode) {.inline.} =
  ## Iterates over the children of the NimNode `n` and its indices.
  for i in 0 ..< n.len:
    yield (i, n[i])

iterator children*(n: NimNode): NimNode {.inline.} =
  ## Iterates over the children of the NimNode `n`.
  for i in 0 ..< n.len:
    yield n[i]

proc copyNode*(r: RefNimNode): RefNimNode =
  ## Shallow copy
  result = RefNimNode(kind: r.kind)
  case r.kind:
  of EmptyKinds: discard
  of IntKinds:
    result.intVal = r.intVal
  of FloatKinds:
    result.floatVal = r.floatVal
  of StrLitKinds:
    result.strVal = r.strVal
  else:
    discard

proc copyTree*(r: RefNimNode): RefNimNode =
  result = case r.kind:
    of LitKinds:
      copyNode(r)
    else:
      var n = RefNimNode(kind: r.kind)
      for c in r:
        n.add copyTree(c)
      n

proc toRef*(n: NimNode): RefNimNode {.inline.}
proc toRefNimNode*(n: NimNode): RefNimNode =
  result = RefNimNode(kind: n.kind)
  case n.kind:
    of nnkNone, nnkEmpty, nnkNilLit: discard
    of nnkCharLit..nnkUint64Lit:
      result.intVal =  n.intVal
    of nnkFloatLit..nnkFloat64Lit:
      result.floatVal = n.floatVal
    of nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
      result.strVal = n.strVal
    of nnkClosedSymChoice, nnkOpenSymChoice:
      result = RefNimNode(kind: nnkIdent, strVal: n[0].strVal)
    else:
      for c in n:
        var r = toRef(c)
        r.parent = result
        result.children.add r

proc toRef*(n: NimNode): RefNimNode {.inline.} =
  n.toRefNimNode()

proc toNimNode*(r: RefNimNode): NimNode =
  result = newNimNode(r.kind)
  case r.kind:
    of nnkNone, nnkEmpty, nnkNilLit: 
      discard
    of nnkCharLit..nnkUint64Lit:
      result.intVal = r.intVal
    of nnkFloatLit..nnkFloat64Lit:
      result.floatVal = r.floatVal
    of nnkStrLit..nnkTripleStrLit, nnkCommentStmt:
      result.strVal = r.strVal
    of nnkIdent, nnkSym:
      result = ident(r.strVal)
    else:
      for c in r:
        result.add toNimNode(c)

when isMainModule:
  import std / genasts
  macro typeTest(n: typed): untyped = 
    var t = getImpl(n).toRef()
    echo &"typeTest {t.treeRepr = }"
    result = newNimNode(nnkDiscardStmt)
    result.add newLit(1)

  static:
    var e = genAst:
      1 + _
    var r = e.toRef()
    var n = r[2]
    
    echo "-- replace test"
    echo r.treeRepr
    if n.kind == nnkIdent:
      n := newLit(4).toRef()
    echo r.treeRepr

    n := newLit(6).toRef()
    echo r.treeRepr

    echo "-- equivalence test"
    var a = newLit(6).toRef()
    assert a === n
    assert a != n
    assert n == n

    var b = RefNimNode(kind: nnkIntLit, intVal: 4)
    assert b !=== n

    echo "-- toNimNode"
    echo r.treeRepr
    var nimNode = r.toNimNode()
    echo nimNode.treeRepr

    echo "-- del test"
    r.del(2)
    echo r.treeRepr
    
    echo "-- no children test"
    try:
      var leaf = newLit(1).toRef()
      leaf.del()
    except NoChildrenDefect:
      echo "caught NoChildrenDefect"
    
    var intval:int = 10
    typeTest(intval)

    echo " -- copyTree"
    var rcopy = e.toRef()
    var og = rcopy
    var r2 = rcopy.copyTree()
    r2[2] := RefNimNode(kind: nnkIntLit, intVal: 1)
    echo rcopy.treeRepr
    echo r2.treeRepr
