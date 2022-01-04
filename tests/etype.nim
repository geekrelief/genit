
type FColor* = object
  r: float
  g*: float
  b: float

proc getR*(f: FColor): float =
  f.r

proc getG*(f: FColor): float =
  f.g

proc getB*(f: FColor): float =
  f.b