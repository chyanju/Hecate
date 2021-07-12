signature OPTIMIZE =
sig
  val foldIf : Expr.t -> Expr.t
end


structure Optimize = struct

structure E = Expr
structure BO = Binop
structure BR = Binrel

fun flip BR.EQ = BR.NEQ
  | flip BR.NEQ = BR.EQ
  | flip BR.GT = BR.LE
  | flip BR.GE = BR.LT
  | flip BR.LT = BR.GE
  | flip BR.LE = BR.GT

fun foldAnd (E.Binop(BO.And, E.Bool(true), e)) = e
  | foldAnd (E.Binop(BO.And, e, E.Bool(true))) = e
  | foldAnd (E.Binop(BO.And, E.Bool(false), e)) = E.Bool(false)
  | foldAnd (E.Binop(BO.And, e, E.Bool(false))) = E.Bool(false)
  | foldAnd e = e

fun foldOr (E.Binop(BO.Or, E.Bool(true), e)) = E.Bool(true)
  | foldOr (E.Binop(BO.Or, e, E.Bool(true))) = E.Bool(true)
  | foldOr (E.Binop(BO.Or, E.Bool(false), e)) = e
  | foldOr (E.Binop(BO.Or, e, E.Bool(false))) = e
  | foldOr e = e

fun negate (E.Binrel(rel, e1, e2)) = E.Binrel(flip rel, e1, e2)
  | negate e = e

fun foldIf (E.If(c, E.Bool(true), f)) =
    foldOr (E.Binop(BO.Or, c, foldIf f))
  | foldIf (E.If(c, E.Bool(false), f)) =
    foldAnd (E.Binop(BO.And, negate c, foldIf f))
  | foldIf e = e
end