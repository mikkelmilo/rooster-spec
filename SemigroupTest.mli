
type __ = Obj.t

type proper = __

type equiv = __

type 'm semigroup =
  'm -> 'm -> 'm
  (* singleton inductive, whose constructor was Build_Semigroup *)

val mappend : 'a1 semigroup -> 'a1 -> 'a1 -> 'a1

val maybeAppend :
  'a1 semigroup -> 'a1 Prelude.Maybe -> 'a1 Prelude.Maybe -> 'a1 Prelude.Maybe

val semigroup_Maybe : 'a1 semigroup -> 'a1 Prelude.Maybe semigroup
