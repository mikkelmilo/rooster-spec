
type __ = Obj.t

type proper = __

type equiv = __

type 'm semigroup =
  'm -> 'm -> 'm
  (* singleton inductive, whose constructor was Build_Semigroup *)

(** val mappend : 'a1 semigroup -> 'a1 -> 'a1 -> 'a1 **)

let mappend semigroup0 =
  semigroup0

(** val maybeAppend :
    'a1 semigroup -> 'a1 Prelude.Maybe -> 'a1 Prelude.Maybe -> 'a1
    Prelude.Maybe **)

let maybeAppend h0 x y =
  match x with
  | Prelude.Just x0 ->
    (match y with
     | Prelude.Just y0 -> Prelude.Just (mappend h0 x0 y0)
     | Prelude.Nothing -> x)
  | Prelude.Nothing -> y

(** val semigroup_Maybe : 'a1 semigroup -> 'a1 Prelude.Maybe semigroup **)

let semigroup_Maybe =
  maybeAppend
