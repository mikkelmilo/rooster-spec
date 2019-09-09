
type nat =
| O
| S of nat

val fst : ('a1,'a2) -> 'a1

val snd : ('a1,'a2) -> 'a2

type comparison =
| Eq
| Lt
| Gt

val compOpp : comparison -> comparison

type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

type int =
| Pos of uint
| Neg of uint

val revapp : uint -> uint -> uint

val rev : uint -> uint

module Little :
 sig
  val double : uint -> uint

  val succ_double : uint -> uint
 end

val add : nat -> nat -> nat

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos :
 sig
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end

module Coq_Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  val pred_N : positive -> n

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : positive -> mask

  val sub_mask : positive -> positive -> mask

  val sub_mask_carry : positive -> positive -> mask

  val sub : positive -> positive -> positive

  val mul : positive -> positive -> positive

  val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1

  val square : positive -> positive

  val div2 : positive -> positive

  val div2_up : positive -> positive

  val size_nat : positive -> nat

  val size : positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val eqb : positive -> positive -> Prelude.Bool

  val leb : positive -> positive -> Prelude.Bool

  val sqrtrem_step :
    (positive -> positive) -> (positive -> positive) -> (positive,mask) ->
    positive,mask

  val sqrtrem : positive -> positive,mask

  val sqrt : positive -> positive

  val gcdn : nat -> positive -> positive -> positive

  val gcd : positive -> positive -> positive

  val ggcdn : nat -> positive -> positive -> positive,(positive,positive)

  val ggcd : positive -> positive -> positive,(positive,positive)

  val coq_Nsucc_double : n -> n

  val coq_Ndouble : n -> n

  val coq_lor : positive -> positive -> positive

  val coq_land : positive -> positive -> n

  val ldiff : positive -> positive -> n

  val coq_lxor : positive -> positive -> n

  val testbit : positive -> n -> Prelude.Bool

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> nat

  val of_succ_nat : nat -> positive

  val of_uint_acc : uint -> positive -> positive

  val of_uint : uint -> n

  val to_little_uint : positive -> uint

  val to_uint : positive -> uint
 end

module N :
 sig
  val succ_double : n -> n

  val double : n -> n

  val succ_pos : n -> positive

  val sub : n -> n -> n

  val compare : n -> n -> comparison

  val leb : n -> n -> Prelude.Bool

  val pos_div_eucl : positive -> n -> n,n

  val coq_lor : n -> n -> n

  val coq_land : n -> n -> n

  val ldiff : n -> n -> n

  val coq_lxor : n -> n -> n

  val testbit : n -> n -> Prelude.Bool
 end

module Z :
 sig
  type t = z

  val zero : z

  val one : z

  val two : z

  val double : z -> z

  val succ_double : z -> z

  val pred_double : z -> z

  val pos_sub : positive -> positive -> z

  val add : z -> z -> z

  val opp : z -> z

  val succ : z -> z

  val pred : z -> z

  val sub : z -> z -> z

  val mul : z -> z -> z

  val pow_pos : z -> positive -> z

  val pow : z -> z -> z

  val square : z -> z

  val compare : z -> z -> comparison

  val sgn : z -> z

  val leb : z -> z -> Prelude.Bool

  val ltb : z -> z -> Prelude.Bool

  val geb : z -> z -> Prelude.Bool

  val gtb : z -> z -> Prelude.Bool

  val eqb : z -> z -> Prelude.Bool

  val max : z -> z -> z

  val min : z -> z -> z

  val abs : z -> z

  val abs_nat : z -> nat

  val abs_N : z -> n

  val to_nat : z -> nat

  val to_N : z -> n

  val of_nat : nat -> z

  val of_N : n -> z

  val to_pos : z -> positive

  val of_uint : uint -> z

  val of_int : int -> z

  val to_int : z -> int

  val iter : z -> ('a1 -> 'a1) -> 'a1 -> 'a1

  val pos_div_eucl : positive -> z -> z,z

  val div_eucl : z -> z -> z,z

  val div : z -> z -> z

  val modulo : z -> z -> z

  val quotrem : z -> z -> z,z

  val quot : z -> z -> z

  val rem : z -> z -> z

  val even : z -> Prelude.Bool

  val odd : z -> Prelude.Bool

  val div2 : z -> z

  val quot2 : z -> z

  val log2 : z -> z

  val sqrtrem : z -> z,z

  val sqrt : z -> z

  val gcd : z -> z -> z

  val ggcd : z -> z -> z,(z,z)

  val testbit : z -> z -> Prelude.Bool

  val shiftl : z -> z -> z

  val shiftr : z -> z -> z

  val coq_lor : z -> z -> z

  val coq_land : z -> z -> z

  val ldiff : z -> z -> z

  val coq_lxor : z -> z -> z
 end

module ZZ :
 sig
  type t = z

  val zero : z

  val one : z

  val two : z

  val double : z -> z

  val succ_double : z -> z

  val pred_double : z -> z

  val pos_sub : positive -> positive -> z

  val add : z -> z -> z

  val opp : z -> z

  val succ : z -> z

  val pred : z -> z

  val sub : z -> z -> z

  val mul : z -> z -> z

  val pow_pos : z -> positive -> z

  val pow : z -> z -> z

  val square : z -> z

  val compare : z -> z -> comparison

  val sgn : z -> z

  val leb : z -> z -> Prelude.Bool

  val ltb : z -> z -> Prelude.Bool

  val geb : z -> z -> Prelude.Bool

  val gtb : z -> z -> Prelude.Bool

  val eqb : z -> z -> Prelude.Bool

  val max : z -> z -> z

  val min : z -> z -> z

  val abs : z -> z

  val abs_nat : z -> nat

  val abs_N : z -> n

  val to_nat : z -> nat

  val to_N : z -> n

  val of_nat : nat -> z

  val of_N : n -> z

  val to_pos : z -> positive

  val of_uint : uint -> z

  val of_int : int -> z

  val to_int : z -> int

  val iter : z -> ('a1 -> 'a1) -> 'a1 -> 'a1

  val pos_div_eucl : positive -> z -> z,z

  val div_eucl : z -> z -> z,z

  val div : z -> z -> z

  val modulo : z -> z -> z

  val quotrem : z -> z -> z,z

  val quot : z -> z -> z

  val rem : z -> z -> z

  val even : z -> Prelude.Bool

  val odd : z -> Prelude.Bool

  val div2 : z -> z

  val quot2 : z -> z

  val log2 : z -> z

  val sqrtrem : z -> z,z

  val sqrt : z -> z

  val gcd : z -> z -> z

  val ggcd : z -> z -> z,(z,z)

  val testbit : z -> z -> Prelude.Bool

  val shiftl : z -> z -> z

  val shiftr : z -> z -> z

  val coq_lor : z -> z -> z

  val coq_land : z -> z -> z

  val ldiff : z -> z -> z

  val coq_lxor : z -> z -> z
 end

val myadd : z -> z -> z
