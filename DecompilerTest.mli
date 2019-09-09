
type nat =
| O
| S of nat

val id : 'a1 -> 'a1

module Main :
 sig
  type unop =
  | OSucc
  | OPred

  type binop =
  | OPlus
  | OMult
  | OMinus

  type exp =
  | ENat of nat
  | EUnOp of unop * exp
  | EBinOp of binop * exp * exp
  | EIf of exp * exp * exp

  type instruction =
  | IPush of nat
  | IUnOp of unop
  | IBinOp of binop
  | IChoose

  type program = instruction ([])

  type 'a stack_machine_functions = { constr_push : (nat -> 'a);
                                      constr_unop : (unop -> 'a -> 'a);
                                      constr_binop : (binop -> 'a -> 'a -> 'a);
                                      constr_if : ('a -> 'a -> 'a -> 'a);
                                      exp_to_elem : (exp -> 'a) }

  val constr_push : 'a1 stack_machine_functions -> nat -> 'a1

  val constr_unop : 'a1 stack_machine_functions -> unop -> 'a1 -> 'a1

  val constr_binop : 'a1 stack_machine_functions -> binop -> 'a1 -> 'a1 -> 'a1

  val constr_if : 'a1 stack_machine_functions -> 'a1 -> 'a1 -> 'a1 -> 'a1

  val stack_machine_state_transition :
    'a1 stack_machine_functions -> instruction -> 'a1 ([]) -> 'a1 ([])
    Prelude.Maybe

  val stack_machine :
    'a1 stack_machine_functions -> program -> 'a1 ([]) -> 'a1 ([])
    Prelude.Maybe

  val decompile_functions : exp stack_machine_functions

  val decompile : program -> exp Prelude.Maybe
 end
