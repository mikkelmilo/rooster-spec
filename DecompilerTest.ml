
type nat =
| O
| S of nat

(** val id : 'a1 -> 'a1 **)

let id x =
  x

module Main =
 struct
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

  (** val constr_push : 'a1 stack_machine_functions -> nat -> 'a1 **)

  let constr_push x = x.constr_push

  (** val constr_unop : 'a1 stack_machine_functions -> unop -> 'a1 -> 'a1 **)

  let constr_unop x = x.constr_unop

  (** val constr_binop :
      'a1 stack_machine_functions -> binop -> 'a1 -> 'a1 -> 'a1 **)

  let constr_binop x = x.constr_binop

  (** val constr_if :
      'a1 stack_machine_functions -> 'a1 -> 'a1 -> 'a1 -> 'a1 **)

  let constr_if x = x.constr_if

  (** val stack_machine_state_transition :
      'a1 stack_machine_functions -> instruction -> 'a1 ([]) -> 'a1 ([])
      Prelude.Maybe **)

  let stack_machine_state_transition funcs insn s =
    match insn with
    | IPush n -> Prelude.Just ((:) ((funcs.constr_push n), s))
    | IUnOp op ->
      (match s with
       | ([]) -> Prelude.Nothing
       | (:) (x, s') -> Prelude.Just ((:) ((funcs.constr_unop op x), s')))
    | IBinOp op ->
      (match s with
       | ([]) -> Prelude.Nothing
       | (:) (y, l) ->
         (match l with
          | ([]) -> Prelude.Nothing
          | (:) (x, s') ->
            Prelude.Just ((:) ((funcs.constr_binop op x y), s'))))
    | IChoose ->
      (match s with
       | ([]) -> Prelude.Nothing
       | (:) (cond, l) ->
         (match l with
          | ([]) -> Prelude.Nothing
          | (:) (v2, l0) ->
            (match l0 with
             | ([]) -> Prelude.Nothing
             | (:) (v1, s') ->
               Prelude.Just ((:) ((funcs.constr_if cond v1 v2), s')))))

  (** val stack_machine :
      'a1 stack_machine_functions -> program -> 'a1 ([]) -> 'a1 ([])
      Prelude.Maybe **)

  let rec stack_machine funcs insns s =
    match insns with
    | ([]) -> Prelude.Just s
    | (:) (insn, insns') ->
      (match stack_machine_state_transition funcs insn s with
       | Prelude.Just s' -> stack_machine funcs insns' s'
       | Prelude.Nothing -> Prelude.Nothing)

  (** val decompile_functions : exp stack_machine_functions **)

  let decompile_functions =
    { constr_push = (fun x -> ENat x); constr_unop = (fun x x0 -> EUnOp (x,
      x0)); constr_binop = (fun x x0 x1 -> EBinOp (x, x0, x1)); constr_if =
      (fun cond e1 e2 -> EIf (cond, e2, e1)); exp_to_elem = id }

  (** val decompile : program -> exp Prelude.Maybe **)

  let decompile insns =
    match stack_machine decompile_functions insns ([]) with
    | Prelude.Just l ->
      (match l with
       | ([]) -> Prelude.Nothing
       | (:) (res, l0) ->
         (match l0 with
          | ([]) -> Prelude.Just res
          | (:) (_, _) -> Prelude.Nothing))
    | Prelude.Nothing -> Prelude.Nothing
 end
