(*
   Functional Programming 2018
   Term project
   Artihmetic expression decompiler

   Christoffer Müller Madsen and
   Jon Michael Aanes

   December 2018
 *)

Require Import List Coq.Arith.Arith Coq.Arith.EqNat Bool.
Import ListNotations.


(* ################################################################# *)

Module Main.

(** * Syntax *)

(** ** Expression language *)

(** The expression language considered in this project is very
    simple. It can describe arithmetics on natural numbers with a select
    set of unary and binary operations. The operations defined for the
    basic version of the language were the unary operations successor
    and predecessor, and the binary operations addition and
    multiplication.

    Since then, we have added subtraction and conditional expressions.

    We define the expression language inductively. *)

Inductive unop :=
  | OSucc : unop
  | OPred : unop.

Inductive binop :=
  | OPlus : binop
  | OMult : binop
  | OMinus : binop.

Inductive exp :=
  | ENat   : nat   -> exp
  | EUnOp  : unop  -> exp -> exp
  | EBinOp : binop -> exp -> exp -> exp
  | EIf    : exp   -> exp -> exp -> exp.


(** ** Stack machine language *)

(** The stack machine language considered is similarly simple. It
    supports pushing natural numbers to the stack and performing unary
    and binary operations on these. The instructions are in reverse
    Polish notation. *)

Inductive instruction :=
  | IPush   : nat   -> instruction
  | IUnOp   : unop  -> instruction
  | IBinOp  : binop -> instruction
  | IChoose : instruction.

(** We define the type [stack] to lighten the syntactical weight of the
    Coq code. *)

Definition stack := list nat.
Definition program := list instruction.

(* ================================================================= *)
(** * Expression evaluation *)

(** We define the function [eval] to give semantics to expressions. *)

Definition eval_unop (op : unop) (v : nat) : nat :=
  match op with
    | OSucc => S v
    | OPred => pred v
  end.

Definition eval_binop (op : binop) (v1 v2 : nat) : nat :=
  match op with
    | OPlus => v1 + v2
    | OMult => v1 * v2
    | OMinus => v1 - v2
  end.

Fixpoint eval (e : exp) : nat :=
  match e with
    | ENat   n          => n
    | EUnOp  op   e     => eval_unop op (eval e)
    | EBinOp op   e1 e2 => eval_binop op (eval e1) (eval e2)
    | EIf    cond e1 e2 => if eval cond then eval e2 else eval e1
  end.


(** We test the [eval] function  *)

Example eval_1 : eval (EBinOp OMult (ENat 4) (ENat 3)) = 12.
Proof. simpl. reflexivity. Qed.

Example eval_2 : eval (ENat 4) = 4.
Proof. simpl. reflexivity. Qed.

Example eval_3 : eval (EUnOp OSucc (ENat 4)) = 5.
Proof. simpl. reflexivity. Qed.

Example eval_4 : eval (EUnOp OPred (ENat 4)) = 3.
Proof. simpl. reflexivity. Qed.

Example eval_5 : eval (EBinOp OPlus (EUnOp OPred (ENat 0)) (EUnOp OSucc (EBinOp OMult (ENat 5) (ENat 3)))) = 16.
Proof. simpl. reflexivity. Qed.

Example eval_6 : eval (EBinOp OMinus (ENat 4) (ENat 3)) = 1.
Proof. simpl. reflexivity. Qed.

Example eval_7 : eval (EBinOp OMinus (ENat 3) (ENat 4)) = 0.
Proof. simpl. reflexivity. Qed.

Example eval_if_1 : eval (EIf (ENat 0) (ENat 1) (ENat 2)) = 2.
Proof. simpl. reflexivity. Qed.

Example eval_if_2 : eval (EIf (ENat 3) (ENat 1) (ENat 2)) = 1.
Proof. simpl. reflexivity. Qed.

Example eval_if_3 : eval (EIf (EBinOp OMult (ENat 4) (ENat 3)) (ENat 1) (ENat 2)) = 1.
Proof. simpl. reflexivity. Qed.

(** It seems to work.  *)


(* ================================================================= *)
(** * Stack machine *)

(** ** General stack machine  *)

(** We observed that the virtual machine and decompiler were nearly
    equivalent in implementation.  For this reason, we generalised the
    shared parts.  The record type [stack_machine_functions] specifies
    the semantics of the general stack machine.

    The function [exp_to_elem] is not used in the actual implementation
    of the stack machine; its purpose is solely to signify what the
    equivalent of running the stack machine on an input would
    be. Running the stack machine on a compiled expression [e] should be
    equivalent to evaluating [exp_to_elem e]. We provide a proof for
    this later.

    The [props] element is a proof that applying [exp_to_elem] on an
    expression is equivalent to running the stack machine with the
    functions specified in the record.

    This will become more clear as you read along. *)

Definition exp_to_elem_props
           {A : Type}
           (exp_to_elem : exp -> A)
           exp_to_elem_push
           exp_to_elem_unop
           exp_to_elem_binop
           exp_to_elem_if : Prop :=

  (forall (n : nat),
      exp_to_elem (ENat n) = exp_to_elem_push n) /\

  (forall (e : exp) (op : unop),
      exp_to_elem (EUnOp op e) = exp_to_elem_unop op (exp_to_elem e)) /\

  (forall (e1 e2 : exp) (op : binop),
      exp_to_elem (EBinOp op e1 e2) = exp_to_elem_binop op (exp_to_elem e1) (exp_to_elem e2)) /\

  (forall (cond e1 e2 : exp),
      exp_to_elem (EIf cond e1 e2) =
      exp_to_elem_if (exp_to_elem cond) (exp_to_elem e2) (exp_to_elem e1)).

Record stack_machine_functions {A : Type} :=
  {
    constr_push  : nat -> A;
    constr_unop  : unop -> A -> A;
    constr_binop : binop -> A -> A -> A;
    constr_if    : A -> A -> A -> A;
    exp_to_elem  : exp -> A;
      }.

(** For later use: A proof tactic to unpack the propositions from a
    record of type [stack_machine_functions] into the local proof
    context.  *)



(** We define a function to execute a single step of the stack
    machine. It takes as input a [stack_machine_functions] and a single
    instruction and outputs a correspondingly updated stack. 
*)

Definition stack_machine_state_transition
           {A: Type}
           (funcs: stack_machine_functions)
           (insn: instruction)
           (s: list A) : option (list A) :=

  match insn, s with
    | IPush n, s'
      => Some ((constr_push funcs n) :: s')

    | IUnOp op, x :: s'
      => Some ((constr_unop funcs op x) :: s')

    | IBinOp op, y :: x :: s'
      => Some ((constr_binop funcs op x y) :: s')

    | IChoose, cond :: v2 :: v1 :: s'
      => Some ((constr_if funcs cond v1 v2) :: s')

    | _, _ => None
  end.

(** We can now define the general stack machine as the recursive
    application of [stack_machine_state_transition] on a [program],
    returning the final stack when all instructions have been
    executed. *)

Fixpoint stack_machine {A : Type} (funcs : stack_machine_functions) (insns : program) (s : list A) : option (list A) :=
  match insns with
    | insn :: insns' => match stack_machine_state_transition funcs insn s with
                          | Some s' => stack_machine funcs insns' s'
                          | _       => None
                        end
    | [] => Some s
  end.

(** ** Virtual machine  *)

(** We can now begin defining the specialisation of the general stack
    machine to be used as the virtual machine.

    First, look at [vm_functions], which specifies the functions used
    for the virtual machine. The proof needed to specify the record is
    provided as [vm_functions_props].

    [exp_to_elem] is set to be the [eval] function, signifying that
    running the virtual machine on a compiled expression is equivalent
    to evaluating the expression using [eval]. *)

Lemma vm_functions_props : exp_to_elem_props eval id eval_unop eval_binop (fun (cond v1 v2 : nat) => (if cond then v1 else v2)).
Proof.
  unfold exp_to_elem_props. auto.
Qed.

Definition vm_functions :=
  {|
    constr_push  := fun (n : nat) => n;
    constr_unop  := eval_unop;
    constr_binop := eval_binop;
    constr_if    := fun (cond v1 v2 : nat) => (if cond then v1 else v2);
    exp_to_elem  := eval;
  |}.


(** The main virtual machine function [vm] can now be defined succinctly
    in terms of [stack_machine]. *)

Definition vm (insns : program) : option nat :=
  match stack_machine vm_functions insns nil with
    | Some (x :: _) => Some x
    | _ => None
  end.

(** We test the [vm] function *)

Example vm_test_1 : vm [IPush 2] = Some 2.
Proof. simpl. reflexivity. Qed.

Example vm_test_2 : vm [IPush 2 ; IPush 3] = Some 3.
Proof. simpl. reflexivity. Qed.

Example vm_test_3 : vm [IPush 2 ; IPush 3 ; IBinOp OPlus] = Some 5.
Proof. simpl. reflexivity. Qed.

Example vm_test_4 : vm [IPush 4 ; IPush 2 ; IPush 3 ; IBinOp OPlus ; IBinOp OMult] = Some 20.
Proof. simpl. reflexivity. Qed.

Example vm_test_5 : vm [IPush 4 ; IPush 2 ; IPush 3 ; IBinOp OPlus ; IBinOp OMult ; IUnOp OSucc] = Some 21.
Proof. simpl. reflexivity. Qed.

Example vm_test_6 : vm [IPush 4 ; IPush 2 ; IPush 3 ; IUnOp OPred ; IBinOp OPlus ; IBinOp OMult ; IUnOp OSucc] = Some 17.
Proof. simpl. reflexivity. Qed.

Example vm_test_7 : vm [] = None.
Proof. simpl. reflexivity. Qed.

Example vm_test_8 : vm [IUnOp OSucc] = None.
Proof. simpl. reflexivity. Qed.

Example vm_test_9 : vm [IPush 2 ; IBinOp OPlus] = None.
Proof. simpl. reflexivity. Qed.

Example vm_test_10 : vm [IPush 4 ; IPush 1 ; IBinOp OMinus] = Some 3.
Proof. simpl. reflexivity. Qed.

Example vm_test_11 : vm [IPush 1 ; IPush 4 ; IBinOp OMinus] = Some 0.
Proof. simpl. reflexivity. Qed.

Example vm_test_choose_1 : vm [ IPush 4 ; IPush 1 ; IPush 0 ; IChoose ] = Some 4.
Proof. simpl. reflexivity. Qed.

Example vm_test_choose_2 : vm [ IPush 4 ; IPush 2 ; IPush 1 ; IChoose ] = Some 2.
Proof. simpl. reflexivity. Qed.

Example vm_test_choose_3 : vm ([IPush 4 ; IPush 2 ; IPush 3 ; IBinOp OPlus ; IBinOp OMult] ++ [IPush 2 ; IPush 3 ; IBinOp OPlus] ++ [IPush 4 ; IPush 2 ; IPush 3 ; IUnOp OPred ; IBinOp OPlus ; IBinOp OMult ; IUnOp OSucc] ++ [IChoose]) = Some 5.
Proof. simpl. reflexivity. Qed.

(** It seems to work. *)

(* ================================================================= *)
(** * Compiler *)

(** We give a functional definition of the compiler for the stack
    machine, which is fairly trivial.  *)

Fixpoint compile (e : exp) : program :=
  match e with
    | ENat   n          => [ IPush n ]
    | EUnOp  op e       => compile e ++ [ IUnOp op ]
    | EBinOp op e1 e2   => compile e1 ++ compile e2 ++ [ IBinOp op ]
    | EIf    cond e1 e2 => compile e2 ++ compile e1 ++ compile cond ++ [ IChoose ]
  end.

Example compile_test_1 : compile (ENat 3) = [ IPush 3 ].
Proof. simpl. reflexivity. Qed.

Example compile_test_2 : compile (EBinOp OPlus (ENat 3) (ENat 4)) = [ IPush 3 ; IPush 4 ; IBinOp OPlus ].
Proof. simpl. reflexivity. Qed.

Example compile_test_3 : compile (EBinOp OPlus (EUnOp OSucc (ENat 3)) (ENat 4)) = [ IPush 3 ; IUnOp OSucc ; IPush 4 ; IBinOp OPlus ].
Proof. simpl. reflexivity. Qed.

Example compile_test_4 : compile (EBinOp OMult (EUnOp OPred (ENat 3)) (ENat 4)) = [ IPush 3 ; IUnOp OPred ; IPush 4 ; IBinOp OMult ].
Proof. simpl. reflexivity. Qed.

Example compile_test_5 : compile (EBinOp OMult (EUnOp OPred (EUnOp OSucc (ENat 3))) (ENat 4)) = [ IPush 3 ; IUnOp OSucc ; IUnOp OPred ; IPush 4 ; IBinOp OMult ].
Proof. simpl. reflexivity. Qed.

Example compile_test_if_1 : compile (EBinOp OPlus
                                            (EIf (EUnOp OSucc (ENat 0))
                                                 (EUnOp OPred (ENat 2))
                                                 (EUnOp OSucc (ENat 4)))
                                            (ENat 41))
                            = [ IPush 4 ; IUnOp OSucc ; IPush 2 ; IUnOp OPred ; IPush 0 ; IUnOp OSucc ; IChoose ; IPush 41 ; IBinOp OPlus ].
Proof. simpl. reflexivity. Qed.

(* ================================================================= *)
(** * Correctness of virtual machine and compiler *)

(** We previously postulated that running the virtual machine on a
    compiled expression is equivalent to applying [eval] to that
    expression.

    In this section, we prove that this proposition holds. *)


(** ** QuickChick intermezzo *)

(** Before giving a formal proof of the proposition, we see if
    QuickChick can find a counter-example. *)

(** The proposition is posed as *)

(* Definition vm_correct_prop (e : exp) := vm (compile e) = Some (eval e)?. *)

(** Before we can check the propositions, we need to do some plumbing
    for QuickChick. *)

(** Deriving the [Show] typeclass for the inductive datatypes is
    necessary for QuickChick usage. *)

(* Derive Show for unop.
Derive Show for binop.
Derive Show for exp.

(** Equality of two expressions is easily decidable, it is trivially
    proved by [dec_eq].  *)

Instance eq_dec_exp (exp1 exp2 : exp) : Dec (exp1 = exp2) := {}.
Proof. dec_eq. Defined.

 *)
(** Next, we implement a function for generating expressions with some
    notion of size. We generate expressions in a straight-forward
    recursive way. *)

(* Fixpoint genSizedexp (size : nat) : G exp :=
  match size with
    | O => (n <- choose (1, 40) ;;
            ret (ENat n))
    | S size' =>
      freq [ (1, (n <- choose (1, 40) ;;
                  ret (ENat n))) ;
             (size, e <- genSizedexp size' ;;
                    op <- (elems [ OSucc ; OPred ]) ;;
                    ret (EUnOp op e)) ;
             (size, e1 <- genSizedexp size' ;;
                    e2 <- genSizedexp size' ;;
                    op <- (elems [ OPlus ; OMult ; OMinus ]) ;;
                   ret (EBinOp op e1 e2)) ;
             (size, e1 <- genSizedexp size' ;;
                    e2 <- genSizedexp size' ;;
                    e3 <- genSizedexp size' ;;
                    ret (EIf e1 e2 e3))]
  end.
 *)
(** We also need to implement a shrinker for expressions. We shrink by
    either
    - shrinking an expression to one of its parts, i.e. shrinking a
      binary operation to one of the expressions it operates on, or
    - mapping an expression to an expression of the same types, with one
      of its parts shrunk, i.e. shrinking a binary operation to the same
      binary operation, with one of the expressions it operates on being
      shrunk.  *)
(* 
Fixpoint shrinkExp (e : exp) : list exp :=
  match e with
    | ENat v => map (fun n => ENat n) (shrink v)
    | EUnOp op e1 => e1 :: shrinkExp e1
    | EBinOp op e1 e2 =>
      [e1 ; e2]
        ++ map (fun e' => EBinOp op e' e2) (shrinkExp e1)
        ++ map (fun e' => EBinOp op e1 e') (shrinkExp e2)
    | EIf cond e1 e2 =>
      [e1 ; e2]
        ++ map (fun cond' => EIf cond' e1 e2) (shrinkExp cond)
        ++ map (fun e' => EIf cond e' e2) (shrinkExp e1)
        ++ map (fun e' => EIf cond e1 e') (shrinkExp e2)
  end.
 *)
(** Now that we have done the necessary plumbing, we can check the proposition. *)

(* QuickChick (forAllShrink (genSizedexp 2) shrinkExp vm_correct_prop).
 *)
(**
    +++ Passed 10000 tests (0 discards)

    It seems to work!
*)

(** ** A formal proof  *)

(** We now give a formal proof of the same proposition.

    In order to give the proof, we need a few lemmas stating properties
    about the virtual stack machine. We state these propositions as
    properties of the general stack machine in order to be able to
    re-use the propositions for later proofs.
 *)

(** The stack machine should have the transitive property,
    i.e. evaluating a list of instructions combined from two sublists
    should be the same as evaluating the two sublists sequentially. *)

Lemma stack_machine_trans : forall (A : Type) funcs insns1 insns2 s s',
    @stack_machine A funcs insns1 s = Some s' ->
    stack_machine funcs (insns1 ++ insns2) s =
    stack_machine funcs (insns2) s'.

(** The proof is by induction over the first list of instructions. *)

Proof.
  intros A funcs insns1.
  induction insns1; intros insns2 s s' H1. 

  (** The case of the empty list of instructions is trivial; the stack
      remains the same.  *)

  - inversion H1. reflexivity.

  (** The induction case requires a bit more work. Here, we do case
      analysis on the type of the instruction at the head of the
      list. *)

  - rewrite <- app_comm_cons.
    destruct a;

      (** In the [INat] case the induction hypothesis and the assumption
          easily proves the proposition. *)

      apply IHinsns1, H1 ||

      (** The cases of [IUnOp], [IBinOp], and [IChoose] require a bit more work.

          We destruct the stack. The stack cannot be empty. These cases
          are solved by [inversion]. In the [cons] case, we apply the
          induction hypothesis after which the proofs is finalised by
          the anitial assumption.

          [IUnOp] requires a single pass of the stack destruction,
          [IBinOp] requires two, and [IChoose] requires three. *)

      (repeat (destruct s; inversion H1; try apply IHinsns1, H1)).

    (** And we are done! *)

Qed.


(** Evaluating a list of instructions, where the prefix of the list is a
    compiled expression should be equivalent to evaluting the rest of
    the list of instructions with the value of the expression on top of
    the original stack.

    The proof is by induction on [e]. The [props] entry of the
    [stack_machine_functions] can be used in all cases to rewriting the
    terms. Rewriting with the induction hypotheses then solves each case.
 *)

(* Lemma stack_machine_compile : forall {A : Type} (funcs : stack_machine_functions)
                                     (e : exp) (insns : program) (s : list A),
    @stack_machine A funcs (compile e ++ insns) s =
    stack_machine funcs insns ((exp_to_elem funcs e) :: s).
Proof.
  intros A funcs e.
  UnpackFunctionPropositions funcs.
  induction e; (intros insns s; apply stack_machine_trans; simpl).
  - rewrite H_push. reflexivity.
  - rewrite H_unop, IHe. reflexivity.
  - rewrite H_binop, IHe1, IHe2. reflexivity.
  - rewrite H_if, IHe3, IHe2, IHe1. reflexivity.
Qed. *)

(** We now prove a general version of the main proposition of this
    section.

    The proof is by case analysis of the expression [e]. All cases can
    be solved by rewriting a number of times with
    [stack_machine_compile] and the propositions given by the
    [stack_machine_functions] record.

    Rewriting with [stack_machine_compile] is equivalent to stepping the
    stack machine through a complete expression. The [props] in the
    [stack_machine_functions] record gives us the properties needed to
    do this. *)

(* Lemma stack_machine_correct : forall {A : Type} (funcs : stack_machine_functions) (e : exp),
    @stack_machine A funcs (compile e) [] = Some [(exp_to_elem funcs) e].
Proof.
  intros A funcs e.
  UnpackFunctionPropositions funcs.
  destruct e; simpl; repeat rewrite stack_machine_compile; simpl; congruence.
Qed. *)


(** The final proposition of this section follows as a specialisation of
    the general lemma above: the composition of [vm] and [compile] are
    equivalent to [eval] for all expressions. *)
(* 
Theorem vm_correct : forall e,
    vm (compile e) = Some (eval e).
Proof.
  intros e.
  unfold vm.
  rewrite stack_machine_correct.
  reflexivity.
Qed. *)

(* ================================================================= *)
(** * Decompiler *)

(** With the interpreter, compiler and virtual machine defined and proved
    correct, we now begin the work on the decompiler. *)

(** As with the virtual machine, we use the general stack machine as the
    foundation of the decompiler. The [exp_to_elem] function is [id]:
    compiling an expression and executing the result with the decompiler
    should be equivalent to the original expression. *)

Lemma decompile_functions_props : exp_to_elem_props id ENat EUnOp EBinOp (fun (cond e1 e2 : exp) => EIf cond e2 e1).
Proof.
  unfold exp_to_elem_props. auto.
Qed.

Definition decompile_functions :=
  {|
    constr_push  := ENat;
    constr_unop  := EUnOp;
    constr_binop := EBinOp;
    constr_if    := fun (cond e1 e2 : exp) => EIf cond e2 e1;
    exp_to_elem  := id;

  |}.

Definition decompile (insns : program) : option exp :=
    match stack_machine decompile_functions insns [] with
      | Some [res] => Some res
      | _ => None
    end.

(** We test the decompiler with some simple tests. *)

Example decompile_test_1 : decompile [IPush 42] =
                           Some (ENat 42).
Proof. simpl. reflexivity. Qed.

Example decompile_test_2
  : decompile [ IPush 2 ; IPush 40 ; IBinOp OPlus ] =
    Some (EBinOp OPlus (ENat 2) (ENat 40)).
Proof. simpl. reflexivity. Qed.

Example decompile_test_3
  : decompile [ IPush 1 ; IUnOp OSucc ; IPush 40 ; IBinOp OPlus ] =
    Some (EBinOp OPlus (EUnOp OSucc (ENat 1)) (ENat 40)).
Proof. simpl. reflexivity. Qed.

Example decompile_test_if_1
  : decompile [ IPush 4 ; IUnOp OSucc ; IPush 2 ; IUnOp OPred ; IPush 0 ; IUnOp OSucc ; IChoose ; IPush 41 ; IBinOp OPlus ] =
    Some ((EBinOp OPlus
            (EIf (EUnOp OSucc (ENat 0))
                 (EUnOp OPred (ENat 2))
                 (EUnOp OSucc (ENat 4)))
            (ENat 41))).
Proof. simpl. reflexivity. Qed.

(** It seems to work. *)

(** ** Left-inverse *)

(** We want the decompiler to be the left-inverse of the compiler,
    i.e. decompiling any compiled expression should yield a result equal
    to the original expression. *)

(** *** QuickChick intermezzo  *)

(** Before we give a formal proof of the proposition, we use QuickChick
    to check for counter-examples.

    We state the proposition: *)

(* Definition decompile_correct_prop (e : exp) := decompile (compile e) = (Some e)?.

(** And check it with QuickChick:  *)

QuickChick (forAllShrink (genSizedexp 2) shrinkExp vm_correct_prop).
 *)
(**
    +++ Passed 10000 tests (0 discards)

    It seems to work!
*)

(** *** A formal proof  *)

(** Proving that the result of decompiling any compiled expression is
    the original expression is closely related to the proposition
    [vm_correct] made for the virtual machine. Again, the proof follows
    from the more general proposition [stack_machine_correct]. *)
(* 
Lemma decompile_correct : forall e,
    decompile (compile e) = Some e.
Proof.
  intros e.
  unfold decompile.
  rewrite stack_machine_correct.
  reflexivity.
Qed. *)


(** ** Right-inverse  *)

(** The decompiler should be the right-inverse of the compiler,
    i.e. compiling the result of any decompiled [program] should yield
    an identical [program]. *)

(** *** QuickChick intermezzo  *)

(** Again, we would like to check for counter-examples before proving
    the proposition formally. In order to use QuickChick for this, we
    need to do a bit of plumbing. *)

(** As with the [exp] type, we need to derive the [Show] typeclass for
    [instruction] and derive decidability of equality between two
    elements of this type. *)
(* 
Derive Show for instruction. *)

(* Instance eq_dec_insns (insns1 insns2 : list instruction) : Dec (insns1 = insns2) := {}.
Proof. dec_eq. Defined.

(** We implement an instruction generator that generates programs
    guaranteed to not fail with a stack underflow. We do this by
    conditioning the generation of instructions based on the current
    size of the stack, e.g. if the stack size is lower than 2,
    generating a binary operation would cause a stack underflow; thus,
    we do not allow generation that instruction in this specific case.
*)

Fixpoint genSizedinsns_helper (size : nat) (s : nat) (insns : list instruction) :=
  match size with
    | O => ret insns
    | S size' =>
      freq [ (if (s = 1)? then 1 else 0, ret insns) ;
             (size,
               (n <- choose (1, 40) ;;
                (genSizedinsns_helper size' (s + 1) (IPush n :: insns)))) ;
             (if (s >= 1)? then 1 else 0,
               (op <- (elems [ OSucc ; OPred ]) ;;
               (genSizedinsns_helper size' s (IUnOp op :: insns)))) ;
             (if (s >= 2)? then 1 else 0,
               (op <- (elems [ OPlus ; OMult ; OMinus ]) ;;
               (genSizedinsns_helper size' (s - 1) (IBinOp op :: insns)))) ;
             (if (s >= 3)? then 1 else 0,
               (genSizedinsns_helper size' (s - 2) (IChoose :: insns)))
           ]
  end.

Fixpoint genSizedinsns (size : nat) : G (list instruction) :=
  rev_insns <- (genSizedinsns_helper size 0 []) ;;
  ret (rev rev_insns).

(** Not all programs generated by the generator can be decompiled
    successfully by [decompile]---some programs would decompile to more
    than one expression, which we disallow in the [decompile]
    function. To accomodate this, we state a predicate for successful
    decompilation using the [decompile] function.

    (Note: The predicate is a QuickChick Checker, since it returns a
    boolean as opposed to a Prop.)

 *)*)

Definition decompiles (insns : list instruction) :=
  match decompile insns with
    | Some _ => true
    | None => false
  end.

(** We are now able to state the proposition we want to check: the
    successful decompilation of a program implies that compiling the result
    of this is equal to the original program. *)

(* Definition compile_decompile_test (ins : list instruction) :=
  (decompiles ins) ==> match (decompile ins) with
                         | Some e => (compile e = ins)?
                         | None => false
                       end.
 *)
(** We check the proposition *)

(* QuickChick (forAll (genSizedinsns 5) compile_decompile_test). *)

(** and get

    +++ Passed 10000 tests (10547 discards)
    
    The proposition seems to hold. The discards come from generated
    programs that were discarded because they did not satisfy the
    [decompiles] predicate. 
*)


(** *** An attempt at a formal proof *)

(** Compared to the proof of the left-inverse property, the formal proof
    of the right-inverse property is more difficult.

    (Note: We do not succeed in proving the proposition stated. We have
    kept the proof attempts in the document to illustrate our work and
    the thoughts made during this.)

    We try to prove the proposition by induction on [e]: *)
(* 
Theorem compile_decompile : forall insns e,
    decompile insns = Some e -> compile e = insns.
Proof.
  intros. generalize dependent insns. induction e; intros insns H.
  - simpl. destruct insns.
    + inversion H.
    + destruct i.
      * destruct insns.
      * inversion H. reflexivity.
        -- destruct i.
           ++ destruct insns.
              ** inversion H.
              ** (* ? *)

(** At this point in the proof, we are stuck. We can keep on alternating
    between destructing [insns] and [i]. In the cases where [insns] is
    [nil], we can use inversion to prove the subgoals:

        H : decompile [IPush n0; IPush n1] = Some (ENat n)

    is obviously not a valid hypothesis. However, in the case where
    [insns] contains an instruction [i], we need to destruct this.  This
    cycle can keep on ad infinitum.

    The main issue with this approach for the proof is that the
    [decompile] function can not yield an answer until it has walked
    fully through the list, e.g. a single [IPush] instruction in the
    head of the list does not indicate that the decompiled expression
    should be a [ENat]; it might as well be that a [IUnOp] instruction
    follows, and the correct decompilation should be a [EUnOp] with an
    [ENat] as sub-expression.

    The logical way of getting around this problem would be by induction
    over the list of instructions. We abort the proof, and try this
    approach instead.  *)
Abort. *)

Theorem compile_decompile : forall insns e,
    decompile insns = Some e -> compile e = insns.
Proof.
  intro insns. induction insns; intros.
  - inversion H.
  - unfold decompile in H. destruct a. simpl in H.


(** It seems that we are stuck yet again. Being able to decompile a
    single instruction could help us progress in the proof. The
    [decompile] function is implemented using [stack_machine], which
    operates in single steps on the list of instructions. This is,
    however, not what we need. Decompiling a single instruction is not
    enough to uniquely determine the expression being decompiled.

    Let us abort this proof for now. *)
Abort.

(* ================================================================= *)
(** *** Inductive relations  *)

(** In an attempt to attack the proof of the right-inverse property of
    the decompiler from another angle, we define the compiler as an
    inductive relation between expressions and programs. *)

Inductive compileR : exp -> program -> Prop :=
  | C_ENat : forall (n : nat),
      compileR (ENat n) [IPush n]

  | C_EUnOp : forall (op : unop) (e : exp) (insns : program),
      compileR e insns
      -> compileR (EUnOp op e) (insns ++ [IUnOp op])

  | C_EBinOp : forall (op : binop) (e1 e2 : exp) (insns1 insns2 : program),
      compileR e1 insns1
      -> compileR e2 insns2
      -> compileR (EBinOp op e1 e2) (insns1 ++ insns2 ++ [IBinOp op])

  | C_EIf : forall (cond e1 e2 : exp) (cond_insns insns1 insns2 : program),
      compileR e1 insns1
      -> compileR e2 insns2
      -> compileR cond cond_insns
      -> compileR (EIf cond e1 e2) (insns2 ++ insns1 ++ cond_insns ++ [ IChoose ]).

(** A pair of an expression and a program should satisfy this relation
    iff the applying [compile] to the expression yields the program. *)

Lemma compile_iff_compileR : forall e insns,
    compileR e insns <-> compile e = insns.
Proof.
  intros e insns.
  split; intro H.
  - induction H; subst; reflexivity.
  - generalize dependent insns.
    induction e; intros insns H; subst; constructor; auto.
Qed.

(** Note that the inductive relation also describes decompilation of
    programs. *)

Definition decompileR (insns : program) (e : exp) : Prop :=
  compileR e insns.

(** This definition leads to [compileR] being equivalent to [decompileR]
    (with swapped arguments). *)

Lemma compileR_iff_decompileR : forall ins e,
    compileR e ins <-> decompileR ins e.
Proof.
  unfold decompileR.
  intros. reflexivity.
Qed.

(** We now want to prove that decompilation of a program [insns]
    yielding some expression [e] implies that [insns] and [e] satisfy
    the [decompileR] relation. Having this lemma at our hands would
    allow us to prove the [compile_decompile] lemma that we struggled
    with earlier.  *)

Lemma decompile_implies_decompileR : forall e insns,
   decompile insns = Some e -> decompileR insns e.
Proof.
  intros e insns. generalize dependent e. induction insns; intros.
  - inversion H.
  - destruct a.
    (** We are back in the same situation as before. Stuck, yet again. *)
Abort.

(** We abort the quest of proving the right-inverse property. It seems
    to be possible, but we need to know more of the properties of the
    [decompile] function in order to progress in these proofs.  *)

(* ================================================================= *)
(** * Eval not unique  *)

(** In this section, we show that for all expressions, there exists a
    different expression which evaluates to the same value. This makes
    it impossible to prove a right-hand version of [vm_correct], as
    opposed to [compile_decompile], which is the right-hand version of
    [decompile_correct].

    To prove this, we need to state a lemma to help us: *)

Lemma binop_cannot_contain_self : forall e e' op, e <> EBinOp op e' e.
  unfold not.
  induction e; intros; inversion H.
  - subst. apply IHe2 in H3. apply H3.
Qed.

(** We now formally state and prove the [eval_not_unique]
    proposition. *)

Lemma eval_not_unique : forall e1, exists e2, eval e1 = eval e2 /\ ~(e1 = e2).
Proof.
  intros e1.
  unfold not.
  destruct e1 eqn:He1; simpl; exists (EBinOp OPlus (ENat 0) e1);
    subst; split; simpl; (reflexivity || intros H; inversion H).
  - subst. apply binop_cannot_contain_self in H3. apply H3.
Qed.

End Main.

(* ================================================================= *)
(** * Adding a no-op instruction *)

(** In this last section, we show that adding a no-op instruction to the
    instruction set invalidates the right-inverse property of
    [decompile] for [compile]. We do this by reimplementing the compiler
    and decompiler and checking the right-inverse proposition with
    QuickChick, which should find a fairly trivial counter-example.

    Illustrating this requires duplicating a fair amount of code. We
    have added comments to show when new code is added.

*)

Module Nop.

  Inductive unop :=
    | OSucc : unop
    | OPred : unop.
  
  Inductive binop :=
    | OPlus : binop
    | OMult : binop
    | OMinus : binop.

  Inductive exp :=
    | ENat   : nat   -> exp
    | EUnOp  : unop  -> exp -> exp
    | EBinOp : binop -> exp -> exp -> exp
    | EIf    : exp   -> exp -> exp -> exp.

  Inductive instruction :=
  | IPush   : nat   -> instruction
  | IUnOp   : unop  -> instruction
  | IBinOp  : binop -> instruction
  | IChoose : instruction

  (** We add the IDont instruction. *)
  | IDont : instruction.

  Definition program := list instruction.
(*   Derive Show for unop.
  Derive Show for binop.
  Derive Show for exp.
 *)
  (* The compiler remains unchanged from the main program. There is no
     case where the compiler outputs an [IDont] instruction. This is an
     important point to note. *)
  Fixpoint compile (e : exp) : program :=
    match e with
    | ENat   n          => [ IPush n ]
    | EUnOp  op e       => compile e ++ [ IUnOp op ]
    | EBinOp op e1 e2   => compile e1 ++ compile e2 ++ [ IBinOp op ]
    | EIf    cond e1 e2 => compile e2 ++ compile e1 ++ compile cond ++ [ IChoose ]
    end.

  Definition exp_to_elem_props
             {A : Type}
             (exp_to_elem : exp -> A)
             exp_to_elem_push
             exp_to_elem_unop
             exp_to_elem_binop
             exp_to_elem_if : Prop :=

    (forall (n : nat),
        exp_to_elem (ENat n) = exp_to_elem_push n) /\

    (forall (e : exp) (op : unop),
        exp_to_elem (EUnOp op e) = exp_to_elem_unop op (exp_to_elem e)) /\

    (forall (e1 e2 : exp) (op : binop),
        exp_to_elem (EBinOp op e1 e2) = exp_to_elem_binop op (exp_to_elem e1) (exp_to_elem e2)) /\

    (forall (cond e1 e2 : exp),
        exp_to_elem (EIf cond e1 e2) =
        exp_to_elem_if (exp_to_elem cond) (exp_to_elem e2) (exp_to_elem e1)).


  Record stack_machine_functions {A : Type} :=
    {
      constr_push  : nat -> A;
      constr_unop  : unop -> A -> A;
      constr_binop : binop -> A -> A -> A;
      constr_if    : A -> A -> A -> A;
      exp_to_elem  : exp -> A;
      props        : @exp_to_elem_props A exp_to_elem constr_push constr_unop constr_binop constr_if
    }.


  Definition stack_machine_state_transition
             {A: Type}
             (funcs: stack_machine_functions)
             (insn: instruction)
             (s: list A) : option (list A) :=

    match insn, s with
    | IPush n, s'
      => Some ((constr_push funcs n) :: s')

    | IUnOp op, x :: s'
      => Some ((constr_unop funcs op x) :: s')

    | IBinOp op, y :: x :: s'
      => Some ((constr_binop funcs op x y) :: s')

    | IChoose, cond :: v2 :: v1 :: s'
      => Some ((constr_if funcs cond v1 v2) :: s')

    (** We add a new case to handle the [IDont] instruction by making no
       changes to the stack.  *)
    | IDont, s' => Some s'

    | _, _ => None
    end.

  Fixpoint stack_machine {A : Type} (funcs : stack_machine_functions) (insns : program) (s : list A) : option (list A) :=
    match insns with
    | insn :: insns' => match stack_machine_state_transition funcs insn s with
                        | Some s' => stack_machine funcs insns' s'
                        | _       => None
                        end
    | [] => Some s
    end.

  Lemma decompile_functions_props : exp_to_elem_props id ENat EUnOp EBinOp (fun (cond e1 e2 : exp) => EIf cond e2 e1).
  Proof.
    unfold exp_to_elem_props. auto.
  Qed.

  Definition decompile_functions :=
    {|
      constr_push  := ENat;
      constr_unop  := EUnOp;
      constr_binop := EBinOp;
      constr_if    := fun (cond e1 e2 : exp) => EIf cond e2 e1;
      exp_to_elem  := id;
      props        := decompile_functions_props;
    |}.


  Definition decompile (insns : program) : option exp :=
    match stack_machine decompile_functions insns [] with
    | Some [res] => Some res
    | _ => None
    end.


(*   Derive Show for instruction. *)
(* 
  Fixpoint genSizedinsns_helper (size : nat) (s : nat) (insns : list instruction) :=
    match size with
    | O => ret insns
    | S size' =>
      freq [ (if (s = 1)? then 1 else 0, ret insns) ;
               (size,
                (n <- choose (1, 40) ;;
                   (genSizedinsns_helper size' (s + 1) (IPush n :: insns)))) ;
               (if (s >= 1)? then 1 else 0,
                (op <- (elems [ OSucc ; OPred ]) ;;
                    (genSizedinsns_helper size' s (IUnOp op :: insns)))) ;
               (if (s >= 2)? then 1 else 0,
                (op <- (elems [ OPlus ; OMult ; OMinus ]) ;;
                    (genSizedinsns_helper size' (s - 1) (IBinOp op :: insns)))) ;
               (if (s >= 3)? then 1 else 0,
                (genSizedinsns_helper size' (s - 2) (IChoose :: insns))) ;
               (** We add a case to generate the [IDont] instruction *)
               (size, genSizedinsns_helper size' s (IDont :: insns))
           ]
    end.

  Fixpoint genSizedinsns (size : nat) : G (list instruction) :=
    rev_insns <- (genSizedinsns_helper size 0 []) ;;
              ret (rev rev_insns).
 *)
  Definition decompiles (insns : list instruction) :=
    match decompile insns with
    | Some _ => true
    | None => false
    end.

(*   Instance eq_dec_insns (insns1 insns2 : list instruction) : Dec (insns1 = insns2) := {}.
  Proof. dec_eq. Defined.
 *)
  (** We state the right-inverse proposition again for use with
      QuickChick.  *)

(*   Definition compile_decompile_test (ins : list instruction) :=
    (decompiles ins) ==> match (decompile ins) with
                         | Some e => (compile e = ins)?
                         | None => false
                         end.
 *)
  (** Checking should result in an error as soon as a program contains
      the [IDont] instruction.  *)

(*   QuickChick (forAll (genSizedinsns 5) compile_decompile_test). *)

  (** QuickChick returns the expected results:
   
   > [IDont; IDont; IPush 20; IDont; IDont]
   
   > Failed after 1 tests and 0 shrinks. (0 discards)

   The right-inverse property does not hold.
  *)

  (** The reasoning behind the right-inverse property not holding for
      this language (with the added no-op) is that that there is no way
      of producing an [IDont] instruction from the compiler. Decompiling
      an expression simply ignores [IDont] instructions, causing there
      to be multiple programs decompiling to the same
      expression. Decompiling a program containing [IDont] instructions
      and compiling it imediately afterwards should yield the same
      program with the [IDont] instructions removed.

*)

End Nop.




From Coq Require Import Extraction.
From Coq Require Import List.
Declare ML Module "coq_spec".
From Hammer Require Import Hammer.
From Hammer Require Import Reconstr.
Add LoadPath "~/Documents/cpdt/src" as Cpdt.
Require Import Cpdt.CpdtTactics.

Extract Inductive bool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inductive option => "Prelude.Maybe" [ "Prelude.Just" "Prelude.Nothing" ].
Extract Inductive unit => "()" [ "()" ].
Extract Inductive list => "([])" [ "([])" "(:)" ].
Extract Inductive prod => "(,)" [ "(,)" ].

Extract Inductive sumbool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inductive sumor => "Prelude.Maybe" [ "Prelude.Just" "Prelude.Nothing" ].
Extract Inductive sum => "Prelude.Either" [ "Prelude.Left" "Prelude.Right" ].

Extract Inlined Constant andb => "(Prelude.&&)".
Extract Inlined Constant orb => "(Prelude.||)".
Extract Inlined Constant negb => "(Prelude.not)".
Extract Inlined Constant app => "(Prelude.++)".

(* DiscoverLemmas "DecompilerTest" Main.stack_machine_functions Main.eval  Main.compile . *)

(** END OF DOCUMENT. *)

(* Local Variables: *)
(* fill-column: 72 *)
(* End: *)
