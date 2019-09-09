module DecompilerTest where

import qualified Prelude

data Nat =
   O
 | S Nat

pred :: Nat -> Nat
pred n =
  case n of {
   O -> n;
   S u -> u}

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

mul :: Nat -> Nat -> Nat
mul n m =
  case n of {
   O -> O;
   S p -> add m (mul p m)}

sub :: Nat -> Nat -> Nat
sub n m =
  case n of {
   O -> n;
   S k -> case m of {
           O -> n;
           S l -> sub k l}}

data Unop =
   OSucc
 | OPred

data Binop =
   OPlus
 | OMult
 | OMinus

data Exp =
   ENat Nat
 | EUnOp Unop Exp
 | EBinOp Binop Exp Exp
 | EIf Exp Exp Exp

data Instruction =
   IPush Nat
 | IUnOp Unop
 | IBinOp Binop
 | IChoose

type Program = ([]) Instruction

eval_unop :: Unop -> Nat -> Nat
eval_unop op v =
  case op of {
   OSucc -> S v;
   OPred -> pred v}

eval_binop :: Binop -> Nat -> Nat -> Nat
eval_binop op v1 v2 =
  case op of {
   OPlus -> add v1 v2;
   OMult -> mul v1 v2;
   OMinus -> sub v1 v2}

eval :: Exp -> Nat
eval e =
  case e of {
   ENat n -> n;
   EUnOp op e0 -> eval_unop op (eval e0);
   EBinOp op e1 e2 -> eval_binop op (eval e1) (eval e2);
   EIf cond e1 e2 -> case eval cond of {
                      O -> eval e2;
                      S _ -> eval e1}}

data Stack_machine_functions a =
   Build_stack_machine_functions (Nat -> a) (Unop -> a -> a) (Binop -> a -> a
                                                             -> a) (a -> a ->
                                                                   a -> a) 
 (Exp -> a)

compile :: Exp -> Program
compile e =
  case e of {
   ENat n -> (:) (IPush n) ([]);
   EUnOp op e0 -> (Prelude.++) (compile e0) ((:) (IUnOp op) ([]));
   EBinOp op e1 e2 ->
    (Prelude.++) (compile e1)
      ((Prelude.++) (compile e2) ((:) (IBinOp op) ([])));
   EIf cond e1 e2 ->
    (Prelude.++) (compile e2)
      ((Prelude.++) (compile e1)
        ((Prelude.++) (compile cond) ((:) IChoose ([]))))}

