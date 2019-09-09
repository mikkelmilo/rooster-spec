(declare-datatype
  list :source |[]|
  (par (a)
    ((nil :source |[]|) (cons :source |:| (head a) (tail (list a))))))
(declare-datatype
  Unop :source Unop ((OSucc :source OSucc) (OPred :source OPred)))
(declare-datatype
  Nat :source Nat ((O :source O) (S :source S (proj1-S Nat))))
(declare-datatype
  Binop :source Binop
  ((OPlus :source OPlus) (OMult :source OMult)
   (OMinus :source OMinus)))
(declare-datatype
  Exp :source Exp
  ((ENat :source ENat (proj1-ENat Nat))
   (EUnOp :source EUnOp (proj1-EUnOp Unop) (proj2-EUnOp Exp))
   (EBinOp :source EBinOp (proj1-EBinOp Binop)
     (proj2-EBinOp Exp) (proj3-EBinOp Exp))
   (EIf :source EIf (proj1-EIf Exp) (proj2-EIf Exp) (proj3-EIf Exp))))
(declare-datatype
  Instruction :source Instruction
  ((IPush :source IPush (proj1-IPush Nat))
   (IUnOp :source IUnOp (proj1-IUnOp Unop))
   (IBinOp :source IBinOp (proj1-IBinOp Binop))
   (IChoose :source IChoose)))
(define-fun-rec
  sub :keep :source sub
  ((x Nat) (y Nat)) Nat
  (match x
    ((O O)
     ((S k)
      (match y
        ((O (S k))
         ((S l) (sub k l))))))))
(define-fun
  pred :keep :source pred
  ((x Nat)) Nat
  (match x
    ((O O)
     ((S u) u))))
(define-fun
  eval_unop :keep :source eval_unop
  ((x Unop) (y Nat)) Nat
  (match x
    ((OSucc (S y))
     (OPred (pred y)))))
(define-fun-rec
  add :keep :source add
  ((x Nat) (y Nat)) Nat
  (match x
    ((O y)
     ((S p) (S (add p y))))))
(define-fun-rec
  mul :keep :source mul
  ((x Nat) (y Nat)) Nat
  (match x
    ((O O)
     ((S p) (add y (mul p y))))))
(define-fun
  eval_binop :keep :source eval_binop
  ((x Binop) (y Nat) (z Nat)) Nat
  (match x
    ((OPlus (add y z))
     (OMult (mul y z))
     (OMinus (sub y z)))))
(define-fun-rec
  eval :keep :source eval
  ((x Exp)) Nat
  (match x
    (((ENat n) n)
     ((EUnOp op e0) (eval_unop op (eval e0)))
     ((EBinOp op2 e1 e2) (eval_binop op2 (eval e1) (eval e2)))
     ((EIf cond e12 e22)
      (match (eval cond)
        ((O (eval e22))
         ((S y) (eval e12))))))))
(define-fun-rec
  ++ :source ++
  (par (a) (((x (list a)) (y (list a))) (list a)))
  (match x
    ((nil y)
     ((cons z xs) (cons z (++ xs y))))))
(define-fun-rec
  compile :keep :source compile
  ((x Exp)) (list Instruction)
  (match x
    (((ENat n) (cons (IPush n) (_ nil Instruction)))
     ((EUnOp op e0)
      (++ (compile e0) (cons (IUnOp op) (_ nil Instruction))))
     ((EBinOp op2 e1 e2)
      (++ (compile e1)
        (++ (compile e2) (cons (IBinOp op2) (_ nil Instruction)))))
     ((EIf cond e12 e22)
      (++ (compile e22)
        (++ (compile e12)
          (++ (compile cond) (cons IChoose (_ nil Instruction)))))))))
(prove
  :lemma
  (forall ((x Nat)) (= (eval (ENat x)) x)))
(prove
  :lemma
  (forall ((x Nat))
    (= (compile (ENat x)) (cons (IPush x) (_ nil Instruction)))))
(prove
  :lemma
  (forall ((x Exp)) (= (eval (EUnOp OSucc x)) (S (eval x)))))
(prove
  :lemma
  (forall ((x Exp) (y Exp)) (= (eval (EIf x y y)) (eval y))))
(prove
  :lemma
  (forall ((x Exp)) (= (eval (EBinOp OMinus x x)) O)))
(prove
  :lemma
  (forall ((x Exp) (y Exp))
    (= (eval (EBinOp OMult y x)) (eval (EBinOp OMult x y)))))
(prove
  :lemma
  (forall ((x Exp) (y Exp))
    (= (eval (EBinOp OPlus y x)) (eval (EBinOp OPlus x y)))))
(prove
  :lemma
  (= (eval (EUnOp OPred (ENat O))) O))
