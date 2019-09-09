(declare-datatype
  pair :tuple :source |(,)|
  (par (a b)
    ((pair2 :tuple :source |(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatype
  list :source |[]|
  (par (a)
    ((nil :source |[]|) (cons :source |:| (head a) (tail (list a))))))
(declare-datatype
  Nat :source Nat ((O :source O) (S :source S (proj1-S Nat))))
(declare-datatype
  Maybe :source Maybe
  (par (a)
    ((Nothing :source Nothing) (Just :source Just (proj1-Just a)))))
(declare-datatype
  Form :source Form
  ((Var :source Var (proj1-Var Nat)) (Ftrue :source Ftrue)
   (Ffalse :source Ffalse)
   (And :source And (proj1-And Form) (proj2-And Form))
   (Or :source Or (proj1-Or Form) (proj2-Or Form))
   (Imp :source Imp (proj1-Imp Form) (proj2-Imp Form))
   (Neg :source Neg (proj1-Neg Form))))
(define-fun-rec
  nnf_aux :keep :source nnf_aux
  ((x Form) (y Nat)) Form
  (match y
    ((O x)
     ((S n)
      (match x
        ((_ x)
         ((And f1 f2) (And (nnf_aux f1 n) (nnf_aux f2 n)))
         ((Or f12 f22) (Or (nnf_aux f12 n) (nnf_aux f22 n)))
         ((Imp f13 f23) (Imp (nnf_aux f13 n) (nnf_aux f23 n)))
         ((Neg f0)
          (match f0
            (((Var z) (Neg (Var z)))
             (Ftrue Ffalse)
             (Ffalse Ftrue)
             ((And f14 f24) (Or (nnf_aux (Neg f14) n) (nnf_aux (Neg f24) n)))
             ((Or f15 f25) (And (nnf_aux (Neg f15) n) (nnf_aux (Neg f25) n)))
             ((Imp f16 f26) (And (nnf_aux f16 n) (nnf_aux (Neg f26) n)))
             ((Neg f) (nnf_aux f n)))))))))))
(define-fun-rec
  next_assignment :keep :source next_assignment
  ((x (list (pair Nat Bool)))) (list (pair Nat Bool))
  (match x
    ((nil (_ nil (pair Nat Bool)))
     ((cons p xs)
      (match p
        (((pair2 id c2)
          (ite
            c2 (cons (pair2 id false) (next_assignment xs))
            (cons (pair2 id true) xs)))))))))
(define-fun
  negb :keep :source negb
  ((x Bool)) Bool (not x))
(define-fun-rec
  map :keep :source map
  (par (a1 a2) (((x (=> a1 a2)) (y (list a1))) (list a2)))
  (match y
    ((nil (_ nil a2))
     ((cons b t) (cons (@ x b) (map x t))))))
(define-fun-rec
  length :keep :source length
  (par (a1) (((x (list a1))) Nat))
  (match x
    ((nil O)
     ((cons y |l'|) (S (length |l'|))))))
(define-fun-rec
  interp :keep :source interp
  ((x (=> Nat Bool)) (y Form)) Bool
  (match y
    (((Var id) (@ x id))
     (Ftrue true)
     (Ffalse false)
     ((And f1 f2) (and (interp x f1) (interp x f2)))
     ((Or f12 f22) (or (interp x f12) (interp x f22)))
     ((Imp f13 f23) (ite (interp x f13) (interp x f23) true))
     ((Neg f) (not (interp x f))))))
(define-fun-rec
  find :keep :source find
  (par (a1) (((x (=> a1 Bool)) (y (list a1))) (Maybe a1)))
  (match y
    ((nil (_ Nothing a1))
     ((cons z tl) (ite (@ x z) (Just z) (find x tl))))))
(define-fun-rec
  eqb :keep :source eqb
  ((x Nat) (y Nat)) Bool
  (match x
    ((O
      (match y
        ((O true)
         ((S z) false))))
     ((S |n'|)
      (match y
        ((O false)
         ((S |m'|) (eqb |n'| |m'|))))))))
(define-fun-rec
  contains_id :keep :source contains_id
  ((x Nat) (y (list Nat))) Bool
  (match y
    ((nil false)
     ((cons z xs) (ite (eqb z x) true (contains_id x xs))))))
(define-fun-rec
  merge_id_lists :keep :source merge_id_lists
  ((x (list Nat)) (y (list Nat))) (list Nat)
  (match x
    ((nil y)
     ((cons z xs)
      (ite
        (contains_id z y) (merge_id_lists xs y)
        (merge_id_lists xs (cons z y)))))))
(define-fun-rec
  form_ids_aux :keep :source form_ids_aux
  ((x Form) (y (list Nat))) (list Nat)
  (match x
    ((_ y)
     ((Var id) (ite (contains_id id y) y (cons id y)))
     ((And f1 f2)
      (merge_id_lists (form_ids_aux f1 y) (form_ids_aux f2 y)))
     ((Or f12 f22)
      (merge_id_lists (form_ids_aux f12 y) (form_ids_aux f22 y)))
     ((Imp f13 f23)
      (merge_id_lists (form_ids_aux f13 y) (form_ids_aux f23 y)))
     ((Neg f) (form_ids_aux f y)))))
(define-fun
  form_ids :keep :source form_ids
  ((x Form)) (list Nat) (form_ids_aux x (_ nil Nat)))
(define-fun
  empty_assignment :keep :source empty_assignment
  ((x Form)) (list (pair Nat Bool))
  (map (lambda ((id Nat)) (pair2 id false)) (form_ids x)))
(define-fun
  beq_id :keep :source beq_id
  ((x Nat) (y Nat)) Bool (eqb x y))
(define-fun
  assignment_to_valuation :keep :source assignment_to_valuation
  ((x (list (pair Nat Bool))) (y Nat)) Bool
  (match
    (find
      (lambda ((z (pair Nat Bool)))
        (match z (((pair2 id x2) (eqb y id)))))
      x)
    ((Nothing false)
     ((Just p) (match p (((pair2 x3 v) v)))))))
(define-fun-rec
  find_valuation_aux :keep :source find_valuation_aux
  ((x Form) (y (list (pair Nat Bool))) (z Nat)) (Maybe (=> Nat Bool))
  (let ((|v'| (lambda ((x2 Nat)) (assignment_to_valuation y x2))))
    (match z
      ((O (ite (interp |v'| x) (Just |v'|) (_ Nothing (=> Nat Bool))))
       ((S n)
        (ite
          (interp |v'| x) (Just |v'|)
          (find_valuation_aux x (next_assignment y) n)))))))
(define-fun-rec
  add0 :keep :source add0
  ((x Nat) (y Nat)) Nat
  (match x
    ((O y)
     ((S p) (S (add0 p y))))))
(define-fun-rec
  mul0 :keep :source mul0
  ((x Nat) (y Nat)) Nat
  (match x
    ((O O)
     ((S p) (add0 y (mul0 p y))))))
(define-fun-rec
  pow :keep :source pow
  ((x Nat) (y Nat)) Nat
  (match y
    ((O (S O))
     ((S m0) (mul0 x (pow x m0))))))
(define-fun
  find_valuation :keep :source find_valuation
  ((x Form)) (Maybe (=> Nat Bool))
  (find_valuation_aux x
    (empty_assignment x) (pow (S (S O)) (length (form_ids x)))))
(define-fun
  solver :keep :source solver
  ((x Form)) Bool
  (match (find_valuation x)
    ((Nothing false)
     ((Just y) true))))
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
(define-fun-rec
  size_of_form :keep :source size_of_form
  ((x Form)) Nat
  (match x
    ((_ (S O))
     ((And f1 f2) (add (size_of_form f1) (size_of_form f2)))
     ((Or f12 f22) (add (size_of_form f12) (size_of_form f22)))
     ((Imp f13 f23) (add (size_of_form f13) (size_of_form f23)))
     ((Neg f) (S (size_of_form f))))))
(define-fun
  nnf :keep :source nnf
  ((x Form)) Form (nnf_aux x (mul (S (S O)) (size_of_form x))))
(define-fun
  nnf_solver :keep :source nnf_solver
  ((x Form)) Bool (solver (nnf x)))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (=> (interp x y) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form)) (=> (solver x) (nnf_solver x))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf_solver x) (solver x))))
(prove
  :lemma
  (= (nnf Ffalse) Ffalse))
(prove
  :lemma
  (= (nnf Ftrue) Ftrue))
(prove
  :lemma
  (not (nnf_solver Ffalse)))
(prove
  :lemma
  (nnf_solver Ftrue))
(prove
  :lemma
  (forall ((x (=> Nat Bool))) (not (interp x Ffalse))))
(prove
  :lemma
  (forall ((x (=> Nat Bool))) (interp x Ftrue)))
(prove
  :lemma
  (forall ((x Nat)) (= (nnf (Var x)) (Var x))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (nnf x)) (nnf x))))
(prove
  :lemma
  (forall ((x Nat)) (nnf_solver (Var x))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf_solver (nnf x)) (nnf_solver x))))
(prove
  :lemma
  (= (nnf (Neg Ffalse)) Ftrue))
(prove
  :lemma
  (= (nnf (Neg Ftrue)) Ffalse))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (nnf y)) (interp x y))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And y x)) (nnf_solver (And x y)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf_solver (And x x)) (nnf_solver x))))
(prove
  :lemma
  (forall ((x Form)) (nnf_solver (Imp x x))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (=> (interp x z) (nnf_solver (Imp y z)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (=> (nnf_solver y) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Or y x)) (nnf_solver (Or x y)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf_solver (Or x x)) (nnf_solver x))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (=> (interp x z) (nnf_solver (Or y z)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (=> (nnf_solver y) (nnf_solver (Or x y)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (And x Ffalse)) (And (nnf x) Ffalse))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (And x Ftrue)) (And (nnf x) Ftrue))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (And Ffalse x)) (And Ffalse (nnf x)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (And Ftrue x)) (And Ftrue (nnf x)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Imp x Ffalse)) (Imp (nnf x) Ffalse))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Imp x Ftrue)) (Imp (nnf x) Ftrue))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Imp Ffalse x)) (Imp Ffalse (nnf x)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Imp Ftrue x)) (Imp Ftrue (nnf x)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Neg (Neg x))) (nnf x))))
(prove
  :lemma
  (forall ((x Nat)) (= (nnf (Neg (Var x))) (Neg (Var x)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Or x Ffalse)) (Or (nnf x) Ffalse))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Or x Ftrue)) (Or (nnf x) Ftrue))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Or Ffalse x)) (Or Ffalse (nnf x)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf (Or Ftrue x)) (Or Ftrue (nnf x)))))
(prove
  :lemma
  (forall ((x Form)) (not (nnf_solver (And x Ffalse)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf_solver (And x Ftrue)) (nnf_solver x))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf_solver (Imp x Ffalse)) (nnf_solver (Neg x)))))
(prove
  :lemma
  (forall ((x Form)) (nnf_solver (Imp Ffalse x))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf_solver (Imp Ftrue x)) (nnf_solver x))))
(prove
  :lemma
  (forall ((x Nat)) (nnf_solver (Neg (Var x)))))
(prove
  :lemma
  (forall ((x Form)) (= (nnf_solver (Or x Ffalse)) (nnf_solver x))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (=> (interp x y) (not (interp x (Neg y))))))
(prove
  :lemma
  (forall ((x Form) (y (=> Nat Bool)) (z Nat))
    (=> (interp y x) (= (interp y (Var z)) (@ y z)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (= (interp x (And z y)) (interp x (And y z)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (And y y)) (interp x y))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form)) (interp x (Imp y y))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (= (interp x (Or z y)) (interp x (Or y z)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (Or y y)) (interp x y))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (And (nnf x) (nnf y)) (nnf (And x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (Imp (nnf x) (nnf y)) (nnf (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (Or (nnf x) (nnf y)) (nnf (Or x y)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (not (interp x (And y Ffalse)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (And y Ftrue)) (interp x y))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (Imp y Ffalse)) (interp x (Neg y)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form)) (interp x (Imp y Ftrue))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form)) (interp x (Imp Ffalse y))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (Imp Ftrue y)) (interp x y))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (Or y Ffalse)) (interp x y))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form)) (interp x (Or y Ftrue))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf (Neg (Imp x y))) (nnf (And x (Neg y))))))
(prove
  :lemma
  (forall ((x Form)) (not (nnf_solver (And x (Neg x))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp y (Neg x))) (nnf_solver (Imp x (Neg y))))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf_solver (Imp x (Neg x))) (nnf_solver (Neg x)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Neg x) y)) (nnf_solver (Or x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Neg (And x y))) (nnf_solver (Imp x (Neg y))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Neg (Or y x))) (nnf_solver (Neg (Or x y))))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf_solver (Neg (Or x x))) (nnf_solver (Neg x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (And x Ffalse))) (Or (nnf (Neg x)) Ftrue))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (And x Ftrue))) (Or (nnf (Neg x)) Ffalse))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (And Ffalse x))) (Or Ftrue (nnf (Neg x))))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (And Ftrue x))) (Or Ffalse (nnf (Neg x))))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (Or x Ffalse))) (And (nnf (Neg x)) Ftrue))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (Or x Ftrue))) (And (nnf (Neg x)) Ffalse))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (Or Ffalse x))) (And Ftrue (nnf (Neg x))))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Neg (Or Ftrue x))) (And Ffalse (nnf (Neg x))))))
(prove
  :lemma
  (forall ((x Form) (y (=> Nat Bool)) (z Form))
    (=> (interp y x) (= (interp y (And z x)) (interp y z)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (=> (interp x z) (interp x (Imp y z)))))
(prove
  :lemma
  (forall ((x Form) (y (=> Nat Bool)) (z Form))
    (=> (interp y x) (= (interp y (Imp x z)) (interp y z)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (=> (interp x z) (interp x (Or y z)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (= (interp x (Imp z (Neg y))) (interp x (Imp y (Neg z))))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (Imp y (Neg y))) (interp x (Neg y)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (= (interp x (Imp (Neg y) z)) (interp x (Or y z)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (= (interp x (Neg (And y z))) (interp x (Imp y (Neg z))))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (= (interp x (Neg (Or z y))) (interp x (Neg (Or y z))))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form))
    (= (interp x (Neg (Or y y))) (interp x (Neg y)))))
(prove
  :lemma
  (forall ((x (=> Nat Bool)) (y Form) (z Form))
    (= (interp x (Or z (Neg y))) (interp x (Imp y z)))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (And x (And z y))) (nnf_solver (And x (And y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (And y (And x z))) (nnf_solver (And x (And y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And x (And x y))) (nnf_solver (And x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And x (Imp x y))) (nnf_solver (And x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And y (Imp x y))) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And y (Imp x x))) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (And x (Or z y))) (nnf_solver (And x (Or y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And y (Or y x))) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And x (Or y y))) (nnf_solver (And x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (Imp x (And z y))) (nnf_solver (Imp x (And y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp x (And x y))) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp x (And y y))) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (Imp y (Imp x z))) (nnf_solver (Imp x (Imp y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp x (Imp x y))) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (Imp x (Or z y))) (nnf_solver (Imp x (Or y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form)) (nnf_solver (Imp x (Or x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp x (Or y y))) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (Imp (And x y) z)) (nnf_solver (Imp x (Imp y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Imp y x) y)) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Imp x y) y)) (nnf_solver (Or x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (Imp (Or y x) z)) (nnf_solver (Imp (Or x y) z)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Or x x) y)) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Or y x) y)) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Or y (And y x))) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (Or y (Imp x z))) (nnf_solver (Imp x (Or y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form) (z Form))
    (= (nnf_solver (Or y (Or x z))) (nnf_solver (Or x (Or y z))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf (And (Neg x) (Neg y))) (nnf (Neg (Or x y))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf (Neg (And x (Neg y)))) (nnf (Or (Neg x) y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf (Neg (And (Neg x) y))) (nnf (Or x (Neg y))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf (Or (Neg x) (Neg y))) (nnf (Neg (And x y))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And x (Imp y Ffalse)))
      (nnf_solver (And x (Neg y))))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And y (Imp x Ftrue))) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And y (Imp Ffalse x))) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And x (Imp Ftrue y))) (nnf_solver (And x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And x (Or y Ffalse))) (nnf_solver (And x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (And y (Or x Ftrue))) (nnf_solver y))))
(prove
  :lemma
  (forall ((x Nat) (y Nat)) (nnf_solver (And (Var x) (Var y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp y (And x Ffalse))) (nnf_solver (Neg y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp x (And y Ftrue))) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Imp x Ffalse) y)) (nnf_solver (Or x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Imp Ftrue x) y)) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form) (y Form))
    (= (nnf_solver (Imp (Or x Ffalse) y)) (nnf_solver (Imp x y)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (And Ffalse Ffalse)))
      (And (nnf x) (And Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (And Ffalse Ftrue)))
      (And (nnf x) (And Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (And Ftrue Ffalse)))
      (And (nnf x) (And Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (And Ftrue Ftrue)))
      (And (nnf x) (And Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Imp Ffalse Ffalse)))
      (And (nnf x) (Imp Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Imp Ffalse Ftrue)))
      (And (nnf x) (Imp Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Imp Ftrue Ffalse)))
      (And (nnf x) (Imp Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Imp Ftrue Ftrue)))
      (And (nnf x) (Imp Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Or Ffalse Ffalse)))
      (And (nnf x) (Or Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Or Ffalse Ftrue)))
      (And (nnf x) (Or Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Or Ftrue Ffalse)))
      (And (nnf x) (Or Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And x (Or Ftrue Ftrue))) (And (nnf x) (Or Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (And Ffalse Ffalse) x))
      (And (And Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (And Ffalse Ftrue) x))
      (And (And Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (And Ftrue Ffalse) x))
      (And (And Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (And Ftrue Ftrue) x))
      (And (And Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Imp Ffalse Ffalse) x))
      (And (Imp Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Imp Ffalse Ftrue) x))
      (And (Imp Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Imp Ftrue Ffalse) x))
      (And (Imp Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Imp Ftrue Ftrue) x))
      (And (Imp Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Or Ffalse Ffalse) x))
      (And (Or Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Or Ffalse Ftrue) x))
      (And (Or Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Or Ftrue Ffalse) x))
      (And (Or Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (And (Or Ftrue Ftrue) x)) (And (Or Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (And Ffalse Ffalse)))
      (Imp (nnf x) (And Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (And Ffalse Ftrue)))
      (Imp (nnf x) (And Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (And Ftrue Ffalse)))
      (Imp (nnf x) (And Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (And Ftrue Ftrue)))
      (Imp (nnf x) (And Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Imp Ffalse Ffalse)))
      (Imp (nnf x) (Imp Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Imp Ffalse Ftrue)))
      (Imp (nnf x) (Imp Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Imp Ftrue Ffalse)))
      (Imp (nnf x) (Imp Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Imp Ftrue Ftrue)))
      (Imp (nnf x) (Imp Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Or Ffalse Ffalse)))
      (Imp (nnf x) (Or Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Or Ffalse Ftrue)))
      (Imp (nnf x) (Or Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Or Ftrue Ffalse)))
      (Imp (nnf x) (Or Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp x (Or Ftrue Ftrue))) (Imp (nnf x) (Or Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (And Ffalse Ffalse) x))
      (Imp (And Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (And Ffalse Ftrue) x))
      (Imp (And Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (And Ftrue Ffalse) x))
      (Imp (And Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (And Ftrue Ftrue) x))
      (Imp (And Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Imp Ffalse Ffalse) x))
      (Imp (Imp Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Imp Ffalse Ftrue) x))
      (Imp (Imp Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Imp Ftrue Ffalse) x))
      (Imp (Imp Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Imp Ftrue Ftrue) x))
      (Imp (Imp Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Or Ffalse Ffalse) x))
      (Imp (Or Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Or Ffalse Ftrue) x))
      (Imp (Or Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Or Ftrue Ffalse) x))
      (Imp (Or Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Imp (Or Ftrue Ftrue) x)) (Imp (Or Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (And Ffalse Ffalse)))
      (Or (nnf x) (And Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (And Ffalse Ftrue)))
      (Or (nnf x) (And Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (And Ftrue Ffalse)))
      (Or (nnf x) (And Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (And Ftrue Ftrue))) (Or (nnf x) (And Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Imp Ffalse Ffalse)))
      (Or (nnf x) (Imp Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Imp Ffalse Ftrue)))
      (Or (nnf x) (Imp Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Imp Ftrue Ffalse)))
      (Or (nnf x) (Imp Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Imp Ftrue Ftrue))) (Or (nnf x) (Imp Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Or Ffalse Ffalse)))
      (Or (nnf x) (Or Ffalse Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Or Ffalse Ftrue))) (Or (nnf x) (Or Ffalse Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Or Ftrue Ffalse))) (Or (nnf x) (Or Ftrue Ffalse)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or x (Or Ftrue Ftrue))) (Or (nnf x) (Or Ftrue Ftrue)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (And Ffalse Ffalse) x))
      (Or (And Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (And Ffalse Ftrue) x))
      (Or (And Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (And Ftrue Ffalse) x))
      (Or (And Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (And Ftrue Ftrue) x)) (Or (And Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Imp Ffalse Ffalse) x))
      (Or (Imp Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Imp Ffalse Ftrue) x))
      (Or (Imp Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Imp Ftrue Ffalse) x))
      (Or (Imp Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Imp Ftrue Ftrue) x)) (Or (Imp Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Or Ffalse Ffalse) x))
      (Or (Or Ffalse Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Or Ffalse Ftrue) x)) (Or (Or Ffalse Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Or Ftrue Ffalse) x)) (Or (Or Ftrue Ffalse) (nnf x)))))
(prove
  :lemma
  (forall ((x Form))
    (= (nnf (Or (Or Ftrue Ftrue) x)) (Or (Or Ftrue Ftrue) (nnf x)))))
(prove
  :lemma
  (forall ((x Form) (y Nat))
    (= (nnf_solver (And x (Var (S O))))
      (nnf_solver (And x (Var (S y)))))))
