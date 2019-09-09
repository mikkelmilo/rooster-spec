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
        (((pair2 id c)
          (ite
            c (cons (pair2 id false) (next_assignment xs))
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
