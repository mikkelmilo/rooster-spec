(declare-datatype
  List :source List
  (par (a)
    ((Nil :source Nil)
     (Cons :source Cons (proj1-Cons a) (proj2-Cons (List a))))))
(define-fun-rec
  app :keep :source app
  (par (a1) (((x (List a1)) (y (List a1))) (List a1)))
  (match x
    ((Nil y)
     ((Cons b l1) (Cons b (app l1 y))))))
(prove
  :lemma
  (par (x) (forall ((y (List x))) (= (app y (_ Nil x)) y))))
(prove
  :lemma
  (par (x) (forall ((y (List x))) (= (app (_ Nil x) y) y))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (List x)) (x2 (List x)))
      (= (app (Cons y z) x2) (Cons y (app z x2))))))
(prove
  :lemma
  (par (x)
    (forall ((y (List x)) (z (List x)) (x2 (List x)))
      (= (app (app y z) x2) (app y (app z x2))))))
