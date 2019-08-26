(declare-datatype
  List (par (a) ((Nil) (Cons (proj1-Cons a) (proj2-Cons (List a))))))
(define-fun-rec
  rev_append
  (par (a1) (((x (List a1)) (y (List a1))) (List a1)))
  (match x
    ((Nil y)
     ((Cons b l0) (rev_append l0 (Cons b y))))))
(define-fun
  qrev
  (par (a1) (((x (List a1))) (List a1))) (rev_append x (_ Nil a1)))
(define-fun-rec
  app
  (par (a1) (((x (List a1)) (y (List a1))) (List a1)))
  (match x
    ((Nil y)
     ((Cons b l1) (Cons b (app l1 y))))))
(define-fun-rec
  rev
  (par (a1) (((x (List a1))) (List a1)))
  (match x
    ((Nil (_ Nil a1))
     ((Cons y |l'|) (app (rev |l'|) (Cons y (_ Nil a1)))))))
(prove (par (x) (forall ((y (List x))) (= (rev y) (qrev y)))))
(prove (par (x) (= (qrev (_ Nil x)) (_ Nil x))))
(prove (par (x) (forall ((y (List x))) (= (qrev (qrev y)) y))))
(prove
  (par (x)
    (forall ((y x)) (= (qrev (Cons y (_ Nil x))) (Cons y (_ Nil x))))))
