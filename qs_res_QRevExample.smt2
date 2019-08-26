(declare-datatype
  List :source List
  (par (a)
    ((Nil :source Nil)
     (Cons :source Cons (proj1-Cons a) (proj2-Cons (List a))))))
(define-fun-rec
  zzrev_append :keep :source zzrev_append
  (par (a1) (((x (List a1)) (y (List a1))) (List a1)))
  (match x
    ((Nil y)
     ((Cons b l0) (zzrev_append l0 (Cons b y))))))
(define-fun
  qzzrev :keep :source qzzrev
  (par (a1) (((x (List a1))) (List a1))) (zzrev_append x (_ Nil a1)))
(define-fun-rec
  app :keep :source app
  (par (a1) (((x (List a1)) (y (List a1))) (List a1)))
  (match x
    ((Nil y)
     ((Cons b l1) (Cons b (app l1 y))))))
(define-fun-rec
  zzrev :keep :source zzrev
  (par (a1) (((x (List a1))) (List a1)))
  (match x
    ((Nil (_ Nil a1))
     ((Cons y |l'|) (app (zzrev |l'|) (Cons y (_ Nil a1)))))))
(prove
  :lemma
  (par (x) (= (zzrev (_ Nil x)) (_ Nil x))))
(prove
  :lemma
  (par (x) (forall ((y (List x))) (= (zzrev (zzrev y)) y))))
(prove
  :lemma
  (par (x)
    (forall ((y x))
      (= (zzrev (Cons y (_ Nil x))) (Cons y (_ Nil x))))))
