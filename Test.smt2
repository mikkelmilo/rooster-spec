(declare-datatype
  List :source List
  (par (a)
    ((Nil :source Nil)
     (Cons :source Cons (proj1-Cons a) (proj2-Cons (List a))))))
(define-fun-rec
  rev_append :keep :source rev_append
  (par (a1) (((x (List a1)) (y (List a1))) (List a1)))
  (match x
    ((Nil y)
     ((Cons b l0) (rev_append l0 (Cons b y))))))
(define-fun
  qrev :keep :source qrev
  (par (a1) (((x (List a1))) (List a1))) (rev_append x (_ Nil a1)))
(define-fun-rec
  app :keep :source app
  (par (a1) (((x (List a1)) (y (List a1))) (List a1)))
  (match x
    ((Nil y)
     ((Cons b l1) (Cons b (app l1 y))))))
(define-fun-rec
  rev :keep :source rev
  (par (a1) (((x (List a1))) (List a1)))
  (match x
    ((Nil (_ Nil a1))
     ((Cons y |l'|) (app (rev |l'|) (Cons y (_ Nil a1)))))))
