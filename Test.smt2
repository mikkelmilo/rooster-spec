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
