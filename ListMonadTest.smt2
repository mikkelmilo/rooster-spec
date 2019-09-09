(declare-datatype
  list :source |[]|
  (par (a)
    ((nil :source |[]|) (cons :source |:| (head a) (tail (list a))))))
(define-fun
  ret :keep :source ret
  (par (a1) (((x a1)) (list a1))) (cons x (_ nil a1)))
(define-fun-rec
  app :keep :source app
  (par (a1) (((x (list a1)) (y (list a1))) (list a1)))
  (match x
    ((nil y)
     ((cons b l1) (cons b (app l1 y))))))
(define-fun-rec
  flat_map :keep :source flat_map
  (par (a1 a2) (((x (=> a1 (list a2))) (y (list a1))) (list a2)))
  (match y
    ((nil (_ nil a2))
     ((cons z t) (app (@ x z) (flat_map x t))))))
(define-fun
  bind :keep :source bind
  (par (a1 a2) (((x (list a1)) (y (=> a1 (list a2)))) (list a2)))
  (flat_map y x))
(define-fun
  kleislicomp :keep :source kleislicomp
  (par (a1 a2 a3)
    (((x (=> a1 (list a2))) (y (=> a2 (list a3))) (z a1)) (list a3)))
  (bind (@ x z) y))
