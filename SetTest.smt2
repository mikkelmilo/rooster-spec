(declare-datatype
  pair :tuple :source |(,)|
  (par (a b)
    ((pair2 :tuple :source |(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatype
  list :source |[]|
  (par (a)
    ((nil :source |[]|) (cons :source |:| (head a) (tail (list a))))))
(define-fun-rec
  set_mem :keep :source set_mem
  (par (a1) (((x (=> a1 (=> a1 Bool))) (y a1) (z (list a1))) Bool))
  (match z
    ((nil false)
     ((cons a2 x1) (ite (@ (@ x y) a2) true (set_mem x y x1))))))
(define-fun-rec
  set_inter :keep :source set_inter
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y (list a1)) (z (list a1))) (list a1)))
  (match y
    ((nil (_ nil a1))
     ((cons a2 x1)
      (ite
        (set_mem x a2 z) (cons a2 (set_inter x x1 z))
        (set_inter x x1 z))))))
(define-fun-rec
  set_add :keep :source set_add
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y a1) (z (list a1))) (list a1)))
  (match z
    ((nil (cons y (_ nil a1)))
     ((cons a2 x1)
      (ite (@ (@ x y) a2) (cons a2 x1) (cons a2 (set_add x y x1)))))))
(define-fun-rec
  set_diff :keep :source set_diff
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y (list a1)) (z (list a1))) (list a1)))
  (match y
    ((nil (_ nil a1))
     ((cons a2 x1)
      (ite
        (set_mem x a2 z) (set_diff x x1 z)
        (set_add x a2 (set_diff x x1 z)))))))
(define-fun-rec
  set_union :keep :source set_union
  (par (a1)
    (((x (=> a1 (=> a1 Bool))) (y (list a1)) (z (list a1))) (list a1)))
  (match z
    ((nil y)
     ((cons a2 y1) (set_add x a2 (set_union x y y1))))))
(define-fun-rec
  map :keep :source map
  (par (a1 a2) (((x (=> a1 a2)) (y (list a1))) (list a2)))
  (match y
    ((nil (_ nil a2))
     ((cons b t) (cons (@ x b) (map x t))))))
(define-fun-rec
  fold_right :keep :source fold_right
  (par (a2 a1) (((x (=> a2 (=> a1 a1))) (y a1) (z (list a2))) a1))
  (match z
    ((nil y)
     ((cons b t) (@ (@ x b) (fold_right x y t))))))
(define-fun
  set_fold_right :keep :source set_fold_right
  (par (a1 a2) (((x (=> a1 (=> a2 a2))) (y (list a1)) (z a2)) a2))
  (fold_right x z y))
(define-fun
  set_map :keep :source set_map
  (par (a2 a1)
    (((x (=> a2 (=> a2 Bool))) (y (=> a1 a2)) (z (list a1)))
     (list a2)))
  (set_fold_right
    (lambda ((a a1)) (lambda ((x2 (list a2))) (set_add x (@ y a) x2)))
    z (_ nil a2)))
(define-fun-rec
  app :keep :source app
  (par (a1) (((x (list a1)) (y (list a1))) (list a1)))
  (match x
    ((nil y)
     ((cons b l1) (cons b (app l1 y))))))
(define-fun-rec
  list_prod :keep :source list_prod :source set_prod
  (par (a1 a2) (((x (list a1)) (y (list a2))) (list (pair a1 a2))))
  (match x
    ((nil (_ nil (pair a1 a2)))
     ((cons z t)
      (app (map (lambda ((y2 a2)) (pair2 z y2)) y) (list_prod t y))))))
