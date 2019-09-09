(declare-const __ :keep :source __ (par (any) any))
(declare-const false_rect :keep :source false_rect (par (a1) a1))
(define-fun
  true_rect :keep :source true_rect
  (par (a1) (((x a1)) a1)) x)
(define-fun
  true_rec :keep :source true_rec
  (par (a1) (((x a1)) a1)) x)
(define-fun
  false_rec :keep :source false_rec
  (par (a1) (() a1)) (_ false_rect a1))
(define-fun
  eq_rect_r :keep :source eq_rect_r
  (par (a1 a2) (((x a1) (y a2) (z a1)) a2)) y)
(define-fun
  eq_rect :keep :source eq_rect
  (par (a1 a2) (((x a1) (y a2) (z a1)) a2)) y)
(define-fun
  eq_rec_r :keep :source eq_rec_r
  (par (a1 a2) (((x a1) (y a2) (z a1)) a2)) y)
(define-fun
  eq_rec :keep :source eq_rec
  (par (a1 a2) (((x a1) (y a2) (z a1)) a2)) y)
(define-fun
  and_rect :keep :source and_rect
  (par (a1) (((x a1)) a1)) x)
(define-fun
  and_rec :keep :source and_rec
  (par (a1) (((x a1)) a1)) x)
