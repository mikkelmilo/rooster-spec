(declare-datatype
  Option :source Option
  (par (a) ((Some :source Some (proj1-Some a)) (None :source None))))
(define-fun
  retOpt :keep :source retOpt
  (par (a1) (((x a1)) (Option a1))) (Some x))
(define-fun
  bindOpt :keep :source bindOpt
  (par (a1 a2)
    (((x (Option a1)) (y (=> a1 (Option a2)))) (Option a2)))
  (match x
    (((Some z) (@ y z))
     (None (_ None a2)))))
(prove
  :lemma
  (par (x)
    (forall ((y x))
      (= (@ (lambda ((z x)) (retOpt z)) y)
        (@ (lambda ((z x)) (Some z)) y)))))
(prove
  :lemma
  (par (x y)
    (forall ((z (=> x (Option y))))
      (= (bindOpt (_ None x) z) (_ None y)))))
(prove
  :lemma
  (par (x)
    (forall ((y (Option x)))
      (= (bindOpt y (lambda ((z x)) (Some z))) y))))
