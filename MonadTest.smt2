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
