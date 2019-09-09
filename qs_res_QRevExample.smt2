(declare-datatype
  list :source |[]|
  (par (a)
    ((nil :source |[]|) (cons :source |:| (head a) (tail (list a))))))
(define-fun-rec
  myqrev :keep :source myqrev
  (par (a1) (((x (list a1)) (y (list a1))) (list a1)))
  (match x
    ((nil y)
     ((cons b l0) (myqrev l0 (cons b y))))))
(define-fun-rec
  ++ :source ++
  (par (a) (((x (list a)) (y (list a))) (list a)))
  (match x
    ((nil y)
     ((cons z xs) (cons z (++ xs y))))))
(define-fun-rec
  rev :keep :source rev
  (par (a1) (((x (list a1))) (list a1)))
  (match x
    ((nil (_ nil a1))
     ((cons y |l'|) (++ (rev |l'|) (cons y (_ nil a1)))))))
(prove
  :lemma
  (par (x) (= (rev (_ nil x)) (_ nil x))))
(prove
  :lemma
  (par (x) (forall ((y (list x))) (= (++ y (_ nil x)) y))))
(prove
  :lemma
  (par (x) (forall ((y (list x))) (= (++ (_ nil x) y) y))))
(prove
  :lemma
  (par (x) (forall ((y (list x))) (= (myqrev y (_ nil x)) (rev y)))))
(prove
  :lemma
  (par (x) (forall ((y (list x))) (= (myqrev (_ nil x) y) y))))
(prove
  :lemma
  (par (x) (forall ((y (list x))) (= (rev (rev y)) y))))
(prove
  :lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (++ (rev y) z) (myqrev y z)))))
(prove
  :lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (rev (++ z y)) (myqrev y (rev z))))))
(prove
  :lemma
  (par (x)
    (forall ((y x)) (= (rev (cons y (_ nil x))) (cons y (_ nil x))))))
(prove
  :lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)))
      (= (++ (++ y z) x2) (++ y (++ z x2))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (cons y (++ z x2)) (++ (cons y z) x2)))))
