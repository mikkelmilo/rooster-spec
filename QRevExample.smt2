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
