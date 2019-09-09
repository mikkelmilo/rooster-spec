(declare-datatype
  Tree :source Tree
  (par (a)
    ((Leaf :source Leaf)
     (Node :source Node (proj1-Node a)
       (proj2-Node (Tree a)) (proj3-Node (Tree a))))))
(define-fun-rec
  mirror :keep :source mirror
  (par (a1) (((x (Tree a1))) (Tree a1)))
  (match x
    ((Leaf (_ Leaf a1))
     ((Node y l r) (Node y (mirror r) (mirror l))))))
(prove
  :lemma
  (par (x) (= (mirror (_ Leaf x)) (_ Leaf x))))
(prove
  :lemma
  (par (x) (forall ((y (Tree x))) (= (mirror (mirror y)) y))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (Tree x)))
      (= (mirror (Node y z (_ Leaf x)))
        (Node y (_ Leaf x) (mirror z))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (Tree x)) (x2 (Tree x)))
      (= (Node y (mirror x2) (mirror z)) (mirror (Node y z x2))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z x) (x2 (Tree x)))
      (= (mirror (Node y x2 (Node z (_ Leaf x) (_ Leaf x))))
        (Node y (Node z (_ Leaf x) (_ Leaf x)) (mirror x2))))))
(prove
  :lemma
  (par (x)
    (forall ((y x) (z (Tree x)) (x2 x))
      (= (mirror (Node y (Node x2 (_ Leaf x) (_ Leaf x)) z))
        (Node y (mirror z) (Node x2 (_ Leaf x) (_ Leaf x)))))))
