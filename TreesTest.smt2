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
