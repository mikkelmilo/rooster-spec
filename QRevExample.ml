
(** val rev : 'a1 ([]) -> 'a1 ([]) **)

let rec rev = function
| ([]) -> ([])
| (:) (x, l') -> (Prelude.++) (rev l') ((:) (x, ([])))

(** val rev_append : 'a1 ([]) -> 'a1 ([]) -> 'a1 ([]) **)

let rec rev_append l l' =
  match l with
  | ([]) -> l'
  | (:) (a, l0) -> rev_append l0 ((:) (a, l'))

(** val myrev : 'a1 ([]) -> 'a1 ([]) **)

let rec myrev = function
| ([]) -> ([])
| (:) (x, l') -> (Prelude.++) (rev l') ((:) (x, ([])))

(** val myqrev : 'a1 ([]) -> 'a1 ([]) -> 'a1 ([]) **)

let rec myqrev l l' =
  match l with
  | ([]) -> l'
  | (:) (a, l0) -> rev_append l0 ((:) (a, l'))
