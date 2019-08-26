(declare-datatype
  pair :tuple :source |(,)|
  (par (a b)
    ((pair2 :tuple :source |(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatype
  Uint :source Uint
  ((Nil :source Nil) (D0 :source D0 (proj1-D0 Uint))
   (D1 :source D1 (proj1-D1 Uint)) (D2 :source D2 (proj1-D2 Uint))
   (D3 :source D3 (proj1-D3 Uint)) (D4 :source D4 (proj1-D4 Uint))
   (D5 :source D5 (proj1-D5 Uint)) (D6 :source D6 (proj1-D6 Uint))
   (D7 :source D7 (proj1-D7 Uint)) (D8 :source D8 (proj1-D8 Uint))
   (D9 :source D9 (proj1-D9 Uint))))
(declare-datatype
  Positive :source Positive
  ((XI :source XI (proj1-XI Positive))
   (XO :source XO (proj1-XO Positive)) (XH :source XH)))
(declare-datatype
  Z :source Z
  ((Z0 :source Z0) (Zpos :source Zpos (proj1-Zpos Positive))
   (Zneg :source Zneg (proj1-Zneg Positive))))
(declare-datatype
  Nat :source Nat ((O :source O) (S :source S (proj1-S Nat))))
(declare-datatype
  N :source N
  ((N0 :source N0) (Npos :source Npos (proj1-Npos Positive))))
(declare-datatype
  Mask :source Mask
  ((IsNul :source IsNul) (IsPos :source IsPos (proj1-IsPos Positive))
   (IsNeg :source IsNeg)))
(declare-datatype
  Int2 :source |Int|
  ((Pos :source Pos (proj1-Pos Uint))
   (Neg :source Neg (proj1-Neg Uint))))
(declare-datatype
  Comparison :source Comparison
  ((Eq :source Eq) (Lt :source Lt) (Gt :source Gt)))
(define-fun
  zero :keep :source zero
  () Z Z0)
(define-fun
  two :keep :source two
  () Z (Zpos (XO XH)))
(define-fun
  to_pos :keep :source to_pos
  ((x Z)) Positive
  (match x
    ((_ XH)
     ((Zpos p) p))))
(define-fun
  to_N :keep :source to_N
  ((x Z)) N
  (match x
    ((_ N0)
     ((Zpos p) (Npos p)))))
(define-fun
  succ_double_mask :keep :source succ_double_mask
  ((x Mask)) Mask
  (match x
    ((IsNul (IsPos XH))
     ((IsPos p) (IsPos (XI p)))
     (IsNeg IsNeg))))
(define-fun
  succ_double0 :keep :source succ_double0
  ((x N)) N
  (match x
    ((N0 (Npos XH))
     ((Npos p) (Npos (XI p))))))
(define-fun-rec
  succ :keep :source succ
  ((x Positive)) Positive
  (match x
    (((XI p) (XO (succ p)))
     ((XO q) (XI q))
     (XH (XO XH)))))
(define-fun
  succ_pos :keep :source succ_pos
  ((x N)) Positive
  (match x
    ((N0 XH)
     ((Npos p) (succ p)))))
(define-fun
  snd :keep :source snd
  (par (a1 a2) (((x (pair a1 a2))) a2)) (match x (((pair2 y z) z))))
(define-fun-rec
  size_nat :keep :source size_nat
  ((x Positive)) Nat
  (match x
    (((XI p0) (S (size_nat p0)))
     ((XO p02) (S (size_nat p02)))
     (XH (S O)))))
(define-fun-rec
  size :keep :source size
  ((x Positive)) Positive
  (match x
    (((XI p0) (succ (size p0)))
     ((XO p02) (succ (size p02)))
     (XH XH))))
(define-fun
  sgn :keep :source sgn
  ((x Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos y) (Zpos XH))
     ((Zneg z) (Zneg XH)))))
(define-fun-rec
  revapp :keep :source revapp
  ((x Uint) (y Uint)) Uint
  (match x
    ((Nil y)
     ((D0 d0) (revapp d0 (D0 y)))
     ((D1 d02) (revapp d02 (D1 y)))
     ((D2 d03) (revapp d03 (D2 y)))
     ((D3 d04) (revapp d04 (D3 y)))
     ((D4 d05) (revapp d05 (D4 y)))
     ((D5 d06) (revapp d06 (D5 y)))
     ((D6 d07) (revapp d07 (D6 y)))
     ((D7 d08) (revapp d08 (D7 y)))
     ((D8 d09) (revapp d09 (D8 y)))
     ((D9 d010) (revapp d010 (D9 y))))))
(define-fun
  rev :keep :source rev
  ((x Uint)) Uint (revapp x Nil))
(define-fun-rec
  pred_double :keep :source pred_double
  ((x Positive)) Positive
  (match x
    (((XI p) (XI (XO p)))
     ((XO q) (XI (pred_double q)))
     (XH XH))))
(define-fun
  pred_double0 :keep :source pred_double0
  ((x Z)) Z
  (match x
    ((Z0 (Zneg XH))
     ((Zpos p) (Zpos (pred_double p)))
     ((Zneg q) (Zneg (XI q))))))
(define-fun
  succ_double1 :keep :source succ_double1
  ((x Z)) Z
  (match x
    ((Z0 (Zpos XH))
     ((Zpos p) (Zpos (XI p)))
     ((Zneg q) (Zneg (pred_double q))))))
(define-fun
  pred_N :keep :source pred_N
  ((x Positive)) N
  (match x
    (((XI p) (Npos (XO p)))
     ((XO q) (Npos (pred_double q)))
     (XH N0))))
(define-fun-rec
  testbit :keep :source testbit
  ((x Positive) (y N)) Bool
  (match x
    (((XI p0)
      (match y
        ((N0 true)
         ((Npos n0) (testbit p0 (pred_N n0))))))
     ((XO p02)
      (match y
        ((N0 false)
         ((Npos n02) (testbit p02 (pred_N n02))))))
     (XH
      (match y
        ((N0 true)
         ((Npos z) false)))))))
(define-fun
  testbit0 :keep :source testbit0
  ((x N) (y N)) Bool
  (match x
    ((N0 false)
     ((Npos p) (testbit p y)))))
(define-fun
  opp :keep :source opp
  ((x Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos x0) (Zneg x0))
     ((Zneg x02) (Zpos x02)))))
(define-fun
  zzopp :keep :source zzopp
  ((x Z)) Z (opp x))
(define-fun
  one :keep :source one
  () Z (Zpos XH))
(define-fun-rec
  of_succ_nat :keep :source of_succ_nat
  ((x Nat)) Positive
  (match x
    ((O XH)
     ((S y) (succ (of_succ_nat y))))))
(define-fun
  of_nat :keep :source of_nat
  ((x Nat)) Z
  (match x
    ((O Z0)
     ((S n0) (Zpos (of_succ_nat n0))))))
(define-fun
  of_N :keep :source of_N
  ((x N)) Z
  (match x
    ((N0 Z0)
     ((Npos p) (Zpos p)))))
(define-fun
  odd :keep :source odd
  ((x Z)) Bool
  (match x
    ((Z0 false)
     ((Zpos p)
      (match p
        ((_ true)
         ((XO y) false))))
     ((Zneg q)
      (match q
        ((_ true)
         ((XO z) false)))))))
(define-fun
  testbit1 :keep :source testbit1
  ((x Z) (y Z)) Bool
  (match y
    ((Z0 (odd x))
     ((Zpos p)
      (match x
        ((Z0 false)
         ((Zpos a0) (testbit a0 (Npos p)))
         ((Zneg a02) (not (testbit0 (pred_N a02) (Npos p)))))))
     ((Zneg z) false))))
(define-fun
  nsucc_double :keep :source nsucc_double
  ((x N)) N
  (match x
    ((N0 (Npos XH))
     ((Npos p) (Npos (XI p))))))
(define-fun
  ndouble :keep :source ndouble
  ((x N)) N
  (match x
    ((N0 N0)
     ((Npos p) (Npos (XO p))))))
(define-fun-rec
  lxor :keep :source lxor
  ((x Positive) (y Positive)) N
  (match x
    (((XI p0)
      (match y
        (((XI q0) (ndouble (lxor p0 q0)))
         ((XO q02) (nsucc_double (lxor p0 q02)))
         (XH (Npos (XO p0))))))
     ((XO p02)
      (match y
        (((XI q03) (nsucc_double (lxor p02 q03)))
         ((XO q04) (ndouble (lxor p02 q04)))
         (XH (Npos (XI p02))))))
     (XH
      (match y
        (((XI q05) (Npos (XO q05)))
         ((XO q06) (Npos (XI q06)))
         (XH N0)))))))
(define-fun
  lxor0 :keep :source lxor0
  ((x N) (y N)) N
  (match x
    ((N0 y)
     ((Npos p)
      (match y
        ((N0 (Npos p))
         ((Npos q) (lxor p q))))))))
(define-fun
  lxor1 :keep :source lxor1
  ((x Z) (y Z)) Z
  (match x
    ((Z0 y)
     ((Zpos a0)
      (match y
        ((Z0 (Zpos a0))
         ((Zpos b0)
          (match (lxor a0 b0)
            ((N0 Z0)
             ((Npos p) (Zpos p)))))
         ((Zneg b02) (Zneg (succ_pos (lxor0 (Npos a0) (pred_N b02))))))))
     ((Zneg a02)
      (match y
        ((Z0 (Zneg a02))
         ((Zpos b03) (Zneg (succ_pos (lxor0 (pred_N a02) (Npos b03)))))
         ((Zneg b04)
          (match (lxor0 (pred_N a02) (pred_N b04))
            ((N0 Z0)
             ((Npos q) (Zpos q)))))))))))
(define-fun-rec
  lor :keep :source lor
  ((x Positive) (y Positive)) Positive
  (match x
    (((XI p0)
      (match y
        (((XI q0) (XI (lor p0 q0)))
         ((XO q02) (XI (lor p0 q02)))
         (XH (XI p0)))))
     ((XO p02)
      (match y
        (((XI q03) (XI (lor p02 q03)))
         ((XO q04) (XO (lor p02 q04)))
         (XH (XI p02)))))
     (XH
      (match y
        ((_ y)
         ((XO q05) (XI q05))))))))
(define-fun
  lor0 :keep :source lor0
  ((x N) (y N)) N
  (match x
    ((N0 y)
     ((Npos p)
      (match y
        ((N0 (Npos p))
         ((Npos q) (Npos (lor p q)))))))))
(define-fun
  log2 :keep :source log2
  ((x Z)) Z
  (match x
    ((_ Z0)
     ((Zpos p0)
      (match p0
        (((XI p) (Zpos (size p)))
         ((XO q) (Zpos (size q)))
         (XH Z0)))))))
(define-fun-rec
  ldiff :keep :source ldiff
  ((x Positive) (y Positive)) N
  (match x
    (((XI p0)
      (match y
        (((XI q0) (ndouble (ldiff p0 q0)))
         ((XO q02) (nsucc_double (ldiff p0 q02)))
         (XH (Npos (XO p0))))))
     ((XO p02)
      (match y
        (((XI q03) (ndouble (ldiff p02 q03)))
         ((XO q04) (ndouble (ldiff p02 q04)))
         (XH (Npos (XO p02))))))
     (XH
      (match y
        ((_ N0)
         ((XO z) (Npos XH))))))))
(define-fun
  ldiff0 :keep :source ldiff0
  ((x N) (y N)) N
  (match x
    ((N0 N0)
     ((Npos p)
      (match y
        ((N0 (Npos p))
         ((Npos q) (ldiff p q))))))))
(define-fun-rec
  land :keep :source land
  ((x Positive) (y Positive)) N
  (match x
    (((XI p0)
      (match y
        (((XI q0) (nsucc_double (land p0 q0)))
         ((XO q02) (ndouble (land p0 q02)))
         (XH (Npos XH)))))
     ((XO p02)
      (match y
        (((XI q03) (ndouble (land p02 q03)))
         ((XO q04) (ndouble (land p02 q04)))
         (XH N0))))
     (XH
      (match y
        ((_ (Npos XH))
         ((XO z) N0)))))))
(define-fun
  land0 :keep :source land0
  ((x N) (y N)) N
  (match x
    ((N0 N0)
     ((Npos p)
      (match y
        ((N0 N0)
         ((Npos q) (land p q))))))))
(define-fun
  lor1 :keep :source lor1
  ((x Z) (y Z)) Z
  (match x
    ((Z0 y)
     ((Zpos a0)
      (match y
        ((Z0 (Zpos a0))
         ((Zpos b0) (Zpos (lor a0 b0)))
         ((Zneg b02)
          (match (pred_N b02)
            ((N0 (Zneg XH))
             ((Npos p) (Zneg (succ_pos (ldiff p a0))))))))))
     ((Zneg a02)
      (match y
        ((Z0 (Zneg a02))
         ((Zpos b03)
          (match (pred_N a02)
            ((N0 (Zneg XH))
             ((Npos p) (Zneg (succ_pos (ldiff p b03)))))))
         ((Zneg b04)
          (Zneg (succ_pos (land0 (pred_N a02) (pred_N b04)))))))))))
(define-fun
  land1 :keep :source land1
  ((x Z) (y Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos a0)
      (match y
        ((Z0 Z0)
         ((Zpos b0)
          (match (land a0 b0)
            ((N0 Z0)
             ((Npos p) (Zpos p)))))
         ((Zneg b02)
          (match (pred_N b02)
            ((N0 (Zpos a0))
             ((Npos q)
              (match (ldiff a0 q)
                ((N0 Z0)
                 ((Npos r) (Zpos r)))))))))))
     ((Zneg a02)
      (match y
        ((Z0 Z0)
         ((Zpos b03)
          (match (pred_N a02)
            ((N0 (Zpos b03))
             ((Npos q)
              (match (ldiff b03 q)
                ((N0 Z0)
                 ((Npos p2) (Zpos p2))))))))
         ((Zneg b04)
          (Zneg (succ_pos (lor0 (pred_N a02) (pred_N b04)))))))))))
(define-fun
  ldiff1 :keep :source ldiff1
  ((x Z) (y Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos a0)
      (match y
        ((Z0 (Zpos a0))
         ((Zpos b0)
          (match (ldiff a0 b0)
            ((N0 Z0)
             ((Npos p) (Zpos p)))))
         ((Zneg b02)
          (match (pred_N b02)
            ((N0 Z0)
             ((Npos q)
              (match (land a0 q)
                ((N0 Z0)
                 ((Npos r) (Zpos r)))))))))))
     ((Zneg a02)
      (match y
        ((Z0 (Zneg a02))
         ((Zpos b03) (Zneg (succ_pos (lor0 (pred_N a02) (Npos b03)))))
         ((Zneg b04)
          (match (ldiff0 (pred_N b04) (pred_N a02))
            ((N0 Z0)
             ((Npos p2) (Zpos p2)))))))))))
(define-fun-rec
  iter_op :keep :source iter_op
  (par (a1) (((x (=> a1 (=> a1 a1))) (y Positive) (z a1)) a1))
  (match y
    (((XI p0) (@ (@ x z) (iter_op x p0 (@ (@ x z) z))))
     ((XO p02) (iter_op x p02 (@ (@ x z) z)))
     (XH z))))
(define-fun-rec
  iter :keep :source iter
  (par (a1) (((x (=> a1 a1)) (y a1) (z Positive)) a1))
  (match z
    (((XI |n'|) (@ x (iter x (iter x y |n'|) |n'|)))
     ((XO |n'2|) (iter x (iter x y |n'2|) |n'2|))
     (XH (@ x y)))))
(define-fun
  iter0 :keep :source iter0
  (par (a1) (((x Z) (y (=> a1 a1)) (z a1)) a1))
  (match x
    ((_ z)
     ((Zpos p) (iter y z p)))))
(define-fun
  fst :keep :source fst
  (par (a1 a2) (((x (pair a1 a2))) a1)) (match x (((pair2 y z) y))))
(define-fun
  even :keep :source even
  ((x Z)) Bool
  (match x
    ((Z0 true)
     ((Zpos p)
      (match p
        ((_ false)
         ((XO y) true))))
     ((Zneg q)
      (match q
        ((_ false)
         ((XO z) true)))))))
(define-fun-rec
  eqb :keep :source eqb
  ((x Positive) (y Positive)) Bool
  (match x
    (((XI p0)
      (match y
        ((_ false)
         ((XI q0) (eqb p0 q0)))))
     ((XO p02)
      (match y
        ((_ false)
         ((XO q02) (eqb p02 q02)))))
     (XH
      (match y
        ((_ false)
         (XH true)))))))
(define-fun
  eqb0 :keep :source eqb0
  ((x Z) (y Z)) Bool
  (match x
    ((Z0
      (match y
        ((_ false)
         (Z0 true))))
     ((Zpos p)
      (match y
        ((_ false)
         ((Zpos q) (eqb p q)))))
     ((Zneg r)
      (match y
        ((_ false)
         ((Zneg q2) (eqb r q2))))))))
(define-fun
  double_pred_mask :keep :source double_pred_mask
  ((x Positive)) Mask
  (match x
    (((XI p) (IsPos (XO (XO p))))
     ((XO q) (IsPos (XO (pred_double q))))
     (XH IsNul))))
(define-fun
  double_mask :keep :source double_mask
  ((x Mask)) Mask
  (match x
    ((_ x)
     ((IsPos p) (IsPos (XO p))))))
(define-funs-rec
  ((sub_mask_carry :keep :source sub_mask_carry
    ((x Positive) (y Positive)) Mask)
   (sub_mask :keep :source sub_mask
    ((x Positive) (y Positive)) Mask))
  ((match x
     (((XI p)
       (match y
         (((XI q) (succ_double_mask (sub_mask_carry p q)))
          ((XO r) (double_mask (sub_mask p r)))
          (XH (IsPos (pred_double p))))))
      ((XO p2)
       (match y
         (((XI q2) (double_mask (sub_mask_carry p2 q2)))
          ((XO q3) (succ_double_mask (sub_mask_carry p2 q3)))
          (XH (double_pred_mask p2)))))
      (XH IsNeg)))
   (match x
     (((XI p)
       (match y
         (((XI q) (double_mask (sub_mask p q)))
          ((XO r) (succ_double_mask (sub_mask p r)))
          (XH (IsPos (XO p))))))
      ((XO p2)
       (match y
         (((XI q2) (succ_double_mask (sub_mask_carry p2 q2)))
          ((XO q3) (double_mask (sub_mask p2 q3)))
          (XH (IsPos (pred_double p2))))))
      (XH
       (match y
         ((_ IsNeg)
          (XH IsNul))))))))
(define-fun
  sub :keep :source sub
  ((x Positive) (y Positive)) Positive
  (match (sub_mask x y)
    ((_ XH)
     ((IsPos z) z))))
(define-fun
  sub0 :keep :source sub0
  ((x N) (y N)) N
  (match x
    ((N0 N0)
     ((Npos |n'|)
      (match y
        ((N0 (Npos |n'|))
         ((Npos |m'|)
          (match (sub_mask |n'| |m'|)
            ((_ N0)
             ((IsPos p) (Npos p)))))))))))
(define-fun
  double1 :keep :source double1
  ((x Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos p) (Zpos (XO p)))
     ((Zneg q) (Zneg (XO q))))))
(define-fun-rec
  pos_sub :keep :source pos_sub
  ((x Positive) (y Positive)) Z
  (match x
    (((XI p)
      (match y
        (((XI q) (double1 (pos_sub p q)))
         ((XO r) (succ_double1 (pos_sub p r)))
         (XH (Zpos (XO p))))))
     ((XO p2)
      (match y
        (((XI q2) (pred_double0 (pos_sub p2 q2)))
         ((XO q3) (double1 (pos_sub p2 q3)))
         (XH (Zpos (pred_double p2))))))
     (XH
      (match y
        (((XI q4) (Zneg (XO q4)))
         ((XO q5) (Zneg (pred_double q5)))
         (XH Z0)))))))
(define-fun
  zzdouble :keep :source zzdouble
  ((x Z)) Z (double1 x))
(define-fun
  double0 :keep :source double0
  ((x N)) N
  (match x
    ((N0 N0)
     ((Npos p) (Npos (XO p))))))
(define-funs-rec
  ((double :keep :source double
    ((x Uint)) Uint)
   (succ_double :keep :source succ_double
    ((x Uint)) Uint))
  ((match x
     ((Nil Nil)
      ((D0 d0) (D0 (double d0)))
      ((D1 d02) (D2 (double d02)))
      ((D2 d03) (D4 (double d03)))
      ((D3 d04) (D6 (double d04)))
      ((D4 d05) (D8 (double d05)))
      ((D5 d06) (D0 (succ_double d06)))
      ((D6 d07) (D2 (succ_double d07)))
      ((D7 d08) (D4 (succ_double d08)))
      ((D8 d09) (D6 (succ_double d09)))
      ((D9 d010) (D8 (succ_double d010)))))
   (match x
     ((Nil (D1 Nil))
      ((D0 d0) (D1 (double d0)))
      ((D1 d02) (D3 (double d02)))
      ((D2 d03) (D5 (double d03)))
      ((D3 d04) (D7 (double d04)))
      ((D4 d05) (D9 (double d05)))
      ((D5 d06) (D1 (succ_double d06)))
      ((D6 d07) (D3 (succ_double d07)))
      ((D7 d08) (D5 (succ_double d08)))
      ((D8 d09) (D7 (succ_double d09)))
      ((D9 d010) (D9 (succ_double d010)))))))
(define-fun-rec
  to_little_uint :keep :source to_little_uint
  ((x Positive)) Uint
  (match x
    (((XI p0) (succ_double (to_little_uint p0)))
     ((XO p02) (double (to_little_uint p02)))
     (XH (D1 Nil)))))
(define-fun
  to_uint :keep :source to_uint
  ((x Positive)) Uint (rev (to_little_uint x)))
(define-fun
  to_int :keep :source to_int
  ((x Z)) Int2
  (match x
    ((Z0 (Pos (D0 Nil)))
     ((Zpos p) (Pos (to_uint p)))
     ((Zneg q) (Neg (to_uint q))))))
(define-fun
  div2_up :keep :source div2_up
  ((x Positive)) Positive
  (match x
    (((XI p0) (succ p0))
     ((XO p02) p02)
     (XH XH))))
(define-fun
  div2 :keep :source div2
  ((x Positive)) Positive
  (match x
    (((XI p0) p0)
     ((XO p02) p02)
     (XH XH))))
(define-fun
  quot2 :keep :source quot2
  ((x Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos p)
      (match p
        ((_ (Zpos (div2 p)))
         (XH Z0))))
     ((Zneg q)
      (match q
        ((_ (Zneg (div2 q)))
         (XH Z0)))))))
(define-fun
  div0 :keep :source div0
  ((x Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos p)
      (match p
        ((_ (Zpos (div2 p)))
         (XH Z0))))
     ((Zneg q) (Zneg (div2_up q))))))
(define-fun-rec
  compare_cont :keep :source compare_cont
  ((x Comparison) (y Positive) (z Positive)) Comparison
  (match y
    (((XI p)
      (match z
        (((XI q) (compare_cont x p q))
         ((XO r) (compare_cont Gt p r))
         (XH Gt))))
     ((XO p2)
      (match z
        (((XI q2) (compare_cont Lt p2 q2))
         ((XO q3) (compare_cont x p2 q3))
         (XH Gt))))
     (XH
      (match z
        ((_ Lt)
         (XH x)))))))
(define-fun
  compare :keep :source compare
  ((x Positive) (y Positive)) Comparison (compare_cont Eq x y))
(define-fun
  compare0 :keep :source compare0
  ((x N) (y N)) Comparison
  (match x
    ((N0
      (match y
        ((N0 Eq)
         ((Npos z) Lt))))
     ((Npos |n'|)
      (match y
        ((N0 Gt)
         ((Npos |m'|) (compare |n'| |m'|))))))))
(define-fun
  leb0 :keep :source leb0
  ((x N) (y N)) Bool
  (match (compare0 x y)
    ((_ true)
     (Gt false))))
(define-fun-rec
  pos_div_eucl :keep :source pos_div_eucl
  ((x Positive) (y N)) (pair N N)
  (match x
    (((XI |a'|)
      (match (pos_div_eucl |a'| y)
        (((pair2 q r)
          (let ((|r'| (succ_double0 r)))
            (ite
              (leb0 y |r'|) (pair2 (succ_double0 q) (sub0 |r'| y))
              (pair2 (double0 q) |r'|)))))))
     ((XO |a'2|)
      (match (pos_div_eucl |a'2| y)
        (((pair2 p r2)
          (let ((|r'2| (double0 r2)))
            (ite
              (leb0 y |r'2|) (pair2 (succ_double0 p) (sub0 |r'2| y))
              (pair2 (double0 p) |r'2|)))))))
     (XH
      (match y
        ((N0 (pair2 N0 (Npos XH)))
         ((Npos p2)
          (match p2
            ((_ (pair2 N0 (Npos XH)))
             (XH (pair2 (Npos XH) N0)))))))))))
(define-fun
  quotrem :keep :source quotrem
  ((x Z) (y Z)) (pair Z Z)
  (match x
    ((Z0 (pair2 Z0 Z0))
     ((Zpos a0)
      (match y
        ((Z0 (pair2 Z0 (Zpos a0)))
         ((Zpos b0)
          (match (pos_div_eucl a0 (Npos b0))
            (((pair2 q r) (pair2 (of_N q) (of_N r))))))
         ((Zneg b02)
          (match (pos_div_eucl a0 (Npos b02))
            (((pair2 p r2) (pair2 (opp (of_N p)) (of_N r2)))))))))
     ((Zneg a02)
      (match y
        ((Z0 (pair2 Z0 (Zneg a02)))
         ((Zpos b03)
          (match (pos_div_eucl a02 (Npos b03))
            (((pair2 q2 r3) (pair2 (opp (of_N q2)) (opp (of_N r3)))))))
         ((Zneg b04)
          (match (pos_div_eucl a02 (Npos b04))
            (((pair2 q3 r4) (pair2 (of_N q3) (opp (of_N r4)))))))))))))
(define-fun
  quot :keep :source quot
  ((x Z) (y Z)) Z (fst (quotrem x y)))
(define-fun
  rem :keep :source rem
  ((x Z) (y Z)) Z (snd (quotrem x y)))
(define-fun
  compare1 :keep :source compare1
  ((x Z) (y Z)) Comparison
  (match x
    ((Z0
      (match y
        ((Z0 Eq)
         ((Zpos z) Lt)
         ((Zneg x2) Gt))))
     ((Zpos |x'|)
      (match y
        ((_ Gt)
         ((Zpos |y'|) (compare |x'| |y'|)))))
     ((Zneg |x'2|)
      (match y
        ((_ Lt)
         ((Zneg |y'2|)
          (match (compare |x'2| |y'2|)
            ((Eq Eq)
             (Lt Gt)
             (Gt Lt))))))))))
(define-fun
  geb :keep :source geb
  ((x Z) (y Z)) Bool
  (match (compare1 x y)
    ((_ true)
     (Lt false))))
(define-fun
  gtb :keep :source gtb
  ((x Z) (y Z)) Bool
  (match (compare1 x y)
    ((_ false)
     (Gt true))))
(define-fun
  leb1 :keep :source leb1
  ((x Z) (y Z)) Bool
  (match (compare1 x y)
    ((_ true)
     (Gt false))))
(define-fun
  zzleb :keep :source zzleb
  ((x Z) (y Z)) Bool (leb1 x y))
(define-fun
  ltb :keep :source ltb
  ((x Z) (y Z)) Bool
  (match (compare1 x y)
    ((_ false)
     (Lt true))))
(define-fun
  max :keep :source max
  ((x Z) (y Z)) Z
  (match (compare1 x y)
    ((_ x)
     (Lt y))))
(define-fun
  min :keep :source min
  ((x Z) (y Z)) Z
  (match (compare1 x y)
    ((_ x)
     (Gt y))))
(define-fun
  zzcompare :keep :source zzcompare
  ((x Z) (y Z)) Comparison (compare1 x y))
(define-fun-rec
  gcdn :keep :source gcdn
  ((x Nat) (y Positive) (z Positive)) Positive
  (match x
    ((O XH)
     ((S n0)
      (match y
        (((XI |a'|)
          (match z
            (((XI |b'|)
              (match (compare |a'| |b'|)
                ((Eq (XI |a'|))
                 (Lt (gcdn n0 (sub |b'| |a'|) (XI |a'|)))
                 (Gt (gcdn n0 (sub |a'| |b'|) (XI |b'|))))))
             ((XO b0) (gcdn n0 (XI |a'|) b0))
             (XH XH))))
         ((XO a0)
          (match z
            (((XI x2) (gcdn n0 a0 (XI x2)))
             ((XO b02) (XO (gcdn n0 a0 b02)))
             (XH XH))))
         (XH XH)))))))
(define-fun
  leb :keep :source leb
  ((x Positive) (y Positive)) Bool
  (match (compare x y)
    ((_ true)
     (Gt false))))
(define-fun
  sqrtrem_step :keep :source sqrtrem_step
  ((x (=> Positive Positive)) (y (=> Positive Positive))
   (z (pair Positive Mask)))
  (pair Positive Mask)
  (match z
    (((pair2 s y2)
      (match y2
        ((_ (pair2 (XO s) (sub_mask (@ y (@ x XH)) (XO (XO XH)))))
         ((IsPos r)
          (let
            ((|s'| (XI (XO s)))
             (|r'| (@ y (@ x r))))
            (ite
              (leb |s'| |r'|) (pair2 (XI s) (sub_mask |r'| |s'|))
              (pair2 (XO s) (IsPos |r'|)))))))))))
(define-fun-rec
  sqrtrem :keep :source sqrtrem
  ((x Positive)) (pair Positive Mask)
  (match x
    (((XI p0)
      (match p0
        (((XI p1)
          (sqrtrem_step (lambda ((y Positive)) (XI y))
            (lambda ((z Positive)) (XI z)) (sqrtrem p1)))
         ((XO p12)
          (sqrtrem_step (lambda ((x2 Positive)) (XO x2))
            (lambda ((x3 Positive)) (XI x3)) (sqrtrem p12)))
         (XH (pair2 XH (IsPos (XO XH)))))))
     ((XO p02)
      (match p02
        (((XI p13)
          (sqrtrem_step (lambda ((x4 Positive)) (XI x4))
            (lambda ((x5 Positive)) (XO x5)) (sqrtrem p13)))
         ((XO p14)
          (sqrtrem_step (lambda ((x6 Positive)) (XO x6))
            (lambda ((x7 Positive)) (XO x7)) (sqrtrem p14)))
         (XH (pair2 XH (IsPos XH))))))
     (XH (pair2 XH IsNul)))))
(define-fun
  sqrt :keep :source sqrt
  ((x Positive)) Positive (fst (sqrtrem x)))
(define-fun
  sqrt0 :keep :source sqrt0
  ((x Z)) Z
  (match x
    ((_ Z0)
     ((Zpos p) (Zpos (sqrt p))))))
(define-fun
  sqrtrem0 :keep :source sqrtrem0
  ((x Z)) (pair Z Z)
  (match x
    ((_ (pair2 Z0 Z0))
     ((Zpos p)
      (match (sqrtrem p)
        (((pair2 s m)
          (match m
            ((_ (pair2 (Zpos s) Z0))
             ((IsPos r) (pair2 (Zpos s) (Zpos r))))))))))))
(define-fun
  compOpp :keep :source compOpp
  ((x Comparison)) Comparison
  (match x
    ((Eq Eq)
     (Lt Gt)
     (Gt Lt))))
(define-funs-rec
  ((add0 :keep :source add0
    ((x Positive) (y Positive)) Positive)
   (add_carry :keep :source add_carry
    ((x Positive) (y Positive)) Positive))
  ((match x
     (((XI p)
       (match y
         (((XI q) (XO (add_carry p q)))
          ((XO r) (XI (add0 p r)))
          (XH (XO (succ p))))))
      ((XO p2)
       (match y
         (((XI q2) (XI (add0 p2 q2)))
          ((XO q3) (XO (add0 p2 q3)))
          (XH (XI p2)))))
      (XH
       (match y
         (((XI q4) (XO (succ q4)))
          ((XO q5) (XI q5))
          (XH (XO XH)))))))
   (match x
     (((XI p)
       (match y
         (((XI q) (XI (add_carry p q)))
          ((XO r) (XO (add_carry p r)))
          (XH (XI (succ p))))))
      ((XO p2)
       (match y
         (((XI q2) (XO (add_carry p2 q2)))
          ((XO q3) (XI (add0 p2 q3)))
          (XH (XO (succ p2))))))
      (XH
       (match y
         (((XI q4) (XI (succ q4)))
          ((XO q5) (XO (succ q5)))
          (XH (XI XH)))))))))
(define-fun
  add1 :keep :source add1
  ((x Z) (y Z)) Z
  (match x
    ((Z0 y)
     ((Zpos |x'|)
      (match y
        ((Z0 (Zpos |x'|))
         ((Zpos |y'|) (Zpos (add0 |x'| |y'|)))
         ((Zneg |y'2|) (pos_sub |x'| |y'2|)))))
     ((Zneg |x'2|)
      (match y
        ((Z0 (Zneg |x'2|))
         ((Zpos |y'3|) (pos_sub |y'3| |x'2|))
         ((Zneg |y'4|) (Zneg (add0 |x'2| |y'4|)))))))))
(define-fun
  pred :keep :source pred
  ((x Z)) Z (add1 x (Zneg XH)))
(define-fun
  zzpred :keep :source zzpred
  ((x Z)) Z (pred x))
(define-fun
  sub1 :keep :source sub1
  ((x Z) (y Z)) Z (add1 x (opp y)))
(define-fun
  zzsub :keep :source zzsub
  ((x Z) (y Z)) Z (sub1 x y))
(define-fun
  succ0 :keep :source succ0
  ((x Z)) Z (add1 x (Zpos XH)))
(define-fun
  zzsucc :keep :source zzsucc
  ((x Z)) Z (succ0 x))
(define-fun
  zzadd :keep :source zzadd
  ((x Z) (y Z)) Z (add1 x y))
(define-fun-rec
  ggcdn :keep :source ggcdn
  ((x Nat) (y Positive) (z Positive))
  (pair Positive (pair Positive Positive))
  (match x
    ((O (pair2 XH (pair2 y z)))
     ((S n0)
      (match y
        (((XI |a'|)
          (match z
            (((XI |b'|)
              (match (compare |a'| |b'|)
                ((Eq (pair2 (XI |a'|) (pair2 XH XH)))
                 (Lt
                  (match (ggcdn n0 (sub |b'| |a'|) (XI |a'|))
                    (((pair2 g p)
                      (match p
                        (((pair2 ba aa) (pair2 g (pair2 aa (add0 aa (XO ba)))))))))))
                 (Gt
                  (match (ggcdn n0 (sub |a'| |b'|) (XI |b'|))
                    (((pair2 f q)
                      (match q
                        (((pair2 ab bb) (pair2 f (pair2 (add0 bb (XO ab)) bb))))))))))))
             ((XO b0)
              (match (ggcdn n0 (XI |a'|) b0)
                (((pair2 h r)
                  (match r (((pair2 aa2 bb2) (pair2 h (pair2 aa2 (XO bb2))))))))))
             (XH (pair2 XH (pair2 (XI |a'|) XH))))))
         ((XO a0)
          (match z
            (((XI x2)
              (match (ggcdn n0 a0 (XI x2))
                (((pair2 g2 p2)
                  (match p2 (((pair2 aa3 bb3) (pair2 g2 (pair2 (XO aa3) bb3)))))))))
             ((XO b02)
              (match (ggcdn n0 a0 b02) (((pair2 g3 p3) (pair2 (XO g3) p3)))))
             (XH (pair2 XH (pair2 (XO a0) XH))))))
         (XH (pair2 XH (pair2 XH z)))))))))
(define-fun-rec
  mul :keep :source mul
  ((x Positive) (y Positive)) Positive
  (match x
    (((XI p) (add0 y (XO (mul p y))))
     ((XO q) (XO (mul q y)))
     (XH y))))
(define-fun
  mul0 :keep :source mul0
  ((x Z) (y Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos |x'|)
      (match y
        ((Z0 Z0)
         ((Zpos |y'|) (Zpos (mul |x'| |y'|)))
         ((Zneg |y'2|) (Zneg (mul |x'| |y'2|))))))
     ((Zneg |x'2|)
      (match y
        ((Z0 Z0)
         ((Zpos |y'3|) (Zneg (mul |x'2| |y'3|)))
         ((Zneg |y'4|) (Zpos (mul |x'2| |y'4|)))))))))
(define-fun
  pow :keep :source pow
  ((x Z) (y Z)) Z
  (match y
    ((Z0 (Zpos XH))
     ((Zpos p) (iter (lambda ((z Z)) (mul0 x z)) (Zpos XH) p))
     ((Zneg x2) Z0))))
(define-fun
  pow_pos :keep :source pow_pos
  ((x Z) (y Positive)) Z
  (iter (lambda ((z Z)) (mul0 x z)) (Zpos XH) y))
(define-fun
  zzmul :keep :source zzmul
  ((x Z) (y Z)) Z (mul0 x y))
(define-fun-rec
  pos_div_eucl0 :keep :source pos_div_eucl0
  ((x Positive) (y Z)) (pair Z Z)
  (match x
    (((XI |a'|)
      (match (pos_div_eucl0 |a'| y)
        (((pair2 q r)
          (let
            ((|r'|
                (let ((|x'| (XO XH)))
                  (match r
                    ((Z0 (add1 Z0 (Zpos XH)))
                     ((Zpos |y'|) (add1 (Zpos (mul |x'| |y'|)) (Zpos XH)))
                     ((Zneg |y'2|) (add1 (Zneg (mul |x'| |y'2|)) (Zpos XH))))))))
            (ite
              (ltb |r'| y)
              (let ((|x'3| (XO XH)))
                (match q
                  ((Z0 (pair2 Z0 |r'|))
                   ((Zpos |y'|) (pair2 (Zpos (mul |x'3| |y'|)) |r'|))
                   ((Zneg |y'2|) (pair2 (Zneg (mul |x'3| |y'2|)) |r'|)))))
              (let ((|x'2| (XO XH)))
                (match q
                  ((Z0 (pair2 (add1 Z0 (Zpos XH)) (sub1 |r'| y)))
                   ((Zpos |y'|)
                    (pair2 (add1 (Zpos (mul |x'2| |y'|)) (Zpos XH)) (sub1 |r'| y)))
                   ((Zneg |y'2|)
                    (pair2 (add1 (Zneg (mul |x'2| |y'2|)) (Zpos XH))
                      (sub1 |r'| y))))))))))))
     ((XO |a'2|)
      (match (pos_div_eucl0 |a'2| y)
        (((pair2 p r2)
          (let
            ((|r'2|
                (let ((|x'4| (XO XH)))
                  (match r2
                    ((Z0 Z0)
                     ((Zpos |y'|) (Zpos (mul |x'4| |y'|)))
                     ((Zneg |y'2|) (Zneg (mul |x'4| |y'2|))))))))
            (ite
              (ltb |r'2| y)
              (let ((|x'6| (XO XH)))
                (match p
                  ((Z0 (pair2 Z0 |r'2|))
                   ((Zpos |y'|) (pair2 (Zpos (mul |x'6| |y'|)) |r'2|))
                   ((Zneg |y'2|) (pair2 (Zneg (mul |x'6| |y'2|)) |r'2|)))))
              (let ((|x'5| (XO XH)))
                (match p
                  ((Z0 (pair2 (add1 Z0 (Zpos XH)) (sub1 |r'2| y)))
                   ((Zpos |y'|)
                    (pair2 (add1 (Zpos (mul |x'5| |y'|)) (Zpos XH)) (sub1 |r'2| y)))
                   ((Zneg |y'2|)
                    (pair2 (add1 (Zneg (mul |x'5| |y'2|)) (Zpos XH))
                      (sub1 |r'2| y))))))))))))
     (XH
      (match y
        ((_ (pair2 (Zpos XH) Z0))
         ((Zpos |y'3|)
          (match (compare (XO XH) |y'3|)
            ((_ (pair2 Z0 (Zpos XH)))
             (Gt (pair2 (Zpos XH) Z0)))))))))))
(define-fun
  div_eucl :keep :source div_eucl
  ((x Z) (y Z)) (pair Z Z)
  (match x
    ((Z0 (pair2 Z0 Z0))
     ((Zpos |a'|)
      (match y
        ((Z0 (pair2 Z0 Z0))
         ((Zpos z) (pos_div_eucl0 |a'| (Zpos z)))
         ((Zneg |b'|)
          (match (pos_div_eucl0 |a'| (Zpos |b'|))
            (((pair2 q r)
              (match r
                (((Zpos |y'|) (pair2 (opp (add1 q (Zpos XH))) (pos_sub |y'| |b'|)))
                 ((Zneg |y'2|)
                  (pair2 (opp (add1 q (Zpos XH))) (Zneg (add0 |b'| |y'2|))))
                 (Z0 (pair2 (opp q) Z0)))))))))))
     ((Zneg |a'2|)
      (match y
        ((Z0 (pair2 Z0 Z0))
         ((Zpos x2)
          (match (pos_div_eucl0 |a'2| (Zpos x2))
            (((pair2 p r2)
              (match r2
                ((_ (pair2 (opp (add1 p (Zpos XH))) (sub1 (Zpos x2) r2)))
                 (Z0 (pair2 (opp p) Z0))))))))
         ((Zneg |b'2|)
          (match (pos_div_eucl0 |a'2| (Zpos |b'2|))
            (((pair2 q2 r3) (pair2 q2 (opp r3))))))))))))
(define-fun
  div3 :keep :source |div|
  ((x Z) (y Z)) Z (match (div_eucl x y) (((pair2 q z) q))))
(define-fun
  modulo :keep :source modulo
  ((x Z) (y Z)) Z (match (div_eucl x y) (((pair2 z r) r))))
(define-fun
  shiftl :keep :source shiftl
  ((x Z) (y Z)) Z
  (match y
    ((Z0 x)
     ((Zpos p)
      (iter
        (lambda ((z Z))
          (let ((|x'| (XO XH)))
            (match z
              ((Z0 Z0)
               ((Zpos |y'|) (Zpos (mul |x'| |y'|)))
               ((Zneg |y'2|) (Zneg (mul |x'| |y'2|)))))))
        x p))
     ((Zneg q) (iter (lambda ((x2 Z)) (div0 x2)) x q)))))
(define-fun
  shiftr :keep :source shiftr
  ((x Z) (y Z)) Z (shiftl x (opp y)))
(define-fun-rec
  of_uint_acc :keep :source of_uint_acc
  ((x Uint) (y Positive)) Positive
  (match x
    ((Nil y)
     ((D0 l) (of_uint_acc l (mul (XO (XI (XO XH))) y)))
     ((D1 l2) (of_uint_acc l2 (add0 XH (mul (XO (XI (XO XH))) y))))
     ((D2 l3) (of_uint_acc l3 (add0 (XO XH) (mul (XO (XI (XO XH))) y))))
     ((D3 l4) (of_uint_acc l4 (add0 (XI XH) (mul (XO (XI (XO XH))) y))))
     ((D4 l5)
      (of_uint_acc l5 (add0 (XO (XO XH)) (mul (XO (XI (XO XH))) y))))
     ((D5 l6)
      (of_uint_acc l6 (add0 (XI (XO XH)) (mul (XO (XI (XO XH))) y))))
     ((D6 l7)
      (of_uint_acc l7 (add0 (XO (XI XH)) (mul (XO (XI (XO XH))) y))))
     ((D7 l8)
      (of_uint_acc l8 (add0 (XI (XI XH)) (mul (XO (XI (XO XH))) y))))
     ((D8 l9)
      (of_uint_acc l9
        (add0 (XO (XO (XO XH))) (mul (XO (XI (XO XH))) y))))
     ((D9 l10)
      (of_uint_acc l10
        (add0 (XI (XO (XO XH))) (mul (XO (XI (XO XH))) y)))))))
(define-fun-rec
  of_uint :keep :source of_uint
  ((x Uint)) N
  (match x
    ((Nil N0)
     ((D0 l) (of_uint l))
     ((D1 l2) (Npos (of_uint_acc l2 XH)))
     ((D2 l3) (Npos (of_uint_acc l3 (XO XH))))
     ((D3 l4) (Npos (of_uint_acc l4 (XI XH))))
     ((D4 l5) (Npos (of_uint_acc l5 (XO (XO XH)))))
     ((D5 l6) (Npos (of_uint_acc l6 (XI (XO XH)))))
     ((D6 l7) (Npos (of_uint_acc l7 (XO (XI XH)))))
     ((D7 l8) (Npos (of_uint_acc l8 (XI (XI XH)))))
     ((D8 l9) (Npos (of_uint_acc l9 (XO (XO (XO XH))))))
     ((D9 l10) (Npos (of_uint_acc l10 (XI (XO (XO XH)))))))))
(define-fun
  of_uint0 :keep :source of_uint0
  ((x Uint)) Z
  (match (of_uint x)
    ((N0 Z0)
     ((Npos p) (Zpos p)))))
(define-fun
  of_int :keep :source of_int
  ((x Int2)) Z
  (match x
    (((Pos d0) (of_uint0 d0))
     ((Neg d02) (opp (of_uint0 d02))))))
(define-fun-rec
  square :keep :source square
  ((x Positive)) Positive
  (match x
    (((XI p0) (XI (XO (add0 (square p0) p0))))
     ((XO p02) (XO (XO (square p02))))
     (XH XH))))
(define-fun
  square0 :keep :source square0
  ((x Z)) Z
  (match x
    ((Z0 Z0)
     ((Zpos p) (Zpos (square p)))
     ((Zneg q) (Zpos (square q))))))
(define-fun-rec
  add :keep :source add
  ((x Nat) (y Nat)) Nat
  (match x
    ((O y)
     ((S p) (S (add p y))))))
(define-fun
  gcd :keep :source gcd
  ((x Positive) (y Positive)) Positive
  (gcdn (add (size_nat x) (size_nat y)) x y))
(define-fun
  ggcd :keep :source ggcd
  ((x Positive) (y Positive))
  (pair Positive (pair Positive Positive))
  (ggcdn (add (size_nat x) (size_nat y)) x y))
(define-fun
  to_nat :keep :source to_nat
  ((x Positive)) Nat
  (iter_op (lambda ((y Nat)) (lambda ((z Nat)) (add y z))) x (S O)))
(define-fun
  to_nat0 :keep :source to_nat0
  ((x Z)) Nat
  (match x
    ((_ O)
     ((Zpos p) (to_nat p)))))
(define-fun
  abs_nat :keep :source abs_nat
  ((x Z)) Nat
  (match x
    ((Z0 O)
     ((Zpos p) (to_nat p))
     ((Zneg q) (to_nat q)))))
(define-fun
  abs_N :keep :source abs_N
  ((x Z)) N
  (match x
    ((Z0 N0)
     ((Zpos p) (Npos p))
     ((Zneg q) (Npos q)))))
(define-fun
  abs :keep :source abs
  ((x Z)) Z
  (match x
    ((_ x)
     ((Zneg p) (Zpos p)))))
(define-fun
  gcd0 :keep :source gcd0
  ((x Z) (y Z)) Z
  (match x
    ((Z0 (abs y))
     ((Zpos a0)
      (match y
        ((Z0 (abs (Zpos a0)))
         ((Zpos b0) (Zpos (gcd a0 b0)))
         ((Zneg b02) (Zpos (gcd a0 b02))))))
     ((Zneg a02)
      (match y
        ((Z0 (abs (Zneg a02)))
         ((Zpos b03) (Zpos (gcd a02 b03)))
         ((Zneg b04) (Zpos (gcd a02 b04)))))))))
(define-fun
  ggcd0 :keep :source ggcd0
  ((x Z) (y Z)) (pair Z (pair Z Z))
  (match x
    ((Z0 (pair2 (abs y) (pair2 Z0 (sgn y))))
     ((Zpos a0)
      (match y
        ((Z0 (pair2 (abs (Zpos a0)) (pair2 (Zpos XH) Z0)))
         ((Zpos b0)
          (match (ggcd a0 b0)
            (((pair2 g p)
              (match p
                (((pair2 aa bb) (pair2 (Zpos g) (pair2 (Zpos aa) (Zpos bb))))))))))
         ((Zneg b02)
          (match (ggcd a0 b02)
            (((pair2 f q)
              (match q
                (((pair2 aa2 bb2)
                  (pair2 (Zpos f) (pair2 (Zpos aa2) (Zneg bb2)))))))))))))
     ((Zneg a02)
      (match y
        ((Z0 (pair2 (abs (Zneg a02)) (pair2 (Zneg XH) Z0)))
         ((Zpos b03)
          (match (ggcd a02 b03)
            (((pair2 h r)
              (match r
                (((pair2 aa3 bb3)
                  (pair2 (Zpos h) (pair2 (Zneg aa3) (Zpos bb3))))))))))
         ((Zneg b04)
          (match (ggcd a02 b04)
            (((pair2 g2 p2)
              (match p2
                (((pair2 aa4 bb4)
                  (pair2 (Zpos g2) (pair2 (Zneg aa4) (Zneg bb4))))))))))))))))
