type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(*Not doing this*)
let sum_tr _ = 12345