type 'a test = 
 | TestCase of 'a
 | TestList of 'a test list

(*This is basically the same code as lecture 9*)
let rec fold_left_list op acc l =
  match l with
  | [] -> acc
  | h::t -> fold_left_list op (op acc h) t


let rec fold_left op acc test = 
  match test with
  (*Just a testcase, apply op to acc and testcase *)
  | TestCase t -> op acc t
  (*List of tests, apply fold left to all in list*)
  | TestList tests -> fold_left_list (fold_left op) acc tests 

