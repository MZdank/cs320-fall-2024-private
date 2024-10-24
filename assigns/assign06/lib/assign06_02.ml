open Utils

let parse lst =
  let rec go tok acc =
    match tok with 
    | [] -> (*if tok is empty, make sure it's just 1 expression*)
      (*idk why I need () around these, but it messes up types without them*)
      (match acc with
      | [expr] -> Some expr
      | _ -> None)
    | TNum n :: rest -> (*if number, add it to stack*)
      go rest (Num n :: acc)
    | TAdd :: rest -> (*apply add to last two elements in stack*)
      (match acc with
      | x :: y :: t -> go rest (Add (y, x) :: t)
      | _ -> None)
    | TLt :: rest -> (*apply less than to last two elems*)
      (match acc with
      | x :: y :: t -> go rest (Lt (y, x) :: t)
      | _ -> None)
    | TIte :: rest -> (*if to last 3 elems*)
      (match acc with
      | x :: y :: z :: t -> go rest (Ite (z, y, x) :: t)
      | _ -> None)
  in
  go lst []