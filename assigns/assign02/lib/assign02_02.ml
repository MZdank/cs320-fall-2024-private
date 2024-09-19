type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

(*This is stolen from the class lib*)
let take n l =
  let[@tail_mod_cons] rec aux n l =
    match n, l with
    | 0, _ | _, [] -> []
    | n, x::l -> x::aux (n - 1) l
  in
  (*if n < 0 then invalid_arg "List.take";*)
  aux n l

(*This is also stolen from the class lib*)
let drop n l =
  let rec aux i = function
    | _x::l when i < n -> aux (i + 1) l
    | rest -> rest
  in
  (*if n < 0 then invalid_arg "List.drop";*)
  aux 0 l

let mk_matrix lst (r, c) =
  let rec build lst r =
    match c with
    | 0 -> []
    | _ ->   
      match r with
      | 0 -> []
      | _ ->
        let row = take c lst in
        let rest = drop c lst in
        row :: build rest (r-1)
      in
      { entries = build lst r; rows = r; cols = c }
        