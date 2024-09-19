type dir =
  | North
  | South
  | East
  | West 

type path = dir list

(*let is_close f1 f2 = abs_float (f1 -. f2) <.000001   *)

(*let _ = assert (is_close (dist [North; North; South; East]) (sqrt 2.)) *)

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

let rec make_move path updown leftright =
  match path with
  | [] -> (updown, leftright)
  | _ ->
    let move = take 1 path in
    let _ = drop 1 path in
    match move with
      | [North] -> make_move path (updown + 1) leftright
      | [South] -> make_move path (updown - 1) leftright
      | [East] -> make_move path updown (leftright + 1)
      | [West] -> make_move path updown (leftright - 1)
      | _ -> (updown, leftright)

let dist path =
  let (updown, leftright) = make_move path 0 0 in
  sqrt (float_of_int ((updown) * (updown) + (leftright) * (leftright)))