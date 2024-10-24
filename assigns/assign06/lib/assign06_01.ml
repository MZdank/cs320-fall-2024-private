open Utils

let lex s =
  let lst = split s in (*remove whitespace*)
  let rec go lst toks =
    match lst with 
    | [] -> Some (List.rev toks) (*Build it backwards, reverse*)
    | h :: t ->
      let token = tok_of_string_opt h in
      match token with
      | Some token -> go t (token :: toks) (*if token, add to list*)
      | None -> None
  in
  go lst []