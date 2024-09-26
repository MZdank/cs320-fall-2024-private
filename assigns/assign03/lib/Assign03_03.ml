type tree = 
  | Leaf of int
  | Node of tree list

let rec findterminal t =
  match t with
  | Leaf _ -> [t] (*If it's a leaf node it's the terminal*)
  | Node [] -> [t] (*If no kids then terminal*)
  | Node cs -> List.concat_map findterminal cs

let rec collapse h t =
  match t with
  | Leaf _ -> t
  | Node cs ->
    if h = 1 then
      let terminals = List.concat_map findterminal cs in
      Node terminals
    else
      let collapsed = List.map (collapse (h - 1)) cs in
      Node collapsed
