open Utils

let rec type_of expr = 
  match expr with
  | Num _ -> Some TInt
  | Add (e1, e2) ->
    let t1 = type_of e1 in
    let t2 = type_of e2 in
    if t1 = Some TInt && t2 = Some TInt then 
      Some TInt (*Both are ints*)
    else
      None
  | Lt (e1, e2) ->
    let t1 = type_of e1 in
    let t2 = type_of e2 in
    if t1 = Some TInt && t2 = Some TInt then
      Some TBool (*both are ints*)
    else
      None
  | Ite (e1, e2, e3) ->
    let t1 = type_of e1 in
    let t2 = type_of e2 in
    let t3 = type_of e3 in
    if t1 = Some TBool && t2 = t3 then
      t2 (*e1 is bool, e2 and e3 same type*)
    else
      None