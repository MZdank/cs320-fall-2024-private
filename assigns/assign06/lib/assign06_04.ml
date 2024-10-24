open Utils

let rec eval e =
  match e with
  | Num n -> VNum n
  | Add (e1, e2) -> (
    let v1 = eval e1 in
    let v2 = eval e2 in
    match v1, v2 with
    | VNum n1, VNum n2 -> VNum (n1 + n2)
    | _ -> VNum (0) (*This should never be used*)
    )
  | Lt (e1, e2) -> (
    let v1 = eval e1 in
    let v2 = eval e2 in
    match v1, v2 with
    | VNum n1, VNum n2 -> VBool (n1 < n2)
    | _ -> VBool (0 < 1) (*unused*)
    )
  | Ite (e1, e2, e3) -> (
    let v1 = eval e1 in
    match v1 with
    | VBool true -> eval e2
    | VBool false -> eval e3
    | _ -> eval e2 (*Never used*)
    )