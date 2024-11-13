open Utils
include My_parser

let rec subst x n m =
  match m with
  | Num _ -> m  
  | Var y -> if x = y then n else m  
  | Unit -> Unit  
  | True -> True  
  | False -> False 
  | Bop (op, m1, m2) -> 
      let m1' = subst x n m1 in
      let m2' = subst x n m2 in
      Bop (op, m1', m2')
  | If (m1, m2, m3) -> 
      let m1' = subst x n m1 in
      let m2' = subst x n m2 in
      let m3' = subst x n m3 in
      If (m1', m2', m3')
  | Fun (y, m0) ->  
      if x = y then m 
      else Fun (y, subst x n m0)
  | App (m1, m2) -> 
      let m1' = subst x n m1 in
      let m2' = subst x n m2 in
      App (m1', m2')
  | Let (y, m1, m2) ->  
      let m1' = subst x n m1 in
      let m2' = if x = y then m2 else subst x n m2 in 
      Let (y, m1', m2')
  
let rec eval env expr =
  let rec go = function
    | True -> Ok (VBool true)
    | False -> Ok (VBool false)
    | Num n -> Ok (VNum n)
    | Fun (x, e) -> Ok (VFun (x, e))
    | Bop (op, e1, e2) -> (
      let m = go e1 in
      let n = go e2 in
      match op, m, n with
      | Add, Ok (VNum x), Ok (VNum y) -> Ok (VNum (x + y))
      | Sub, Ok (VNum x), Ok (VNum y) -> Ok (VNum (x - y))
      | Mul, Ok (VNum x), Ok (VNum y) -> Ok (VNum (x * y))
      | Div, Ok (VNum x), Ok (VNum y) -> if y <> 0 then Ok (VNum (x / y)) else Error DivByZero
      | Mod, Ok (VNum x), Ok (VNum y) -> if y <> 0 then Ok (VNum (x mod y)) else Error DivByZero
      | Lt, Ok (VNum x), Ok (VNum y) -> Ok (VBool (x < y))
      | Lte, Ok (VNum x), Ok (VNum y) -> Ok (VBool (x <= y))
      | Gt, Ok (VNum x), Ok (VNum y) -> Ok (VBool (x > y))
      | Gte, Ok (VNum x), Ok (VNum y) -> Ok (VBool (x >= y))
      | Eq, Ok (VNum x), Ok (VNum y) -> Ok (VBool (x = y))
      | Neq, Ok (VNum x), Ok (VNum y) -> Ok (VBool (x <> y))
      | And, Ok (VBool x), Ok (VBool y) -> Ok (VBool (x && y))
      | Or, Ok (VBool x), Ok (VBool y) -> Ok (VBool (x || y))
      | _ -> Error InvalidIfCond )
    | If (op, e2, e3) -> (
      match go op with
      | Ok (VBool true) -> go e2
      | Ok (VBool false) -> go e3
      | _ -> Error (InvalidIfCond)
    )
    | Var x -> (
      match Env.find_opt x env with
      | Some v -> Ok v
      | None -> Error (UnknownVar x)
    )
    | Let (x, e1, e2) -> (
      match go e1 with
      | Ok v -> eval (Env.add x v env) e2
      | Error e -> Error e
    )
    | App (e1, e2) -> (
      match go e1 with
      | Ok (VFun (x, body)) -> (
        match go e2 with
        | Ok v -> eval (Env.add x v env) body
        | _ -> Error (InvalidApp)
      )
      | _ -> Error (InvalidApp)
    )
    | Unit -> Ok VUnit
  in go expr

let eval = eval Env.empty

let interp str =
  match parse str with
  | Some expr -> eval expr
  | None -> Error (ParseFail)