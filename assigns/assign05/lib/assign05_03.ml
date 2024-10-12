type ident = string

type ty = 
| Unit
| Arr of ty * ty

type expr = 
| Var of ident
| Fun of ident * ty * expr
| App of expr * expr

let rec in_context gamma x =
  match gamma with
  | [] -> None (*means we searched all of gamma*)
  | (y, ty) :: rest -> 
    if x = y then 
      Some ty (*return type if match*)
    else 
      in_context rest x (*keep searching*)

let rec type_of gamma e =
  match e with
  | Var x -> in_context gamma x
  | Fun (x, ty1, e) -> type_of_fun gamma x ty1 e
  | App (e1, e2) -> type_of_app gamma e1 e2

(*Take context, fun name, type input, body*)
and type_of_fun gamma x ty1 e =
  let gamma' = (x, ty1) :: gamma in (*add x to context*)
  match type_of gamma' e with
  | Some ty2 -> Some (Arr (ty1, ty2)) (*if body has type, return it*)
  | None -> None

and type_of_app gamma e1 e2 =
  match type_of gamma e1 with
  | None -> None
  | Some Unit -> None 
  | Some (Arr (ty2, ty)) -> (*make sure e1 is a fun*)
    match type_of gamma e2 with
    | Some ty2' ->
      if ty2 = ty2' then (*if type of e1 is expected type*)
        Some ty
      else
        None 
    | _ -> None