open Utils
include My_parser

exception AssertFail
exception DivByZero

(*Converting sfexpr to expr*)
let rec desugar_sfexpr sfexpr =
  match sfexpr with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SFun { arg = (arg_name, arg_ty); args; body } ->
    List.fold_right
      (fun (name, ty) acc -> Fun (name, ty, acc))
      ((arg_name, arg_ty) :: args)
      (desugar_sfexpr body)
  | SApp (f, x) -> App (desugar_sfexpr f, desugar_sfexpr x)
  | SLet { is_rec; name; args; ty; value; body } ->
    let desugared_value =
      if args = [] then desugar_sfexpr value
      else
        List.fold_right
          (fun (arg_name, arg_ty) acc -> Fun (arg_name, arg_ty, acc))
          args
          (desugar_sfexpr value)
    in
    Let
      { is_rec; name; ty; value = desugared_value; body = desugar_sfexpr body }
  | SIf (cond, t_branch, f_branch) ->
    If (desugar_sfexpr cond, desugar_sfexpr t_branch, desugar_sfexpr f_branch)
  | SBop (bop, left, right) ->
    Bop (bop, desugar_sfexpr left, desugar_sfexpr right)
  | SAssert expr -> Assert (desugar_sfexpr expr)

let rec desugar prog =
  match prog with
  | [] -> Unit
  | l :: ls ->
    let rest_desugared = desugar ls in
    let { is_rec; name; args; ty; value } = l in
    let desugared_value =
      if args = [] then desugar_sfexpr value
      else
        List.fold_right
          (fun (arg_name, arg_ty) acc -> Fun (arg_name, arg_ty, acc))
          args
          (desugar_sfexpr value)
    in
    Let
      { is_rec; name; ty; value = desugared_value; body = rest_desugared }

let rec type_of ctxt =
  let rec go = function
    | Unit -> Ok UnitTy
    | True -> Ok BoolTy
    | False -> Ok BoolTy
    | Num _ -> Ok IntTy
    | Var x -> (
      match List.assoc_opt x ctxt with
      | Some ty -> Ok ty
      | None -> Error (Utils.UnknownVar x)
      )
    | Fun (x, ty, e) -> (
      match type_of ((x, ty) :: ctxt) e with
      | Ok ty_out -> Ok (FunTy (ty, ty_out))
      | Error err -> Error err
    )
    | Bop (op, e1, e2) -> (
        let t1 = go e1 in
        let t2 = go e2 in
        match op, t1, t2 with
        | Add, Ok IntTy, Ok IntTy -> Ok IntTy
        | Sub, Ok IntTy, Ok IntTy -> Ok IntTy
        | Mul, Ok IntTy, Ok IntTy -> Ok IntTy
        | Div, Ok IntTy, Ok IntTy -> Ok IntTy
        | Mod, Ok IntTy, Ok IntTy -> Ok IntTy
        | Lt, Ok IntTy, Ok IntTy -> Ok BoolTy
        | Lte, Ok IntTy, Ok IntTy -> Ok BoolTy
        | Gt, Ok IntTy, Ok IntTy -> Ok BoolTy
        | Gte, Ok IntTy, Ok IntTy -> Ok BoolTy
        | Eq, Ok IntTy, Ok IntTy -> Ok BoolTy
        | Neq, Ok IntTy, Ok IntTy -> Ok BoolTy
        | And, Ok BoolTy, Ok BoolTy -> Ok BoolTy
        | Or, Ok BoolTy, Ok BoolTy -> Ok BoolTy
        | _, Ok t1, Ok t2 -> (
          match op with
          | Add | Sub | Mul | Div | Mod when t1 <> IntTy -> Error (OpTyErrL (op, IntTy, t1))
          | Add | Sub | Mul | Div | Mod when t2 <> IntTy -> Error (OpTyErrR (op, IntTy, t2))
          | Lt | Lte | Gt | Gte | Eq | Neq when t1 <> IntTy -> Error (OpTyErrL (op, IntTy, t1))
          | Lt | Lte | Gt | Gte | Eq | Neq when t2 <> IntTy -> Error (OpTyErrR (op, IntTy, t2))
          | And | Or when t1 <> BoolTy -> Error (OpTyErrL (op, BoolTy, t1))
          | And | Or when t2 <> BoolTy -> Error (OpTyErrR (op, BoolTy, t2))
          | _ -> Error ParseErr
        )
      | _, Error err, _ -> Error err
      | _, _, Error err -> Error err
    )
    | If (e1, e2, e3) -> (
        match go e1, go e2, go e3 with
        | Ok BoolTy, Ok t2, Ok t3 when t2 = t3 -> Ok t3
        | Ok BoolTy, Ok t2, Ok t3 -> Error (IfTyErr (t2, t3))
        | Ok ty, _, _ -> Error (IfCondTyErr ty)
        | _ -> Error (ParseErr)
    )
    | App (e1, e2) -> (
        match go e1, go e2 with
        | Ok (FunTy (ty_arg, ty_out)), Ok ty when ty = ty_arg -> Ok ty_out
        | Ok (FunTy (ty_arg, _)), Ok ty -> Error (FunArgTyErr (ty_arg, ty))
        | Ok ty, _ -> Error (FunAppTyErr ty)
        | _ -> Error (ParseErr)
      )
      | Let { is_rec; name; ty; value; body } ->(
        if is_rec then
          (* Recursive let bindings *)
          match value with
          | Fun (x, ty_arg, e) ->
              let fun_ty = FunTy (ty_arg, ty) in
              let ctxt' = (name, fun_ty) :: (x, ty_arg) :: ctxt in
              (match type_of ctxt' e with
              | Ok ty_out when ty_out = ty -> type_of ((name, fun_ty) :: ctxt) body
              | _ -> Error (Utils.ParseErr))
          | _ -> Error (Utils.ParseErr)
        else
          (* Non-recursive let bindings *)
          match go value with
          | Ok t1 when t1 = ty -> type_of ((name, ty) :: ctxt) body
          | _ -> Error (Utils.ParseErr)
        )
    | Assert e -> (
        match go e with
        | Ok BoolTy -> Ok UnitTy
        | _ -> Error (Utils.ParseErr)
      )
  in
  go

let type_of expr = type_of [] expr

let rec eval env =
  let rec go = function
    | Unit -> VUnit
    | True -> (VBool true)
    | False -> (VBool false)
    | Var x -> (
      match Env.find_opt x env with
      | Some v -> v
      | None -> raise (Failure ("Unbound variable: " ^ x))
    )
    | Num n -> (VNum n)
    | Fun (str, _, e) ->
        (VClos
           { name = None 
           ; arg = str
           ; body = e
           ; env
           })  
    | Bop (op, e1, e2) -> (
      let m = go e1 in
      let n = go e2 in
      match op, m, n with
      | Div, (VNum x), (VNum y) -> if y <> 0 then (VNum (x / y)) else raise DivByZero
      | Mod, (VNum x), (VNum y) -> if y <> 0 then (VNum (x mod y)) else raise DivByZero
      | Sub, (VNum x), (VNum y) -> (VNum (x - y))
      | Mul, (VNum x), (VNum y) -> (VNum (x * y))
      | Lt, (VNum x), (VNum y) -> (VBool (x < y))
      | Lte, (VNum x), (VNum y) -> (VBool (x <= y))
      | Gt, (VNum x), (VNum y) -> (VBool (x > y))
      | Gte, (VNum x), (VNum y) -> (VBool (x >= y))
      | Eq, (VNum x), (VNum y) -> (VBool (x = y))
      | Neq, (VNum x), (VNum y) -> (VBool (x <> y))
      | And, (VBool x), (VBool y) -> (VBool (x && y))
      | Or, (VBool x), (VBool y) -> (VBool (x || y))
      | Add, (VNum x), (VNum y) -> (VNum (x + y))
      | _ -> raise AssertFail
    )
    | If (e1, e2, e3) -> (
      match go e1 with
      | (VBool true) -> go e2
      | (VBool false) -> go e3
      | _ -> raise AssertFail
    )
    | App (e1, e2) -> (
      match go e1 with
      | VClos { arg; body; env = fun_env; name = None } -> (
          match go e2 with
          | v -> eval (Env.add arg v fun_env) body
        )
      | VClos { arg; body; env = fun_env; name = Some f } -> (
          match go e2 with
          | v ->
              let updated_env =
                Env.add f (VClos { arg; body; env = fun_env; name = Some f }) fun_env
              in
              eval (Env.add arg v updated_env) body
        )
      | _ -> raise AssertFail
    )     
  | Let { is_rec; name; value; body; _ } -> (
    if is_rec then
      let closure =
        match value with
        | Fun (arg, _, body) ->
          (VClos { name = Some name; arg; body; env = env })
        | _ -> raise AssertFail
      in
      begin
        match closure with
        | clos ->
          let rec_env = Env.add name clos env in
          eval rec_env body
      end
    else
      match eval env value with
      | v ->
        let updated_env = Env.add name v env in
        eval updated_env body
    )
  | Assert e -> (
    match go e with
      | VBool true -> VUnit
      | _ -> raise AssertFail
    )
  in go

let eval = eval Env.empty

let interp str =
  match parse str with
  | None -> Error ParseErr
  | Some prog -> 
      let expr = desugar prog in
      match type_of expr with
      | Error e -> Error e
      | Ok ty -> 
          try 
            Ok (eval expr)
          with
          | AssertFail -> Error (AssertTyErr ty)
          | DivByZero -> Error (OpTyErrL (Div, IntTy, IntTy)) (* Adjust as needed *)
