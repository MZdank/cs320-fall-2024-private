open Utils
include My_parser

let rec ty_subst t x ty =
  match ty with
  | TUnit -> TUnit
  | TInt -> TInt
  | TFloat -> TFloat
  | TBool -> TBool
  | TVar y -> if x = y then t else TVar y
  | TList t1 -> TList (ty_subst t x t1)
  | TOption t1 -> TOption (ty_subst t x t1)
  | TPair (t1, t2) -> TPair (ty_subst t x t1, ty_subst t x t2)
  | TFun (t1, t2) -> TFun (ty_subst t x t1, ty_subst t x t2)

let ty_subst_c t x (t1, t2) =
  (ty_subst t x t1, ty_subst t x t2)

let ty_subst_cs t x constraints =
  List.map (ty_subst_c t x) constraints

let rec fvs = function
  | TUnit -> VarSet.empty
  | TInt -> VarSet.empty
  | TFloat -> VarSet.empty
  | TBool -> VarSet.empty
  | TVar x -> VarSet.of_list [x]
  | TList t -> fvs t
  | TOption t -> fvs t
  | TPair (t1, t2) -> VarSet.union (fvs t1) (fvs t2)
  | TFun (t1, t2) -> VarSet.union (fvs t1) (fvs t2)

  let unify init_ty constraints =
    let rec go = function
      | [] -> Some init_ty (* base case *)
      | [TVar "_out", t] -> Some t 
      | (t1, t2) :: cs when t1 = t2 -> go cs
      | (TFun (in1, out1), TFun (in2, out2)) :: cs ->
          go ((in1, in2) :: (out1, out2) :: cs) 
      | (TVar x, t) :: cs ->
          if VarSet.mem x (fvs t) then
            None 
          else
            go (ty_subst_cs t x cs)
      | (t, TVar x) :: cs -> go ((TVar x, t) :: cs)
      | _ -> None 
  in
  match go constraints with
  | Some unified_ty ->
      let free_vars = VarSet.to_list (fvs unified_ty) in
      Some (Forall (free_vars, unified_ty))
  | None -> None
  

let type_of _ _ = assert false

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let eval_expr _ _ = assert false

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
