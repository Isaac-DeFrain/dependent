open Type

let rec sub_type_type t x = function
  | TVar v as self -> if v = x then t else self
  | TFun (dom, cod) -> TFun (sub_type_type t x dom, sub_type_type t x cod)
  | TPi (v, v_t, body) as self ->
      if x = v then self
      else TPi (v, sub_type_type t x v_t, sub_type_type t x body)
  | atomic -> atomic

and sub_term_type e x = function
  | TFun (t1, t2) -> TFun (sub_term_type e x t1, sub_term_type e x t2)
  | TPi (v, t, body) as self ->
      if x = v then self
      else TPi (v, sub_term_type e x t, sub_term_type e x body)
  | atomic -> atomic

let rec type_assignment b tenv pseudo = function
  | BLit _ | Bunop (_, _) | Bbiop (_, _, _) -> TBool
  | ILit _ | Iunop (_, _) | Ibiop (_, _, _) -> TInt
  | Var _ as v -> (
      match TermMap.find_opt v pseudo with Some t -> t | None -> TStar)
  | Lam (x, t, body) ->
      let pseudo' = TermMap.add (Var x) t pseudo in
      let t_body = type_assignment b tenv pseudo' body in
      let t_vars = tvars t_body in
      if List.mem x t_vars then TPi (x, t, t_body) else TFun (t, t_body)
  | App (f, g) as app -> (
      let f_t = type_assignment b tenv pseudo f in
      let g_t = type_assignment b tenv pseudo g in
      match f_t with
      | TFun (in_t, out_t) ->
          if type_match b tenv g_t in_t then out_t else TBad app
      | TPi (x, t, body) ->
          if type_match b tenv g_t t then sub_term_type g x body else TBad app
      | _ -> TBad app)

and type_match b tenv t = function
  | TStar -> true
  | TBool -> (
      match t with TVar v -> env_lookup_match b tenv v TBool | _ -> t = TBool)
  | TInt -> (
      match t with TVar v -> env_lookup_match b tenv v TInt | _ -> t = TInt)
  (* TODO: is this what we really want? *)
  | TVar v -> (
      match t with
      | TVar v' -> (
          v' = v
          ||
          match (VarMap.find_opt v tenv, VarMap.find_opt v' tenv) with
          | (None, _) -> b
          | (Some v_t, None) -> type_match b tenv t v_t
          | (Some v_t, Some v'_t) -> type_match b tenv v'_t v_t)
      | _ -> env_lookup_match b tenv v t)
  | TFun (in_t, out_t) as self -> (
      match t with
      | TFun (in_t', out_t') ->
          type_match b tenv in_t in_t' && type_match b tenv out_t' out_t
      | TPi (x, in_t', out_t') when not (List.mem x (tvars out_t')) ->
          type_match b tenv in_t' in_t && type_match b tenv out_t out_t'
      | TVar v as var ->
          var <> in_t && var <> out_t && env_lookup_match b tenv v self
      | _ -> false)
  | TPi (x, t', body) as self -> (
      match t with
      | TFun (in_t, out_t) ->
          type_match b tenv in_t t' && type_match b tenv body out_t
      | TPi (y, t'', body') ->
          type_match b tenv t'' t'
          && type_match b tenv body (sub_term_type (Var x) y body')
      | TVar v -> env_lookup_match b tenv v self
      | _ -> false)
  (* TODO: TApp *)
  | TApp (t1, t2) -> (
      match t with
      | TApp (t1', t2') -> type_match b tenv t1' t1 && type_match b tenv t2 t2'
      | _ -> false)
  | T tm -> type_match b tenv t (type_assignment b tenv TermMap.empty tm)
  | TBad _ -> false

(* type_match_infer = arbitrary match modulo type env *)
(* type_match_derive = only match according to type env *)
and env_lookup_match b tenv v t =
  match VarMap.find_opt v tenv with
  | Some t' -> type_match b tenv t t'
  | None -> ( match t with TVar v' -> v = v' | _ -> b)

(** Assignment of type for inference *)
let type_assign_infer = type_assignment true

let type_assign_infer' = type_assign_infer VarMap.empty TermMap.empty

let type_match_infer = type_match true

let type_match_infer' = type_match_infer VarMap.empty

(** Assignment of type for derivation *)
let type_assign_derive = type_assignment false

let type_assign_derive' = type_assign_derive VarMap.empty TermMap.empty

let type_match_derive = type_match false

let type_match_derive' = type_match_derive VarMap.empty
