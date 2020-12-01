open Type

let rec sub_type_type t x = function
| TVar v as self -> if v = x then t else self
| TFun (dom, cod) ->
    TFun (sub_type_type t x dom , sub_type_type t x cod)
| TPi (v, v_t, body) as self ->
    if x = v then self
    else TPi (v , sub_type_type t x v_t, sub_type_type t x body)
| atomic -> atomic

and sub_term_type e x = function
| TFun (t1, t2) -> TFun (sub_term_type e x t1, sub_term_type e x t2)
| TPi (v, t, body) as self ->
    if x = v then self
    else TPi (v, sub_term_type e x t, sub_term_type e x body)
| atomic -> atomic

let rec type_assignment tenv pseudo = function
| BLit _ | Bunop (_, _) | Bbiop (_, _, _) -> TBool
| ILit _ | Iunop (_, _) | Ibiop (_, _, _) -> TInt
| Var _ as v ->
    begin match TermMap.find_opt v pseudo with
    | Some t -> t
    | None -> TStar
    end
| Lam (x, t, body) ->
    let pseudo' = TermMap.add (Var x) t pseudo in
    let t_body = type_assignment tenv pseudo' body in
    let t_vars = type_vars t_body in
    if List.mem x t_vars then TPi (x, t, t_body)
    else TFun (t, t_body)
| App (f, g) as app ->
    let f_t = type_assignment tenv pseudo f in
    let g_t = type_assignment tenv pseudo g in
    begin match f_t with
    | TFun (in_t, out_t) ->
        if type_match tenv g_t in_t then out_t
        else TBad app
    | TPi (x, t, body) ->
        if type_match tenv g_t t then sub_term_type g x body
        else TBad app
    | _ -> TBad app
    end

and type_match tenv t = function
| TStar -> true
| TBool ->
    begin match t with
    | TVar v -> env_lookup_match tenv v TBool
    | _ -> t = TBool
    end
| TInt ->
    begin match t with
    | TVar v -> env_lookup_match tenv v TInt
    | _ -> t = TInt
    end
| TVar v ->
    begin match t with
    | TVar v' ->
        v' = v ||
        begin match VarMap.find_opt v tenv, VarMap.find_opt v' tenv with
        | None, _ -> true
        | Some v_t, None -> type_match tenv t v_t
        | Some v_t, Some v'_t -> type_match tenv v'_t v_t
        end
    | _ -> env_lookup_match tenv v t
    end
| TFun (in_t, out_t) as self ->
    begin match t with
    | TFun (in_t', out_t') ->
        type_match tenv in_t in_t' && type_match tenv out_t' out_t
    | TPi (x, in_t', out_t') when not (List.mem x (type_vars out_t')) ->
        type_match tenv in_t' in_t && type_match tenv out_t out_t'
    | TVar v as var ->
        var <> in_t && var <> out_t && env_lookup_match tenv v self
    | _ -> false
    end
| TPi (x, t', body) as self->
    begin match t with
    | TFun (in_t, out_t) ->
        type_match tenv in_t t' && type_match tenv body out_t
    | TPi (y, t'', body') ->
        type_match tenv t'' t' && type_match tenv body (sub_term_type (Var x) y body')
    | TVar v -> env_lookup_match tenv v self
    | _ -> false
    end
(* TODO: TApp *)
| TApp (t1, t2) ->
    begin match t with
    | TApp (t1', t2') -> type_match tenv t1' t1 && type_match tenv t2 t2'
    | _ -> false
    end
| T tm -> type_match tenv t (type_assignment tenv TermMap.empty tm)
| TBad _ -> false

(* TODO: distinguish between when type variables should be arbitrarily matched and not *)
and env_lookup_match tenv v t =
  match VarMap.find_opt v tenv with
  | Some t' -> type_match tenv t t'
  | None ->
      begin match t with
      | TVar v' -> v = v'
      | _ -> false
      end

let type_assignment' = type_assignment VarMap.empty TermMap.empty
let type_match' = type_match VarMap.empty

type judgement =
  { pseudo: (term * t) list
  ; term: term
  ; t: t }

let check_derivation judge =
  let pseudo =
    List.fold_left (fun m (k, v) -> TermMap.add k v m) TermMap.empty judge.pseudo
  in
  let term_t = type_assignment VarMap.empty pseudo judge.term in
  (* print_endline (string_of_term judge.term ^ " has type " ^ string_of_type term_t) ;
  print_endline ("Type to match " ^ string_of_type judge.t) ; *)
  type_match' term_t judge.t
