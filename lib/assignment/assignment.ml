open Type

let rec type_vars = function
| TVar v -> [v]
| TFun (t_var, t_body) -> type_vars t_var @ type_vars t_body
| TPi (x, t_var, t_body) -> x :: type_vars t_var @ type_vars (t_body x)
| _ -> []

let rec sub_type_type t x = function
| TVar v as self -> if v = x then t else self
| TFun (dom, cod) ->
    TFun (sub_type_type t x dom , sub_type_type t x cod)
| TPi (v, v_t, body) as self ->
    if x = v then self
    else TPi (v , sub_type_type t x v_t, fun s -> sub_type_type t x (body s))
| atomic -> atomic

and sub_term_type e x = function
| TPi (v, t, body) as self ->
    if x = v then self
    else TPi (v, sub_term_type e x t, fun s -> sub_term_type e x (body s))
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
    if List.mem x t_vars then TPi (x, t, fun _ -> t_body)
    else TFun (t, t_body)
| App (f, g) as app ->
    let f_t = type_assignment tenv pseudo f in
    let g_t = type_assignment tenv pseudo g in
    begin match f_t with
    | TFun (in_t, out_t) ->
        if type_match tenv g_t in_t then out_t
        else TBad app
    | TPi (x, t, body) ->
        if type_match tenv g_t t then sub_term_type g x (body x)
        else TBad app
    | _ -> TBad app
    end

and type_match tenv t = function
| TStar -> true
| TBool -> t = TBool
| TInt -> t = TInt
| TVar v ->
    begin match t with
    | TVar v' ->
        v' = v ||
        begin match VarMap.find_opt v tenv, VarMap.find_opt v' tenv with
        | None, _ -> true
        | Some v_t, None -> type_match tenv t v_t
        | Some v_t, Some v'_t -> type_match tenv v'_t v_t
        end
    | _ ->
        begin match VarMap.find_opt v tenv with
        | Some t' -> type_match tenv t t'
        | None -> true
        end
    end
| TFun (in_t, out_t) ->
    begin match t with
    | TFun (in_t', out_t') ->
        type_match tenv in_t' in_t && type_match tenv out_t out_t'
    | _ -> false
    end
| TPi (x, t', body) ->
    begin match t with
    | TFun (in_t, out_t) ->
        type_match tenv in_t t' && type_match tenv (body x) out_t
    | TPi (y, t'', body') ->
        type_match tenv t'' t' && type_match tenv (body x) (body' y)
    | _ -> false
    end
| TBad _ -> false

let type_assignment' = type_assignment VarMap.empty TermMap.empty
let type_match' = type_match VarMap.empty
