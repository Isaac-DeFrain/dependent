open Assignment
open Type

let rec sub e1 x e2 =
  match e2 with
  | Var v as y-> if v = x then e1 else y
  | Lam (v, t, body) as lam -> if v = x then lam else Lam (v, t, sub e1 x body)
  | App (f, g) -> App (sub e1 x f, sub e1 x g)
  | x -> x

let rec eval env = function
| Var x -> VarMap.find x env
| Lam (_, _, _) as lam -> Vclosure (reduce lam, env)
| App (Lam (x, t, body), g) ->
    let pseudo = TermMap.singleton (Var x) t in
    let g_t = type_assignment pseudo g in
    let env' = VarMap.add x (eval env g) env in
    begin match t with
    | TVar _ -> eval env' body
    | _ ->
        if t = g_t then eval env' body
        else Villtyped ("The type of " ^ string_of_term g ^ " does not match " ^ string_of_type t)
    end
| App (_, _) as app ->
    let app' = reduce app in
    if app' = app then Virreducible
    else eval env app'
| BLit b -> Vbool b
| Bunop (unop, b) -> Vbool (unop b)
| Bbiop (biop, a, b) -> Vbool (biop a b)
| ILit i -> Vint i
| Iunop (unop, i) -> Vint (unop i)
| Ibiop (biop, i, j) -> Vint (biop i j)

and reduce = function
| Var _ as v -> v
| Lam (x, t, body) -> Lam (x, t, reduce body)
| App (Lam (x, t, body), g) as self ->
    let pseudo = TermMap.singleton (Var x) t in
    begin match t with
    | TVar _ -> sub g x body
    | _ ->
        if t = type_assignment pseudo g
        then sub g x body
        else self
    end
| App (f, g) -> App (reduce f, reduce g)
| Bunop (unop, b) -> BLit (unop b)
| Bbiop (biop, a, b) -> BLit (biop a b)
| Iunop (unop, i) -> ILit (unop i)
| Ibiop (biop, i, j) -> ILit (biop i j)
| lit -> lit

and reduce_full = function
| Var _ as v -> v
| Lam (x, t, body) -> Lam (x, t, reduce_full body)
| App (Lam (x, t, body), g) as self ->
    let pseudo = TermMap.singleton (Var x) t in
    begin match t with
    | TVar _ -> reduce_full (sub g x body)
    | _ ->
        if t = type_assignment pseudo g
        then reduce_full (sub g x body)
        else self
    end
| App (f, g) ->
    begin match reduce f with
    | Lam (_, _, _) as lam -> reduce_full (App (lam, g))
    | _ -> App (reduce_full f, reduce_full g)
    end
| Bunop (unop, b) -> BLit (unop b)
| Bbiop (biop, a, b) -> BLit (biop a b)
| Iunop (unop, i) -> ILit (unop i)
| Ibiop (biop, i, j) -> ILit (biop i j)
| lit -> lit
