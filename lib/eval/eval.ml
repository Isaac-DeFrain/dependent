open Assignment.Assign
open Type

let rec sub e x = function
  | Var v as y -> if v = x then e else y
  | Lam (v, t, body) as lam -> if v = x then lam else Lam (v, t, sub e x body)
  | App (f, g) -> App (sub e x f, sub e x g)
  | atomic -> atomic

let rec eval venv tenv = function
  | Var x -> VarMap.find x venv
  | Lam (_, _, _) as lam -> Vclosure (reduce tenv lam, venv)
  | App (Lam (x, t, body), g) -> (
      let pseudo = TermMap.singleton (Var x) t in
      let g_t = type_assign_infer tenv pseudo g in
      let venv' = VarMap.add x (eval venv tenv g) venv in
      match t with
      | TVar v -> (
          match VarMap.find_opt v tenv with
          | Some v_t ->
              if v_t = g_t then eval venv' tenv body else illtyped g v_t
          | None -> eval venv' tenv body)
      | _ -> if t = g_t then eval venv' tenv body else illtyped g t)
  | App (_, _) as app ->
      let app' = reduce tenv app in
      if app' = app then Virreducible app else eval venv tenv app'
  | BLit b -> Vbool b
  | Bunop (unop, b) -> Vbool (unop b)
  | Bbiop (biop, a, b) -> Vbool (biop a b)
  | ILit i -> Vint i
  | Iunop (unop, i) -> Vint (unop i)
  | Ibiop (biop, i, j) -> Vint (biop i j)

and reduce tenv = function
  | Var _ as v -> v
  | Lam (x, t, body) -> Lam (x, t, reduce tenv body)
  | App (Lam (x, t, body), g) as self -> (
      let pseudo = TermMap.singleton (Var x) t in
      match t with
      | TVar _ -> sub g x body
      | _ -> if t = type_assign_infer tenv pseudo g then sub g x body else self)
  | App (f, g) -> App (reduce tenv f, reduce tenv g)
  | Bunop (unop, b) -> BLit (unop b)
  | Bbiop (biop, a, b) -> BLit (biop a b)
  | Iunop (unop, i) -> ILit (unop i)
  | Ibiop (biop, i, j) -> ILit (biop i j)
  | lit -> lit

and reduce_full tenv = function
  | Var _ as v -> v
  | Lam (x, t, body) -> Lam (x, t, reduce_full tenv body)
  | App (Lam (x, t, body), g) as self -> (
      let pseudo = TermMap.singleton (Var x) t in
      match t with
      | TVar _ -> reduce_full tenv (sub g x body)
      | _ ->
          if t = type_assign_infer tenv pseudo g then
            reduce_full tenv (sub g x body)
          else self)
  | App (f, g) -> (
      match reduce tenv f with
      | Lam (_, _, _) as lam -> reduce_full tenv (App (lam, g))
      | _ -> App (reduce_full tenv f, reduce_full tenv g))
  | Bunop (unop, b) -> BLit (unop b)
  | Bbiop (biop, a, b) -> BLit (biop a b)
  | Iunop (unop, i) -> ILit (unop i)
  | Ibiop (biop, i, j) -> ILit (biop i j)
  | lit -> lit

and illtyped term t =
  Villtyped
    (String.concat
       " "
       ["The type of"; string_of_term term; " does not match"; string_of_type t])

let eval' = eval VarMap.empty VarMap.empty

let reduce' = reduce VarMap.empty

let reduce_full' = reduce_full VarMap.empty
