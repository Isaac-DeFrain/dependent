open Type

let rec type_vars = function
| TFun (t_var, t_body) -> type_vars t_var @ type_vars t_body
| TPi (x, t_var, t_body) -> x :: type_vars t_var @ type_vars (t_body x)
| _ -> []

let rec type_assignment pseudo = function
| BLit _ | Bunop (_, _) | Bbiop (_, _, _) -> TBool
| ILit _ | Iunop (_, _) | Ibiop (_, _, _) -> TInt
| Var _ as v -> TermMap.find v pseudo
| Lam (x, t, body) ->
    let pseudo' = TermMap.add (Var x) t pseudo in
    let t_body = type_assignment pseudo' body in
    let t_vars = type_vars t_body in
    if List.mem x t_vars then TPi (x, t, fun _ -> t_body)
    else TFun (t, t_body)
| _ -> TStar
