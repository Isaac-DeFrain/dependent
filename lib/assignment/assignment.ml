(* open Eval *)
open Type

module Term = struct
  type t = Type.term
  let compare = Core.Poly.compare
end
module TermMap = Map.Make(Term)

type pseudocontext = t TermMap.t

let rec type_vars = function
| TFun (x, t_var, t_body) -> x :: type_vars t_var @ type_vars t_body
| TPi (x, t_var, t_body) -> x :: type_vars t_var @ type_vars t_body
| _ -> []

let rec type_assignment pseudo = function
| BLit _ -> TBool
| Bunop (_, _) -> TBool
| Bbiop (_, _, _) -> TBool
| ILit _ -> TInt
| Iunop (_, _) -> TInt
| Ibiop (_, _, _) -> TInt
| Var _ as v -> TermMap.find v pseudo
| Lam (x, t, body) ->
    let pseudo' = TermMap.add (Var x) t pseudo in
    let t_body = type_assignment pseudo' body in
    let t_vars = type_vars t_body in
    if List.mem x t_vars then TPi (x, t, t_body)
    else TFun (x, t, t_body)
| _ -> TStar
