(* open Eval *)
open Type

module Term = struct
  type t = Type.term
  let compare = Core.Poly.compare
end
module TermMap = Map.Make(Term)

type pseudocontext = t TermMap.t

let rec type_vars = function
| TFun (t_var, t_body) -> type_vars t_var @ type_vars t_body
| TPi (t_var, t_body) -> type_vars t_var @ type_vars t_body
| _ -> []

let rec type_assignment pseudo = function
| BLit _ -> TBool
| ILit _ -> TInt
| Var _ as v -> TermMap.find v pseudo
| Lam (x, t, body) ->
    let pseudo' = TermMap.add (Var x) t pseudo in
    let t_body = type_assignment pseudo' body in
    TPi (t, t_body)
| _ -> TStar
