open Type
open Core.Poly

let decl_term_vars = ref ([] : (string * t) list)

let decl_type_vars = ref ([] : string list)

let ( @= ) r x = r := List.sort_uniq compare (!r @ [x])

let uniq = List.sort_uniq String.compare

let rec vars = function
| Var x -> [x]
| Lam (x, _, body) -> uniq (x :: vars body)
| App (f, g) -> uniq (vars f @ vars g)
| _ -> []

let rec free_vars = function
| Var x -> [x]
| Lam (x, _, body) -> free_vars body |> remove x |> uniq
| App (f, g) -> free_vars f @ free_vars g |> uniq
| _ -> []

(* check all type variables are declared *)
let fst (a, _) = a

let check_var v =
  List.map fst !decl_term_vars |> List.mem (fst v)

let check_tvar v = List.mem v !decl_type_vars

let check_tvar_ v =
  if not (check_tvar v) then
  raise (Failure (
    v ^ " is not declared as a type variable\nMaybe you want to add:\n "
    ^ v ^ " : *\nto your pseudocontext"))

let check_tvars vs =
  let fails = ref [] in
  let check_tvar' u =
    try check_tvar_ u with
    Failure s -> fails := s :: !fails
  in
  List.iter check_tvar' vs ;
  let f = !fails in
  f = [] || raise (Failure (String.concat "\n" f))
