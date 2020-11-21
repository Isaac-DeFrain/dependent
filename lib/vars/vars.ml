open Type

let remove x = List.filter ((<>) x)

let rec vars = function
| Var x -> [x]
| Lam (x, _, body) -> List.sort_uniq compare (x :: vars body)
| App (f, g) -> List.sort_uniq compare (vars f @ vars g)
| _ -> []

let rec free_vars = function
| Var x -> [x]
| Lam (x, _, body) -> free_vars body |> remove x
| App (f, g) -> List.sort_uniq compare (free_vars f @ free_vars g)
| _ -> []
