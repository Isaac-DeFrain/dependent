module VarMap = Map.Make(String)

type term =
  | Var of string
  | Lam of string * t * term
  | App of term * term
  | BLit of bool
  | Bunop of (bool -> bool) * bool
  | Bbiop of (bool -> bool -> bool) * bool * bool
  | ILit of int
  | Iunop of (int -> int) * int
  | Ibiop of (int -> int -> int) * int * int

and value =
  | Vbool of bool
  | Vint of int
  | Vclosure of term * env
  | Virreducible of term
  | Villtyped of string

and env = value VarMap.t

and t =
  | TBool
  | TInt
  | TVar of string
  | TFun of t * t
  | TPi of string * t * t
  | TStar
  | TBad of term
  | TApp of t * t
  | T of term

module Term = struct
  type t = term
  let compare = Core.Poly.compare
end
module TermMap = Map.Make(Term)

type pseudo = t TermMap.t

let rec string_of_type = function
| TBool -> "Bool"
| TInt -> "Int"
| TVar s -> s
| TFun (t1, t2) -> string_of_type t1 ^ " -> " ^ string_of_type t2
| TPi (x, t1, t2) ->
    "Pi " ^ x ^ ": " ^ string_of_type t1 ^ ". " ^ string_of_type t2
| TStar -> "*"
| TBad term -> "Type error: " ^ string_of_term term
| TApp (t1, t2) -> string_of_type t1 ^ " " ^ string_of_type t2
| T term -> string_of_term term

and string_of_term = function
| Var v -> v
| Lam (x, t, body) ->
    "\\" ^ x ^ ": " ^ string_of_type t ^ ". " ^ string_of_term body
| App (f, g) -> "(" ^ string_of_term f ^ " " ^ string_of_term g ^ ")"
| BLit b -> string_of_bool b
| Bunop (unop, b) -> string_of_bool (unop b)
| Bbiop (biop, a, b) -> string_of_bool (biop a b)
| ILit i -> string_of_int i
| Iunop (unop, i) -> string_of_int (unop i)
| Ibiop (biop, i, j) -> string_of_int (biop i j)

let rec tvars = function
| TVar x -> [x]
| TFun (t1, t2) -> tvars t1 @ tvars t2 |> sort
| TPi (x, t, body) -> x :: tvars t @ tvars body |> sort
| TApp (t1, t2) -> tvars t1 @ tvars t2 |> sort
| _ -> []

and free_tvars =function
| TVar x -> [x]
| TFun (t1, t2) -> free_tvars t1 @ free_tvars t2 |> sort
| TPi (x, t, body) -> free_tvars t @ (free_tvars body |> remove x) |> sort
| TApp (t1, t2) -> free_tvars t1 @ free_tvars t2 |> sort
| _ -> []

and sort = List.sort_uniq String.compare

and remove x = List.filter ((<>) x)
let print_type t = string_of_type t |> print_endline

let print_term t = string_of_term t |> print_endline

let rec string_of_value = function
| Vbool b -> string_of_bool b
| Vint i -> string_of_int i
| Vclosure (f, env) ->
    "< " ^ string_of_term f ^ ", {" ^ string_of_env env ^ "} >"
| Virreducible t -> "Irreducible: " ^ string_of_term t
| Villtyped msg -> "Type error: " ^ msg

and string_of_env env =
  VarMap.bindings env
  |> List.fold_left (fun acc (k, v) ->
      acc @ [ k ^ ": " ^ string_of_value v ]) []
  |> String.concat ", "

let print_value v = string_of_value v |> print_endline
