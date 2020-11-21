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
  | Virreducible
  | Villtyped of string

and env = value VarMap.t

and t =
  | TBool
  | TInt
  | TVar of string
  | TFun of t * t
  | TPi of string * t * (string -> t)
  | TStar

and kind =
  | Base of t
  | Higher of t

module Term = struct
  type t = term
  let compare = Core.Poly.compare
end
module TermMap = Map.Make(Term)

type pseudocontext = t TermMap.t

let rec string_of_type = function
| TBool -> "Bool"
| TInt -> "Int"
| TVar s -> "'" ^ s
| TFun (t1, t2) -> string_of_type t1 ^ " -> " ^ string_of_type t2
| TPi (x, t1, _) ->
    "Pi " ^ x ^ ": " ^ string_of_type t1 ^ ". " ^ "'t(" ^ x ^ ")"
| TStar -> "*"

let print_type t = string_of_type t |> print_endline

let rec string_of_term = function
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

let print_term t = string_of_term t |> print_endline
