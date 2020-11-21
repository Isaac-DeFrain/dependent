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
  | TFun of string * t * t
  | TPi of string * t * t
  | TStar

and kind =
  | Base of t
  | Higher of t

let rec to_string = function
| TBool -> "Bool"
| TInt -> "Int"
| TFun (_, t1, t2) -> to_string t1 ^ " -> " ^ to_string t2
| TPi (x, t1, t2) -> "Pi " ^ x ^ ": " ^ to_string t1 ^ ". " ^ to_string t2
| TStar -> "*"

let print t = to_string t |> print_endline
