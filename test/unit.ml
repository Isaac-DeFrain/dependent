open Dependent.Lambda
open Eval
open Type
open Assignment
open Vars
open Utils

let true_lam = Lam ("x", TVar "a", Lam ("y", TVar "a", Var "x"))

let false_lam = Lam ("x", TVar "a", Lam ("y", TVar "a", Var "y"))

(* free_vars/vars *)
let () =
  let v = vars true_lam in
  let fv = free_vars true_lam in
  print_res "true_lam vars test" (v = ["x"; "y"] && fv = [])

(* type_assignment *)
let () =
  let t = type_assignment TermMap.empty true_lam in
  let expect = TFun (TVar "a", TFun (TVar "a", TVar "a")) in
  print_res "true_lam type test" (t = expect)

(* eval *)
let () =
  let app = App (App (true_lam, ILit 1), ILit 2) in
  let expect = Vint 1 in
  print_res "true_lam eval test" (eval VarMap.empty app = expect)

(* reduce/reduce_full *)
let () =
  let app = App (App (true_lam, ILit 1), ILit 2) in
  let expect = App (Lam ("y", TVar "a", ILit 1), ILit 2) in
  print_res "true_lam reduce test" (reduce app = expect)

let () =
  let app = App (App (true_lam, ILit 1), ILit 2) in
  let expect = ILit 1 in
  print_res "true_lam reduce_full test" (reduce_full app = expect)
