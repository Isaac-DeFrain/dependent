open Dependent.Lambda
open Eval
open Type
open Assignment
open Vars
open Utils

let true_lam = Lam ("x", TVar "a", Lam ("y", TVar "a", Var "x"))

let false_lam = Lam ("x", TVar "a", Lam ("y", TVar "a", Var "y"))

let omega =
  App
    ( Lam ("x", TStar, App (Var "x", Var "x")),
      Lam ("x", TStar, App (Var "x", Var "x")) )

let fun_ab_t = TFun (TVar "a", TVar "b")

let pi_ab_t = TPi ("x", TVar "a", fun _ -> TVar "b")

(* free_vars/vars *)
let () =
  test_group ~name:"free_vars/vars" ~desc:"term should have expected varaibles"

let () =
  let v = vars true_lam in
  let fv = free_vars true_lam in
  print_res "true_lam vars" (v = ["x"; "y"] && fv = [])

(* type_assignment *)
let () =
  test_group ~name:"type_assignment" ~desc:"term should have the expected type"

let () =
  let t = type_assignment' true_lam in
  let expect = TFun (TVar "a", TFun (TVar "a", TVar "a")) in
  print_res "true_lam type" (t = expect)

(* eval *)
let () =
  test_group ~name:"eval" ~desc:"term should evaluate to the expected value"

let () =
  let app = App (App (true_lam, ILit 1), ILit 2) in
  let expect = Vint 1 in
  print_res "true_lam eval" (eval' app = expect)

let () =
  let value = eval' omega in
  print_res
    "omega eval"
    (value = Villtyped "The type of \\x: *. (x x)  does not match *")

(* reduce/reduce_full *)
let () =
  test_group ~name:"reduce/reduce_full" ~desc:"should match the given types"

let () =
  let app = App (App (true_lam, ILit 1), ILit 2) in
  let expect = App (Lam ("y", TVar "a", ILit 1), ILit 2) in
  print_res "true_lam reduce" (reduce' app = expect)

let () = print_res "omega reduce" (reduce' omega = omega)

let () =
  let app = App (App (true_lam, ILit 1), ILit 2) in
  let expect = ILit 1 in
  print_res "true_lam reduce_full" (reduce_full' app = expect)

(* sub_type *)
let () =
  test_group
    ~name:"sub_type"
    ~desc:"substitution should give the expected result"

let () =
  let sub_a = sub_type_type TBool "a" fun_ab_t in
  let expect1 = TFun (TBool, TVar "b") in
  let sub_ab = sub_type_type TInt "b" sub_a in
  let expect2 = TFun (TBool, TInt) in
  print_res "sub_type1" (sub_a = expect1) ;
  print_res "sub_type2" (sub_ab = expect2)

let () =
  let ( = ) a b = compare a b = 0 in
  let sub_a = sub_type_type TBool "a" pi_ab_t in
  let expect1 = TPi ("x", TBool, fun _ -> TVar "b") in
  let sub_ab = sub_type_type TInt "b" sub_a in
  let expect2 = TPi ("x", TBool, fun _ -> TInt) in
  print_res "sub_type3" (sub_a = expect1) ;
  print_res "sub_type4" (sub_ab = expect2)

(* type_match *)
let () = test_group ~name:"type_match" ~desc:"should match the given types"

let () =
  let expect = TFun (TVar "a", TInt) in
  print_res "type_match1" (type_match' TBool TStar) ;
  print_res "type_match2" (type_match' TBool TBool) ;
  print_res "type_match3" (type_match' fun_ab_t TStar) ;
  print_res "type_match4" (type_match' fun_ab_t expect)
