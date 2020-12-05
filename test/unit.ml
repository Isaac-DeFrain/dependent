open Dependent.Lambda
open Eval
open Type
open Assignment.Assign
open Assignment.Derive
open Vars
open Utils

let true_lam = Lam ("x", TVar "a", Lam ("y", TVar "a", Var "x"))

let false_lam = Lam ("x", TVar "a", Lam ("y", TVar "a", Var "y"))

let omega =
  App
    ( Lam ("x", TStar, App (Var "x", Var "x")),
      Lam ("x", TStar, App (Var "x", Var "x")) )

let fun_ab_t = TFun (TVar "a", TVar "b")

let pi_ab_t = TPi ("x", TVar "a", TVar "b")

(** free_vars/vars *)
let () =
  test_group ~name:"free_vars/vars" ~desc:"term should have expected varaibles"

let () =
  let v = vars true_lam in
  let fv = free_vars true_lam in
  print_res "true_lam vars" (v = ["x"; "y"] && fv = []) ;
  let v = tvars fun_ab_t in
  print_res "fun_ab_t vars" (v = ["a"; "b"]) ;
  let v = tvars pi_ab_t in
  print_res "pi_ab_t vars" (v = ["a"; "b"; "x"])

(** type inference *)
let () =
  test_group ~name:"type_assignment" ~desc:"term should have the expected type"

let () =
  let t = type_assign_infer' true_lam in
  let expect = TFun (TVar "a", TFun (TVar "a", TVar "a")) in
  print_res "true_lam type" (t = expect) ;
  let pseudo = TermMap.singleton (Var "x") (TFun (TBool, TInt)) in
  let t = type_assign_infer VarMap.empty pseudo (App (Var "x", BLit true)) in
  let expect = TInt in
  print_res "pseudo_lookup type1" (t = expect) ;
  let t = type_assign_infer VarMap.empty pseudo (App (Var "x", ILit 42)) in
  let expect = TBad (App (Var "x", ILit 42)) in
  print_res "pseudo_lookup type2" (t = expect)

(* eval *)
let () =
  test_group ~name:"eval" ~desc:"term should evaluate to the expected value"

let () =
  let app = App (App (true_lam, ILit 1), ILit 2) in
  let expect = Vint 1 in
  print_res "true_lam eval" (eval' app = expect) ;
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
  print_res "true_lam reduce" (reduce' app = expect) ;
  print_res "omega reduce" (reduce' omega = omega) ;
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
  print_res "sub_type2" (sub_ab = expect2) ;
  let ( = ) a b = compare a b = 0 in
  let sub_a = sub_type_type TBool "a" pi_ab_t in
  let expect1 = TPi ("x", TBool, TVar "b") in
  let sub_ab = sub_type_type TInt "b" sub_a in
  let expect2 = TPi ("x", TBool, TInt) in
  print_res "sub_type3" (sub_a = expect1) ;
  print_res "sub_type4" (sub_ab = expect2)

(* type_match *)
let () =
  test_group
    ~name:"type_match_infer"
    ~desc:"inferred types should match the given types"

let () =
  let expect = TFun (TVar "a", TInt) in
  print_res "type_match1" (not (type_match_infer' TBool TInt)) ;
  print_res "type_match2" (type_match_infer' TBool TStar) ;
  print_res "type_match3" (type_match_infer' TBool TBool) ;
  print_res "type_match4" (type_match_infer' (TVar "a") (TVar "b")) ;
  print_res "type_match5" (type_match_infer' fun_ab_t TStar) ;
  print_res "type_match6" (type_match_infer' expect fun_ab_t) ;
  let open VarMap in
  let tenv = singleton "b" TInt in
  print_res "type_match7" (type_match_infer tenv fun_ab_t expect) ;
  let tenv = singleton "a" TInt |> add "b" TBool in
  let expect = TFun (TInt, TBool) in
  print_res "type_match8" (type_match_infer tenv fun_ab_t expect)

let () =
  test_group
    ~name:"type_match_derive"
    ~desc:"derived types should match the given types"

let () =
  let expect = TFun (TVar "a", TInt) in
  print_res "type_match1" (not (type_match_derive' TBool TInt)) ;
  print_res "type_match2" (type_match_derive' TBool TStar) ;
  print_res "type_match3" (type_match_derive' TBool TBool) ;
  print_res "type_match4" (type_match_derive' fun_ab_t TStar) ;
  print_res "type_match5" (not (type_match_derive' (TVar "a") (TVar "b"))) ;
  let open VarMap in
  let tenv = singleton "b" TInt in
  print_res "type_match6" (type_match_derive tenv fun_ab_t expect) ;
  let tenv = singleton "a" TInt |> add "b" TBool in
  let expect = TFun (TInt, TBool) in
  print_res "type_match7" (type_match_derive tenv fun_ab_t expect)

(** Derivations *)
let () =
  test_group
    ~name:"derivations"
    ~desc:"derivations should be valid in given pseudocontext"

let () =
  let pseudo = [(Var "x", TVar "a")] in
  let term = Var "x" in
  let t = TVar "a" in
  print_res "derivation1" (check_derivation { pseudo; term; t }) ;
  let t = TVar "b" in
  print_res "derivation2" (not (check_derivation { pseudo; term; t })) ;
  let pseudo = [(Var "t", TStar); (Var "x", TVar "t")] in
  let term = App (Lam ("x", TVar "t", Var "x"), Var "x") in
  print_res "derivation3" (check_derivation { pseudo; term; t = TVar "t" }) ;
  let term = App (Lam ("x", TVar "t", Var "x"), Var "x") in
  print_res
    "derivation4"
    (not (check_derivation { pseudo; term; t = TVar "s" }))

let () = print_all "unit"
