open Type
open Assign

type judgement = { pseudo : (term * t) list; term : term; t : t }

let check_derivation judge =
  let pseudo =
    List.fold_left
      (fun m (k, v) -> TermMap.add k v m)
      TermMap.empty
      judge.pseudo
  in
  let t = type_assign_derive VarMap.empty pseudo judge.term in
  type_match_derive' t judge.t

(* let rec check_derivation judge =
     let pseudo =
       List.fold_left (fun m (k, v) -> TermMap.add k v m) TermMap.empty judge.pseudo
     in
     let t = type_assign_derive VarMap.empty pseudo judge.term in
     let term_str = string_of_term judge.term in
     let t_str, jt_str = string_of_type t, string_of_type judge.t in
     print_endline ("Pseudo: " ^ string_of_alist judge.pseudo) ;
     print_endline ("Infer: " ^ term_str ^ " : " ^ t_str) ;
     print_endline ("Want: " ^ term_str ^ " : " ^ jt_str) ;
     let res = type_match_derive' t judge.t in
     if res then res
     else (print_endline ("Did you mean " ^ term_str ^ " : " ^ t_str ^ " instead of " ^ term_str ^ " : " ^ jt_str ^ "?") ; res)

   and string_of_alist pseudo =
     List.fold_right (fun (k, v) acc ->
       (string_of_term k ^ ": " ^ string_of_type v) :: acc) pseudo []
     |> String.concat ", " *)
