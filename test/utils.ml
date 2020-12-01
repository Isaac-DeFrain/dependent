let all_tests = ref true

let print_res name res =
  all_tests := !all_tests && res ;
  if res then
    Printf.printf "\027[38;5;2m%s\027[0m" (name ^ " test" ^ " PASSED\n")
  else Printf.printf "\027[38;5;1m%s\027[0m" (name ^ " test" ^ " FAILED\n")

let print_all name =
  let name' = String.uppercase_ascii name in
  if !all_tests then
    Printf.printf "\n\027[38;5;2m%s\027[0m" ("ALL " ^ name' ^ " TESTS PASSED")
  else
    Printf.printf "\n\027[38;5;1m%s\027[0m" ("SOME " ^ name' ^ " TEST(S) FAILED")

let test_group ~name ~desc = Printf.printf "*%s*\n  %s\n" name desc
