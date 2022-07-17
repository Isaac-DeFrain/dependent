open Printf

let all_tests = ref true

let failed_tests = ref []

let print_res name res =
  all_tests := !all_tests && res ;
  if res then printf "\027[38;5;2m%s\027[0m" (name ^ " test" ^ " PASSED\n")
  else (
    printf "\027[38;5;1m%s\027[0m" (name ^ " test" ^ " FAILED\n") ;
    failed_tests := !failed_tests @ [name])

let rec print_all name =
  let name' = String.uppercase_ascii name in
  if !all_tests then
    printf "\n\027[38;5;2m%s\027[0m" ("ALL " ^ name' ^ " TESTS PASSED")
  else (
    printf
      "\n\027[38;5;1m%s\027[0m\n"
      ("THE FOLLOWING " ^ name' ^ " TEST(S) FAILED:") ;
    print_failed ())

and print_failed () =
  List.iter (printf "\027[38;5;1m- %s\027[0m\n") !failed_tests

let test_group ~name ~desc = printf "*%s*\n  %s\n" name desc
