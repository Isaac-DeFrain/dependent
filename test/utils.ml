let print_res name res =
  if res then
    Printf.sprintf "\027[38;5;2m%s\027[0m" (name ^ " test" ^ " PASSED")
    |> Printf.printf "%s\n"
  else
    Printf.sprintf "\027[38;5;1m%s\027[0m" (name ^ " test" ^ " FAILED")
    |> Printf.printf "%s\n"

let test_group ~name ~desc = Printf.printf "*%s*\n  %s\n" name desc
