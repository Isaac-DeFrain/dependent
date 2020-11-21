let print_res name res =
  if res then
    Printf.sprintf "\027[38;5;2m%s\027[0m" (name ^ " PASSED")
    |> Printf.printf "%s\n"
  else
    Printf.sprintf "\027[38;5;1m%s\027[0m" (name ^ " FAILED")
    |> Printf.printf "%s\n"
