let sum a b =
  a +. b

let () =
  print_endline ("4.2 + 1.2 = " ^ string_of_float (sum 4.2 1.2))