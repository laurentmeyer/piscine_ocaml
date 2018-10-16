let () =
  List.iter (fun c -> print_string (Color.toString c)) Color.all ;
  print_newline () ;
  List.iter (fun c -> print_endline (Color.toStringVerbose c)) Color.all ;