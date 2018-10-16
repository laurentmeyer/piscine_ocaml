let () =
  let print_all v =
    print_string (Value.toStringVerbose v) ;
    print_string (": value: " ^ (string_of_int (Value.toInt v))) ;
    print_string (": shortname: " ^ (Value.toString v)) ;
    (* print_string (": next: " ^ (Value.toString (Value.next v))) ;
    print_string (": previous: " ^ (Value.toString (Value.previous v))) ; *)
    print_newline () in
  let print_next v =
    print_string (Value.toStringVerbose v) ;
    print_string (": value: " ^ (string_of_int (Value.toInt v))) ;
    print_string (": shortname: " ^ (Value.toString v)) ;
    print_string (": next: " ^ (Value.toString (Value.next v))) ;
    print_newline () in
  let print_prev v =
    print_string (Value.toStringVerbose v) ;
    print_string (": value: " ^ (string_of_int (Value.toInt v))) ;
    print_string (": shortname: " ^ (Value.toString v)) ;
    print_string (": prev: " ^ (Value.toString (Value.previous v))) ;
    print_newline () in
  let except_ace = [Value.T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King] in
  let except_2 = [Value.T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As] in
  (* List.iter print_all Value.all ; *)
  (* List.iter print_next except_ace ; *)
  List.iter print_prev except_2 ;
