let rec is_in_list a l =
  match l with
    | [] -> false
    | head :: tail -> head = a || is_in_list a tail
  
let rec crossover a b =
  match a with
    | head :: tail when is_in_list head b -> head :: crossover tail b
    | head :: tail -> crossover tail b
    | _ -> []





let () =
  let print_common_int a b =
    let print_int_space i = begin print_int i ; print_char ' ' end in
    begin
      List.iter print_int_space (crossover a b);
      print_newline ()
    end
  in
  let print_common_str a b =
    let print_str_space s = print_string s ; print_char ' ' in
    begin
      List.iter print_str_space (crossover a b);
      print_newline ()
    end
  in
  begin
    print_common_int [] [3; 4; 5];
    print_common_int [3; 4; 5] [];
    print_common_int [1; 2; 3] [3; 4; 5];
    print_common_int [3; 4; 5] [1; 2; 3];
    print_common_int [1; 2; 3; 5; 6] [3; 4; 5; 6; 7];
    print_common_str ["toto"; "tata"; "teuteu"] ["toutou"; "tata"; "toto"];
  end