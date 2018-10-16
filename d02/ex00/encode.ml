let increment_head_tuple l =
  match l with
  | [] -> []
  | head :: tail -> let (a, b) = head in (a + 1, b) :: tail

let rec encode l = match l with
  | head :: next :: tail when head = next -> increment_head_tuple (encode (next :: tail))
  | head :: tail -> [(1, head)] @ (encode tail)
  | _ -> []


let () =
  let print_tuple (a, b) = print_int a ; print_char b in
  let print_encode sequence =
    begin
      List.iter print_tuple (encode sequence);
      print_newline ()
    end
  in
  begin
    print_encode [];
    print_encode ['A'];
    print_encode ['A'; 'A'; 'A'; 'B'; 'B'; 'B'];
    print_encode ['A'; 'A'; 'A'; 'B'; 'B'; 'C'; 'B'; 'A'; 'A']
  end