let ft_print_alphabet () =
  let rec loop character =
    let ascii_of_z = int_of_char 'z' in
    let ascii_of_current = int_of_char character in
    if ascii_of_current <= ascii_of_z
    then
      begin
        print_char character;
        loop (char_of_int (ascii_of_current + 1))
      end
  in
  loop 'a';
  print_char '\n'


let () = ft_print_alphabet ()