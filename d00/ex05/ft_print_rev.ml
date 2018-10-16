let ft_print_rev str =
  let rec loop pos =
    if pos < 0
    then print_char '\n'
    else
      let c = String.get str pos in
      begin
        print_char c;
        loop (pos - 1)
      end
  in
  loop ((String.length str) - 1)

let () =
  print_string "KaYaK: ";
  ft_print_rev "KaYaK";
  print_string ": ";
  ft_print_rev "";
  print_string "Hello, world!: ";
  ft_print_rev "Hello, world!"