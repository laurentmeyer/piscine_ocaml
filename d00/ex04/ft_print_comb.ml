let ft_print_comb () =
  let rec loop a b c =
    if a < 7 then begin
      if b < 10 then begin
        if c < 10 then begin
          print_int a;
          print_int b;
          print_int c;
          print_string ", ";
          loop a b (c + 1)
        end
        else loop a (b + 1) (b + 2)
      end
      else loop (a + 1) (a + 2) (a + 3)
    end
  in
  loop 0 1 2;
  print_string "789\n"

let main () =
  ft_print_comb ()

let () = main ()