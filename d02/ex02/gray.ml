let reverse l =
  let rec reverse_aux original reversed =
    match original with
    | [] -> reversed
    | head :: tail  -> reverse_aux tail (head :: reversed)
  in reverse_aux l []

let prepend l str =
  let rec prepend_aux reversed prepended =
    match reversed with
    | [] -> prepended
    | head :: tail -> prepend_aux tail ((str ^ head) :: prepended)
  in
  prepend_aux (reverse l) []

let concat a b =
  let rec concat_aux emptied filled  =
    match emptied with
    | [] -> filled
    | head :: tail  -> concat_aux tail (head :: filled)
  in concat_aux (reverse a) b

let rec make_gray n =
  match n with
  | _ when n <= 0 -> []
  | 1 -> ["0"; "1"]
  | _ -> let previous = (make_gray (n - 1)) in
      concat (prepend previous "0") (prepend (reverse previous) "1")

let rec print_list l =
  match l with
  | head :: [] -> print_string head
  | head :: tail -> print_string head ; print_char ' ' ; print_list tail
  | [] -> ()

let gray n = print_list (make_gray n)

let () = gray 7