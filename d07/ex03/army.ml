class ['a] army (first : 'a) =
  object (self)
    val _army : 'a list = [first]
    method add elt = {< _army = elt :: _army >}
    method delete = {< _army = match _army with
        | head :: tail -> tail
        | [] -> [] >}
    method print_army = List.iter (fun i -> print_string i#to_string ; print_char ' ') _army ; print_newline ()
  end