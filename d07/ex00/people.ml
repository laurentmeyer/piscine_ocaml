class people name =
  object
    initializer print_endline ("one people named " ^ name ^ " was created")
    val _name : string = name
    val _hp : int = 100
    method to_string = _name ^ " has hp of " ^ string_of_int _hp
    method talk = print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
    method die = print_endline "Aaaarghh!"
  end