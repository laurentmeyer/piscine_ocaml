class dalek =
  object (self)
    val _name : string = Random.self_init ();
      let lowercase = String.init 3 (fun x -> char_of_int (Random.int 26 + int_of_char 'a')) in
      "Dalek" ^ String.capitalize_ascii lowercase
    val _hp : int = 100
    val mutable _shield : bool = true
    method to_string = _name
                       ^ " with hp of " ^ string_of_int _hp ^ " and "
                       ^ if _shield then "shield" else "no shield"
    method talk =
      let sentences = [|"Explain! Explain!"; " Exterminate! Exterminate!"; "I obey!"; "You are the Doctor! You are the enemy of the Daleks!"|] in
      let index = Random.self_init () ; Random.int 4 in
      print_endline sentences.(index)
    method exterminate (p : People.people) = _shield <- not _shield ; p#die
    method die = print_endline "Emergency Temporal Shift!"

  end