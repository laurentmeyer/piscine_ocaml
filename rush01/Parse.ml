let serialize_state (s : Vitals.t) =
  Printf.sprintf "health%d energy%d hygiene%d happiness%d" s.health s.energy s.hygiene s.happiness

let unserialize_state a b c d : Vitals.t =
  { health = a ;
    energy = b ;
    hygiene = c ;
    happiness = d ; }

let write_state_file state =
  let filepath = "save.itama" in
  let oc = open_out_gen [Open_wronly ; Open_trunc ; Open_creat] 0o600 filepath in
  begin try output_string oc (serialize_state state)
    with
    | Failure str -> print_endline "Error"
  end;
  close_out oc

let read_state_file () =
  let filepath = "save.itama" in
  begin try
      let ic = Scanf.Scanning.from_file filepath in
      let state = Scanf.bscanf ic "health%d energy%d hygiene%d happiness%d" unserialize_state in
      Scanf.Scanning.close_in ic ;
      state
    with
    | e -> Vitals.all
  end