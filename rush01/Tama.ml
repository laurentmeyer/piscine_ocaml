let get_init_state () : Vitals.t = Parse.read_state_file ()

let update_state (a : Actions.t) (s : Vitals.t) = match a with
  | Eat -> Vitals.add s { health = 25 ; energy = (-10) ; hygiene = (-20) ; happiness = 5 ; }
  | Thunder -> Vitals.add s { health = (-20) ; energy = 25 ; hygiene = 0 ; happiness = (-20) ; }
  | Bath -> Vitals.add s { health = (-20) ; energy = (-10) ; hygiene = 25 ; happiness = 5 ; }
  | Kill -> Vitals.add s { health = (-20) ; energy = (-10) ; hygiene = 0 ; happiness = 20 ; }
  | Time -> Vitals.add s { health = (-1) ; energy = 0 ; hygiene = 0 ; happiness = 0 ; }
  | None -> s