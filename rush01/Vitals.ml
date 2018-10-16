  type t = { health : Points.t ;
             energy : Points.t ;
             hygiene : Points.t ;
             happiness : Points.t ; }
  let zero = { health = Points.zero ;
               energy = Points.zero ;
               hygiene = Points.zero ;
               happiness = Points.zero; }
  let all = { health = 100 ;
               energy = 100 ;
               hygiene = 100 ;
               happiness = 100 ; }
  let add a b = { health = Points.add a.health b.health ;
                  energy = Points.add a.energy b.energy ;
                  hygiene = Points.add a.hygiene b.hygiene ;
                  happiness = Points.add a.happiness b.happiness ; }
  let sub a b = { health = Points.sub a.health b.health ;
                  energy = Points.sub a.energy b.energy ;
                  hygiene = Points.sub a.hygiene b.hygiene ;
                  happiness = Points.sub a.happiness b.happiness ; }
let to_string state = "Health{" ^ string_of_int state.health ^
                      "} Energy{" ^ string_of_int state.energy ^
                      "} Hygiene{" ^ string_of_int state.hygiene ^
                      "} Happiness{" ^ string_of_int state.happiness ^ "}"