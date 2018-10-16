let () =
  let dalek = new Dalek.dalek in
  let human = new People.people "Jean-Fion" in 
  let doctor = new Doctor.doctor "Dre" 42 human in 
  print_endline (dalek#to_string) ;
  dalek#talk ;
  dalek#exterminate human ;
  print_endline (dalek#to_string) ;
  doctor#use_sonic_screwdriver ;
  dalek#die ;
