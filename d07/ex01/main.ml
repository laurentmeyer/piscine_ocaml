let () =
  let doctor = new Doctor.doctor "Maboule" 42 (new People.people "Jean-Mich'") in
  print_endline (doctor#to_string) ;
  doctor#talk ;
  doctor#travel_in_time 1942 2018 ;
  doctor#use_sonic_screwdriver ;
  print_endline (doctor#regenerate_exposed#to_string) ;