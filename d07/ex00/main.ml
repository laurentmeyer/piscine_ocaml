let () =
  let person = new People.people "zob" in
  print_endline (person#to_string) ;
  (new People.people "Jean-Jean")#talk ;
  person#die