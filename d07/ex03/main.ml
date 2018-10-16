let () =
  let army = new Army.army (new Dalek.dalek) in
  army#print_army ;
  let army = army#add (new Dalek.dalek) in
  army#print_army ;
  let army = army#add (new Dalek.dalek) in
  army#print_army ;
  let army = army#delete in
  army#print_army ;
  let army = army#delete in
  army#print_army ;
  let army = army#delete in
  army#print_army ;
