let () =
  let jokes = [|
  "C'est quoi deux trous dans un trou ? C'est ton nez dans mon cul" ;
  "Tu connais la différence entre du pastis et du pastus ? le pastis a un bon gout d'anis" ;
  "Monsieur et Madame Tallu ont quatre fils: Jean, Jean, Jean et Jean. Les quatre Jean Tallu" ;
  "C'est quoi un canif ? c'est un petit fien" ;
  "Ma femme compile le C"
  |] in
  Random.self_init () ;
  let i = Random.int 5 in
  print_endline jokes.(i)