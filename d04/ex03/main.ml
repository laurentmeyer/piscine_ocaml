let () =
  let deck = Deck.newDeck () in
  let rec drawAll d =
    let (card, remaining) = Deck.drawCard d in
    print_endline (Deck.Card.toStringVerbose card) ;
    drawAll remaining in
  List.iter print_endline (Deck.toStringListVerbose deck);
  List.iter print_endline (Deck.toStringList deck) ;
  drawAll deck
