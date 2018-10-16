type state = {
  tree: Tree.tree;
  turn: Tree.sign;
  name_O: string;
  name_X: string;
  winner: Tree.sign;
  size: int;
  depth: int
}
  
type action =
  Move of Tree.position
  

let makeMove (state : state) ((row, col) : Tree.position) : Tree.tree =
  let tree = Tree.setSign state.tree row col state.size state.depth state.turn in
  Tree.updateWon state.turn state.size tree

let reducerTree state action = 
  match action with
  | Move position -> makeMove state position

let toggleTurn currentSign =
  if currentSign = Tree.O then Tree.X else Tree.O

let reducerTurn state action =   
  match action with
  | Move _ -> toggleTurn state.turn

let reducer state action = 
  {
    tree = reducerTree state action;
    turn = reducerTurn state action;
    name_O = state.name_O;
    name_X = state.name_X;
    winner = state.winner;
    size = state.size;
    depth = state.depth
  }

let rec getSize () =
  print_string "Enter the size: ";
  let input = read_line () in
  let invalidSize () =
    print_endline "Invalid size.";
    getSize() 
  in
  match int_of_string_opt input with
  | None -> invalidSize ()
  | Some size when size < 0 -> invalidSize ()
  | Some size -> size

let rec getDepth () =
  print_string "Enter the depth: ";
  let input = read_line () in
  let invalidDepth () =
    print_endline "Invalid depth.";
    getDepth() 
  in
  match int_of_string_opt input with
  | None -> invalidDepth ()
  | Some depth when depth < 0 -> invalidDepth ()
  | Some depth -> depth

let rec getName sign =
  print_string ("Enter the name for " ^ sign ^ " : ");
  read_line ()
  
let get_init_state () =
  let size = getSize () in
  let depth = getDepth () in
  let name_O = getName "O" in
  let name_X = getName "X" in
  let tree = Tree.newTree size depth in
  {
    tree = tree ;
    turn = Tree.O ;
    name_O = name_O ;
    name_X = name_X ;
    winner = Undefined;
    size = size;
    depth = depth
  }

let nameOfSign (state : state) (s : Tree.sign) : string =
  if s = Tree.O then state.name_O else state.name_X

let stringToPosition str =
  let split = String.split_on_char ' ' str in
  match split with
  | x :: y :: [] -> Ok (int_of_string_opt x, int_of_string_opt y)
  | _ -> Error "Incorrect format"

let isBounded row col state =
  let max = Utils.int_power state.size state.depth in    
  row >= 0 && row < max && col >= 0 && col < max

let isUndefined row col state =
  (Tree.getSign state.tree row col state.size state.depth) = Tree.Undefined
  

let isValidPosition position state = match position with
  | Ok (Some row, Some col) -> 
    let row = row - 1 in
    let col = col - 1 in
    if (isBounded row col state && isUndefined row col state) then Ok (row, col)
    else Error "Illegal move"
  | _ -> Error "Incorrect format"

let rec getPosition state =
  let input = read_line () in
  let position = stringToPosition input in
  match isValidPosition position state with
  | Ok (row, col) -> (row, col)
  | Error message -> print_endline message; getPosition state

let checkWinner state =
  Tree.getWon state.turn state.size state.tree

let newGame () =
  print_string "Do you want to restart the game? [Y/n]";
  let input = read_line () in
  (input = "y" || input = "Y")

let rec main_loop state =
  Tree.printGrid state.tree state.size state.depth ;
  print_endline (nameOfSign state state.turn ^ "'s turn to play") ;
  let position = getPosition state in
  let state = reducer state (Move position) in
  match checkWinner state with
  | Tree.Undefined -> main_loop state
  | s -> begin
    Tree.printGrid state.tree state.size state.depth ; 
    print_endline ("Player " ^ (nameOfSign state s) ^ " wins the game\n") ;
    if newGame () then main_loop (get_init_state ())
  end

let () =
  let init_state = get_init_state () in
  main_loop init_state
