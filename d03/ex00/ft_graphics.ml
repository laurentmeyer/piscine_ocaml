type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec main_loop ()=
  main_loop ()

let draw_square x y size = 
  (* Graphics.open_graph (" " ^ (string_of_int x) ^ "x" ^ (string_of_int y)) ; *)
  let up = x + size / 2 in
  let down = x - size / 2 in
  let left = y - size / 2 in
  let right = y + size / 2 in
  Graphics.moveto down left ;
  Graphics.lineto down right ;
  Graphics.lineto up right ;
  Graphics.lineto up left ;
  Graphics.lineto down left

let connect_squares center_left center_right size =
  let (x, y) = center_left in Graphics.moveto (x + size / 2) y ;
  let (x, y) = center_right in Graphics.lineto (x - size / 2) y

let draw_node n x y size =
  draw_square x y size ; 
  Graphics.moveto x y ;
  match n with
  | Nil -> Graphics.draw_string "Nil"
  | Node (i, _, _) -> Graphics.draw_string (string_of_int i)

let rec draw_tree_node_from n x y =
  let size = 60 in
  let x_offset = 100 in
  let y_offset = 50 in
  draw_node n x y size ;
  match n with
  | Nil -> ()
  | Node (i, l, r) -> begin
      let parent_center = (x, y) in
      let left_center = (x + x_offset, y - y_offset) in 
      let right_center = (x + x_offset, y + y_offset) in 
      begin
        let (x, y) = left_center in draw_tree_node_from l x y ;
        let (x, y) = right_center in draw_tree_node_from r x y ;
        connect_squares parent_center left_center size ;
        connect_squares parent_center right_center size
      end
    end

let draw_tree_node n =
  let origin_x = 100 in
  let origin_y = 300 in
  draw_tree_node_from n origin_x origin_y

let () = 
  Graphics.open_graph ("") ;
  draw_tree_node (Node (42, Node (21, Node (21, Nil, Nil), Node (21, Node (21, Nil, Nil), Nil)), Node (21, Nil, Node (21, Nil, Nil))));
  main_loop ()