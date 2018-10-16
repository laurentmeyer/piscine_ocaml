(* type t = Eat | Thunder | Bath | Kill | Time *)
open Sdlevent
open Sdlkey

let is_EAT_button x y : bool =
  x >= 10 && x <= (10 + 130) && y >= 625 && y <= 675

let is_THUNDER_button x y : bool =
  x >= 160 && x <= (160 + 130) && y >= 625 && y <= 675

let is_BATH_button x y : bool =
  x >= 310 && x <= (310 + 130) && y >= 625 && y <= 675

let is_KILL_button x y : bool =
  x >= 460 && x <= (460 + 130) && y >= 625 && y <= 675

let get_action x y =
  if is_EAT_button x y then (Eat: Actions.t)
  else if is_THUNDER_button x y then (Thunder: Actions.t)
  else if is_BATH_button x y then (Bath: Actions.t)
  else if is_KILL_button x y then (Kill: Actions.t)
  else (None: Actions.t)


let rec handle_events state =
  match poll () with
  | None -> (None : Actions.t)
  | Some QUIT ->
    begin
      Parse.write_state_file state ;
      ignore(raise (Failure "quit")) ;
      (None : Actions.t)
    end
  | Some MOUSEBUTTONDOWN {mbe_x = x ; mbe_y = y} -> get_action x y
  | Some event -> (None : Actions.t)

let rec handle_quit () =
  Display.game_over ();
  match poll () with
  | Some QUIT ->
    Parse.write_state_file Vitals.all ;
    ignore(raise (Failure "quit"))
  | _ -> handle_quit ()