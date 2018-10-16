let is_alive (state:Vitals.t) =
  state.health > 0 &&
  state.energy > 0 &&
  state.hygiene > 0 &&
  state.happiness > 0

let rec game_loop state last_tick swap_img =
  if not @@ is_alive state
  then Events.handle_quit ();
  let new_action = Events.handle_events state in 
  let updated_state = Tama.update_state new_action state in
  let new_tick = Sdltimer.get_ticks () in
  Display.update updated_state swap_img;
  if new_tick >= last_tick + 1000
  then
    begin
      let dot_state = Tama.update_state (Time : Actions.t) updated_state in
      game_loop dot_state new_tick (not swap_img)
    end
  else game_loop updated_state last_tick swap_img

let game_init () =
  Sdl.init [`EVERYTHING];
  at_exit Sdl.quit;
  Sdlttf.init ();
  at_exit Sdlttf.quit;
  let state = Tama.get_init_state () in 
  try game_loop state (Sdltimer.get_ticks ()) true with
  | Failure e ->
    Sdl.quit ()

let () =
  game_init ()
