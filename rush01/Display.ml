open Sdlvideo

let add_background screen img_path =
  begin try
      let image = Sdlloader.load_image img_path in
      blit_surface ~dst_rect:(rect 0 0 0 0) ~src:image ~dst:screen ()
    with
    | _	-> print_endline "error opening bg"; exit (-1)
  end

let add_image screen img_path =
  begin try
      let image = Sdlloader.load_image img_path in
      fill_rect screen (map_RGB screen ((230, 230, 230):color));
      blit_surface ~dst_rect:(rect 205 239 0 0) ~src:image ~dst:screen ()
    with
    | _	-> print_endline "error opening img"; exit (-1)
  end

let add_rectangle screen rect =
  begin try
      fill_rect ~rect:rect screen @@ map_RGB screen ((70, 120, 120):color);
    with
    | _	-> print_endline "error filling bg"; exit (-1)
  end

let add_text screen position str =
  begin try
      let font = Sdlttf.open_font "resources/Roboto-Black.ttf" 20 in
      let text = Sdlttf.render_text_blended font str ~fg:Sdlvideo.black in
      blit_surface ~dst_rect:position ~src:text ~dst:screen ()
    with e -> print_endline @@ Printexc.to_string e; exit (-1)
  end

let update_tama_idle screen img_state =
  if img_state = true
  then add_image screen "resources/idle_2.png"
  else add_image screen "resources/idle_1.png"

let update (state:Vitals.t) img_state =
  let screen = set_video_mode 600 800 [`DOUBLEBUF] in
  update_tama_idle screen img_state;
  add_text screen (rect 10 50 0 0) @@ "Health: " ^ string_of_int state.health;
  add_text screen (rect 150 50 0 0) @@ "Energy: " ^ string_of_int state.energy;
  add_text screen (rect 280 50 0 0) @@ "Hygiene: " ^ string_of_int state.hygiene;
  add_text screen (rect 440 50 0 0) @@ "Happyness: " ^ string_of_int state.happiness;

  add_rectangle screen (rect 10 625 130 50);
  add_text screen (rect 60 638 0 0) "EAT";
  add_rectangle screen (rect 160 625 130 50);
  add_text screen (rect 180 638 0 0) "THUNDER";
  add_rectangle screen (rect 310 625 130 50);
  add_text screen (rect 345 638 0 0) "BATH";
  add_rectangle screen (rect 460 625 130 50);
  add_text screen (rect 505 638 0 0) "KILL";
  flip screen

let game_over () =
  let screen = set_video_mode 600 800 [`DOUBLEBUF] in
  add_background screen "resources/bg.jpg";
  flip screen


