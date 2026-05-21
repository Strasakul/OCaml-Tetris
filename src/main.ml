open Graphics

let () =
  Random.self_init ();

  open_graph " 400x600";
  set_window_title "OCaml Tetris";
  auto_synchronize false;

  let initial_state : Game.game_state = {
    current_piece = Game.make_random_piece ();
    settled_board = [];
    score = 0;
    is_game_over = false;
  } in

  try Game.game_loop initial_state (Unix.gettimeofday ())
  with Graphic_failure _ -> print_endline "Game closed."

