open Graphics

type block_color = Cyan | Yellow | Purple | Green | Red | Blue | Orange
type shape_type = I | O | T | S | Z | J | L

type piece = {
  shape : shape_type;
  color : block_color;
  blocks : (int * int) list;
  x : int;
  y : int;
}

(* Game Board *)
type board = (int * int * block_color) list

type game_state = {
  current_piece : piece;
  settled_board : board;
  score : int;
  is_game_over : bool;
}

let blocks_of_shape = function
  | O -> [(0,0); (1,0); (0,1); (1,1)]
  | I -> [(-1,0); (0,0); (1,0); (2,0)]
  | T -> [(-1,0); (0,0); (1,0); (0,1)]
  | S -> [(-1,0); (0,0); (0,1); (1,1)]
  | Z -> [(-1,1); (0,1); (0,0); (1,0)]
  | J -> [(-1,0); (0,0); (1,0); (-1,1)]
  | L -> [(-1,0); (0,0); (1,0); (1,1)]

let color_to_graphics = function
  | Cyan   -> rgb 0 240 240
  | Yellow -> rgb 240 240 0
  | Purple -> rgb 160 0 240
  | Green  -> rgb 0 240 0
  | Red    -> rgb 240 0 0
  | Blue   -> rgb 0 0 240
  | Orange -> rgb 240 160 0


(* ---------------------------------------------------------------------------------------------------------------------------------------- *)

let rotate_piece piece =
   { piece with blocks = List.map (fun (x, y) -> (-y, x)) piece.blocks}


(* Generates a new random piece *)
let make_random_piece () = 
  let shape, color = 
    match Random.int 7 with
    | 0 -> (I, Cyan)
    | 1 -> (O, Yellow)
    | 2 -> (T, Purple)
    | 3 -> (S, Green)
    | 4 -> (Z, Red)
    | 5 -> (J, Blue)
    | _ -> (L, Orange)
  in
  { shape; color; blocks = blocks_of_shape shape; x = 4; y = 18}


let get_level score = (score / 500) + 1

let get_drop_speed score =
  let level = get_level score in
  (* Starts at 0.5 seconds. Subtracts 0.05s per level*)
  max 0.05 (0.55 -. (float_of_int level *. 0.05))


(* Collision detection *)
let is_valid_position piece board =
  List.for_all (fun (dx, dy) -> 
    let abs_x = piece.x + dx in
    let abs_y = piece.y + dy in
    abs_x >= 0 && abs_x < 10 && abs_y >= 0 &&
    not (List.exists (fun (bx, by, _) -> bx = abs_x && by = abs_y) board)
    ) piece.blocks


(* Line Clearing *)

let blocks_in_row board y =
  List.filter(fun (_, by, _) -> by = y) board |> List.length

let find_full_rows board =
  let rec check_row y acc = 
    if y > 19 then acc
    else if blocks_in_row board y = 10 then check_row (y + 1) (y :: acc)
    else check_row (y + 1) acc
  in
  check_row 0 []

let clear_lines board =
  let full_rows = find_full_rows board in
  let num_cleard = List.length full_rows in

  if num_cleard = 0 then (board, 0)
  else
    let remaining_blocks = List.filter (fun (_, by, _) ->
      not (List.mem by full_rows)
      ) board 
    in

    let shifted_board = List.map (fun (bx, by, col) ->
      let rows_below = List.filter (fun ry -> ry < by) full_rows |> List.length in(bx, by - rows_below, col)
      ) remaining_blocks
    in

    let points = num_cleard * 100 in
    (shifted_board, points)


(* Moves current piece down. If it can't, it merges into the board and spawns a new piece *)
let tick_gravity state = 
  let p = state.current_piece in
  let down_piece = { p with y = p.y - 1 } in
  if is_valid_position down_piece state.settled_board then
    { state with current_piece = down_piece }
  else
    (* Check Collision *)
    let new_board = List.fold_left (fun acc (dx, dy) ->
      (p.x + dx, p.y + dy, p.color) :: acc
      ) state.settled_board p.blocks
    in

    (* Clear Lines *)
    let updated_board, points = clear_lines new_board in
    let next_piece = make_random_piece () in

    let game_over_triggered = not (is_valid_position next_piece updated_board) in

    { 
      settled_board = updated_board;
      current_piece = next_piece;
      score = state.score + points;
      is_game_over = game_over_triggered;
    }

let reset_game () =
  {
    current_piece = make_random_piece ();
    settled_board = [];
    score = 0;
    is_game_over = false;
  }

let handle_input state key = 
  if state.is_game_over then state
  else

  let p = state.current_piece in
  let next_piece = 
    match key with
    | 'a' -> { p with x = p.x - 1 }
    | 'd' -> { p with x = p.x + 1 }
    | 's' -> { p with y = p.y - 1 } (* Soft drop *)
    | 'q' -> rotate_piece p
    | 'e' -> rotate_piece (rotate_piece (rotate_piece p))
    | _ -> p
  in
  if is_valid_position next_piece state.settled_board then
    { state with current_piece = next_piece }
  else
    state



(* ---------------------------------------------------------------------------------------------------------------------------------------- *)


(* Rendering *)

let block_size = 25
let board_offset_x = 75
let board_offset_y = 50




(* Setup restart button *)
let btn_x = board_offset_x + 60
let btn_y = board_offset_y + 160
let btn_w = 130
let btn_h = 30

let is_clicking_button mx my =
  mx >= btn_x && mx <= (btn_x + btn_w) && my >= btn_y && my <= (btn_y + btn_h)

(* Draws a single block at a specific grid position *)
let draw_block grid_x grid_y color =
  let screen_x = board_offset_x + (grid_x * block_size) in
  let screen_y = board_offset_y + (grid_y * block_size) in
  set_color (color_to_graphics color);
  fill_rect screen_x screen_y (block_size - 1) (block_size - 1)




(* Draws one frame of a game state *)
let draw_game state =
  clear_graph ();

  (* Draw background (10 colls and 20 rows) *)
  set_color (rgb 30 30 30);
  fill_rect board_offset_x board_offset_y (10 * block_size) (20 * block_size);

  (* Draw grid lines *)
  set_color (rgb 50 50 50);
  for col = 0 to 10 do
    moveto (board_offset_x + col * block_size) board_offset_y;
    lineto (board_offset_x + col * block_size) (board_offset_y + 20 * block_size)
  done;

  for row = 0 to 20 do 
    moveto board_offset_x (board_offset_y + row * block_size);
    lineto (board_offset_x + 10 * block_size) (board_offset_y + row * block_size)
  done;

  (* Draw the current board *)
  List.iter (fun (bx, by, col) -> draw_block bx by col) state.settled_board;

  (* Draw the active falling piece *)
  let p = state.current_piece in
  List.iter (fun (dx, dy) -> draw_block (p.x + dx) (p.y + dy) p.color) p.blocks;

  (* Score *)
  set_color black;
  moveto board_offset_x (board_offset_y + (20 * block_size) + 15);
  draw_string ("Score: " ^ string_of_int state.score);

  let current_level = get_level state.score in
  moveto (board_offset_x + 140) (board_offset_y + (20 * block_size) + 15);
  draw_string ("Level: " ^ string_of_int current_level);

  if state.is_game_over then begin
    set_color (rgb 0 0 0);
    fill_rect board_offset_x (board_offset_y + 200) (10 * block_size) 60;

    set_color red;
    moveto (board_offset_x + 55) (board_offset_y + 222);
    draw_string "G A M E    O V E R";

    set_color (rgb 50 150 50);
    fill_rect btn_x btn_y btn_w btn_h;

    set_color white;
    moveto (btn_x + btn_w/4) (btn_y + btn_h/4);
    draw_string "Restart";
  end;

  synchronize ()


(* ---------------------------------------------------------------------------------------------------------------------------------------- *)

  (* Game loop *)

let rec game_loop state tick =

  let mx, my = mouse_pos () in
  let is_clicked = button_down () in

  (* Check and handle game over*)

  if state.is_game_over then begin

    if is_clicked && is_clicking_button mx my then begin
      (* Flush input queue *)
      while key_pressed () do
        ignore (read_key ())
      done;
      let fresh_state = reset_game () in
      draw_game fresh_state;
      Unix.sleepf 0.016;
      game_loop fresh_state (Unix.gettimeofday ())
      
    end else begin
      draw_game state;
      Unix.sleepf 0.016;
      game_loop state tick
    end

  end else begin

    (* Core Loop *)

    let current_time = Unix.gettimeofday () in

    let current_speed = get_drop_speed state.score in

    let state, new_tick = 
      if current_time -. tick > current_speed then
        (tick_gravity state, current_time)
      else
        (state, tick)
    in

    let state = 
      if key_pressed () then
        handle_input state (read_key ())
      else
        state
    in

    draw_game state;
    Unix.sleepf 0.016;
    game_loop state new_tick
  end


