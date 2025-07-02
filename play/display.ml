open! Core
open Async
open Game_strategies_common_lib

let square_size = 50

let init game_kind =
  let size = Game_kind.board_length game_kind * square_size in
  Graphics.open_graph (Printf.sprintf " %dx%d" (size + 200) (size + 50));
  Graphics.set_window_title
    (match game_kind with Tic_tac_toe -> "Tic Tac Toe" | Omok -> "Omok")

let draw_x x y size =
  let x = x - (size / 2) in
  let y = y - (size / 2) in
  Graphics.draw_segments
    (Array.of_list [ (x, y, x + size, y + size); (x, y + size, x + size, y) ])

let draw_header state =
  let x_name, o_name =
    match state with
    | State.Game_in_progress { x_name; o_name; _ }
    | Game_over { x_name; o_name; _ } ->
        (x_name, o_name)
  in
  Graphics.set_text_size 18;
  Graphics.moveto 10 (Graphics.size_y () - 20);
  Graphics.set_color Graphics.red;
  Graphics.draw_string [%string "X: %{x_name}"];
  Graphics.moveto 10 (Graphics.size_y () - 40);
  Graphics.set_color Graphics.blue;
  Graphics.draw_string [%string "O: %{o_name}"];
  match state with
  | State.Game_in_progress _ -> ()
  | Game_over { reason; _ } -> (
      let message_x_offset = 150 in
      Graphics.moveto message_x_offset (Graphics.size_y () - 20);
      Graphics.set_color Graphics.black;
      match reason with
      | Draw -> Graphics.draw_string "It's a draw!"
      | Winner piece ->
          let name = match piece with X -> x_name | O -> o_name in
          Graphics.draw_string
            [%string "Winner: %{Piece.to_string piece} (%{name})"]
      | Player_timed_out piece ->
          Graphics.draw_string [%string "Player %{piece#Piece} timed out!"]
      | Error_requesting_move { piece; error } ->
          Graphics.draw_string
            [%string
              "Error requesting move for %{piece#Piece}: \
               %{Error.to_string_mach error}"]
      | Illegal_move { piece; error } -> (
          Graphics.draw_string [%string "Illegal move by %{piece#Piece}"];
          Graphics.moveto message_x_offset (Graphics.size_y () - 30);
          match error with
          | Out_of_bounds -> Graphics.draw_string "Position is out of bounds."
          | Occupied_space piece ->
              Graphics.draw_string
                [%string
                  "Position is already occupied by %{Piece.to_string piece}."]))

let render state =
  Graphics.display_mode false;
  (* Clear the screen *)
  Graphics.clear_graph ();
  (* Draw the header *)
  draw_header state;
  (* Draw the grid *)
  let indices =
    List.init (Game_kind.board_length (State.game state).game_kind) ~f:Fn.id
  in
  Graphics.set_color (Graphics.rgb 128 128 128);
  Graphics.set_line_width 3;
  List.cartesian_product indices indices
  |> List.iter ~f:(fun (x, y) ->
         Graphics.draw_rect (x * square_size) (y * square_size) square_size
           square_size);
  (* Draw pieces *)
  Graphics.set_line_width 5;
  let scale x factor = Float.to_int (Float.of_int x *. factor) in
  Map.iteri (State.game state).board
    ~f:(fun ~key:{ Position.row; column } ~data:piece ->
      match (piece : Piece.t) with
      | X ->
          Graphics.set_color Graphics.red;
          draw_x
            ((row * square_size) + (square_size / 2))
            ((column * square_size) + (square_size / 2))
            (scale square_size 0.6)
      | O ->
          Graphics.set_color Graphics.blue;
          Graphics.draw_circle
            ((row * square_size) + (square_size / 2))
            ((column * square_size) + (square_size / 2))
            (scale square_size 0.3));
  Graphics.display_mode true;
  Graphics.synchronize ()
