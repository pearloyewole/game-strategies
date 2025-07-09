open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]
let find_board_length (game: Game.t) : int  = 
  Game_kind.board_length (game.game_kind) 

let print_game (game : Game.t) =
  let print_row row =
    let cells =
      List.init (find_board_length game) ~f:(fun col ->
          let pos = { Position.row; column = col } in
          match Map.find game.board pos with
          | Some X -> "X"
          | Some O -> "O"
          | None -> " ")
    in
    let cell_strings = String.concat ~sep:" | " cells in
    print_endline cell_strings
  in
  (**put into a list then print*)
  print_row 0;
  print_endline "---------";
  print_row 1;
  print_endline "---------";
  print_row 2

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()



let get_all_positions (game: Game.t) = 
    let all_positions_list = List.cartesian_product (List.init (find_board_length game) ~f:(fun row -> row )) (List.init (find_board_length game) ~f:(fun col-> col )) in 
    List.map all_positions_list ~f:(fun (row, col) -> {Position.row=row ; Position.column=col})

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  (*takes a game as input and returns a list of available positions *)
  let all_positions_list = get_all_positions game in 
  List.filter all_positions_list ~f:(fun pos -> not (Map.mem game.board pos)) 

let check_for_win (game: Game.t) (target_piece: Piece.t): bool =
  (*create a list of winning positions and then use list.mem to see if the current board wins *)
  let directions = [ (0, 1); (1, 0); (1, 1); (1, -1) ] in
  let board_size = find_board_length game in 
  let piece_positions =
    Map.filter game.board ~f:(fun p -> Piece.equal p target_piece)
    |> Map.keys
  in

  List.exists piece_positions ~f:(fun { row; column } ->
    List.exists directions ~f:(fun (dr, dc) ->
      let line =
        List.init board_size ~f:(fun i ->
          { Position.row = row + (i * dr); column = column + (i * dc) })
      in
      List.for_all line ~f:(fun pos ->
        match Map.find game.board pos with
        | Some p when Piece.equal p target_piece -> true
        | _ -> false
      )
    )
  )

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  (*check for an illegal move, then check for a win, and then if all cells are full and no winnner, then draw, otheriwse continue*)

  (*check for an illegal move*)
  let board_positions = get_all_positions game in 
  let illegal_move_check = (List.exists board_positions ~f:(
    fun pos-> pos.row >= find_board_length game || pos.column >= find_board_length game)) in 

  if illegal_move_check 
    then Evaluation.Illegal_move
  else (
  (*check for a win*)
  if (check_for_win game Piece.X) 
    then Evaluation.Game_over  {winner = Some Piece.X}
  else if (check_for_win game Piece.O)
    then Evaluation.Game_over {winner = Some Piece.O}
    (*check for a draw*)
  else 
    (
    if List.is_empty (available_moves game)
      then Evaluation.Game_over {winner = None } 
  else Evaluation.Game_continues 
    )
  ) 
;;

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  (*find winning moves by figuring out the next location to move to 
  use check for win and the piece you are given 
  *)
  let available_positions = available_moves game in 
  List.filter available_positions ~f:(fun pos ->
    let simulated_board = Map.set game.board ~key:pos ~data:me in
    let simulated_game = {game with board = simulated_board} in 
    check_for_win simulated_game me 
  )


(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  (*check to see if opponent piece is in winning moves list *)
    let opponent_piece = Piece.flip me in 
    let losing_list = winning_moves ~me:opponent_piece game in 
    losing_list

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate non_win in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let open_positions = available_moves game in 
  let win_moves = winning_moves ~me:you_play game in 
  let block_moves = losing_moves ~me:you_play game in
  let board_size = find_board_length game in 
  let center = {Position.row = board_size/2; Position.column = board_size/2} in 
  let bottom_right_edge = {Position.row = board_size-1; Position.column = board_size-1} in 

  if not (List.is_empty block_moves) 
    then List.hd_exn block_moves 
else 
  if (List.mem open_positions center ~equal:Position.equal) then
    center
  else 
    if (List.mem open_positions bottom_right_edge ~equal:Position.equal) then
    bottom_right_edge
  else 
  if not (List.is_empty win_moves)
    then List.hd_exn win_moves 
else List.hd_exn open_positions


