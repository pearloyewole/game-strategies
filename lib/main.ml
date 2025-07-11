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

  let indices =
      List.init (find_board_length game) ~f:(fun row -> row) in 
  List.iter indices ~f:(fun row -> print_row row; print_endline (String.make (find_board_length game * 4) '-'))

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

let available_moves_that_do_not_immediately_lose ~(me : Piece.t) (game : Game.t) : Position.t list = 
    (*find all the moves that are legal and will not let the opponent win*)
    let open_moves = available_moves game in 
    let safe_moves = List.filter open_moves ~f:(fun move ->
      let simulated_board = Map.set game.board ~key:move ~data:me in 
      let simulated_game = {game with board = simulated_board} in
      let opponent_wins = losing_moves ~me: me simulated_game in 
      List.is_empty opponent_wins
      )  in 

      safe_moves 



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

let exercise_six=
  Command.async ~summary:"Exercise 6: one move ahead"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = available_moves_that_do_not_immediately_lose ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
      ("six", exercise_six;)
    ]

(* Exercise 5 *)

let score ~(game : Game.t) (you_play : Piece.t) : float =
  match evaluate game with
  | Game_over { winner = Some winner } ->
      if Piece.equal winner you_play then Float.infinity else -.Float.infinity
  | Game_over { winner = None } -> 0.0  
  | Game_continues -> 0.0  
  | Illegal_move -> -.Float.infinity

let simulate (game : Game.t) (pos : Position.t) (piece : Piece.t) : Game.t =
  let new_board = Map.set game.board ~key:pos ~data:piece in
  { game with board = new_board }

let rec minimax ~(game : Game.t) ~(depth : int) ~(you_play : Piece.t) ~(maximizing : bool) : float =
  match evaluate game with
  | Game_over _ | Illegal_move ->
      score ~game:game you_play
  | Game_continues ->
      if depth = 0 then score ~game:game you_play
      else
        let moves = available_moves game in
        if maximizing then
          List.fold moves ~init:(-.Float.infinity) ~f:(fun acc move ->
            let sim_game = simulate game move you_play in
            let eval = minimax ~game:sim_game  ~depth:(depth - 1)
                                  ~you_play ~maximizing:false in
            Float.max acc eval
          )
        else
          let opponent = Piece.flip you_play in
          List.fold moves ~init: Float.infinity ~f:(fun acc move ->
            let sim_game = simulate game move opponent in
            let eval = minimax ~game: sim_game ~depth:(depth - 1)
                                  ~you_play:you_play ~maximizing:true in
            Float.min acc eval
          )

let best_move ~(game : Game.t) ~(you_play : Piece.t) ~(depth : int) : Position.t list =
  let moves = available_moves game in
  match moves with
  | [] -> []
  | _ ->
    let scored_moves = List.map moves ~f:(fun move ->
      let sim_game = simulate game move you_play in
      let score = minimax ~game:sim_game ~depth:(depth - 1)
                          ~you_play:you_play ~maximizing:false in
      (move, score, evaluate sim_game)
    ) in
    let max_score =
      List.fold scored_moves ~init:Float.neg_infinity ~f:(fun acc (_, score, _) ->
        Float.max acc score)
    in
    print_game game ; 
    print_s [%message (scored_moves: (Position.t * float * Evaluation.t) list) ] ; 
    List.filter scored_moves ~f:(fun (_, score, _) -> Float.equal score max_score)
    |> List.map ~f:(fun (move, _, _) -> move)




  let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let open_positions = available_moves_that_do_not_immediately_lose ~me: you_play game in 
  let _win_moves = winning_moves ~me:you_play game in
  let safe_moves = best_move ~game: game ~you_play: you_play ~depth: 1 in 
  let block_moves = losing_moves ~me:you_play game in
  let board_size = find_board_length game in 
  let _center = {Position.row = board_size/2; Position.column = board_size/2} in  
  let _bottom_right_edge = {Position.row = board_size-1; Position.column = board_size-1} in 

  if not (List.is_empty safe_moves)
    then List.nth_exn safe_moves (Random.State.int (Random.State.make_self_init ()) (List.length safe_moves))
  else 
    if not (List.is_empty block_moves || List.length block_moves > 1) 
    then 
      List.nth_exn block_moves (Random.State.int (Random.State.make_self_init ()) (List.length block_moves))
else 
  List.nth_exn  open_positions (Random.State.int (Random.State.make_self_init ()) (List.length open_positions))