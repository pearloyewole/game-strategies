open! Core
open! Async
open Game_strategies_common_lib

module Illegal_move = struct
  type t = Out_of_bounds | Occupied_space of Piece.t [@@deriving sexp_of]
end

module Outcome = struct
  type t =
    | Next_piece of Piece.t
    | Winner of Piece.t
    | Draw
    | Illegal_move of Illegal_move.t
  [@@deriving sexp_of]
end

module Game_state = struct
  type t = {
    game : Game.t;
    next_piece : Piece.t;
    x_connection : Rpc.Connection.t;
    o_connection : Rpc.Connection.t;
    x_name : string;
    o_name : string;
    move_timeout : Time_ns.Span.t;
  }
end

module Game_over_reason = struct
  type t =
    | Winner of Piece.t
    | Draw
    | Illegal_move of { piece : Piece.t; error : Illegal_move.t }
    | Player_timed_out of Piece.t
    | Error_requesting_move of { piece : Piece.t; error : Error.t }
  [@@deriving sexp_of]
end

type t =
  | Game_in_progress of Game_state.t
  | Game_over of {
      reason : Game_over_reason.t;
      game : Game.t;
      x_name : string;
      o_name : string;
    }

let game = function
  | Game_in_progress { game; _ } | Game_over { game; _ } -> game

let game_kind t = (game t).game_kind
