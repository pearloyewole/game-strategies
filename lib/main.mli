open! Core
open Game_strategies_common_lib

val print_game : Game.t -> unit

val make_move : game:Game.t -> you_play:Piece.t -> Position.t
(** [make_move ~game ~you_play] is a function that takes a game and a piece, and
    returns the position where the piece should be placed. *)

val command : Async.Command.t
