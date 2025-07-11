open! Core

type t = Tic_tac_toe | Omok [@@deriving sexp_of, equal, bin_io]

val to_string : t -> string
val to_string_hum : t -> string

val board_length : t -> int
(** [board_length] returns the length of the board. 3 for [ Tic_tac_toe ] and 15
    for [Omok]. *)

val win_length : t -> int
(** [win_length] returns the winning length of the board. 3 for [ Tic_tac_toe ]
    and 5 for [Omok]. *)
