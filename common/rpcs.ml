open! Core
open Async

module Take_turn = struct
  module Query = struct
    type t = { game : Game.t; you_play : Piece.t } [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = Position.t [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create ~name:"take-turn" ~version:0 ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
end
