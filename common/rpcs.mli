open! Core
open Async

module Take_turn : sig
  module Query : sig
    type t = { game : Game.t; you_play : Piece.t } [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = Position.t [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end
