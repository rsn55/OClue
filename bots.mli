(** Bots contains all the functions a user or AI bot may use during 
    gameplay.  *)

(** When a player [answerer] reveals a card [shown] to another  *)
type reveal = {
  shown: Board.card;
  answerer: Board.card;
}

(** All that a player can observe about a move made by another player.  *)
type move = {
  asker: Board.card;
  guess: Board.solution;
  answerer: Board.card option;
  asked: Board.card list;
}

(** Instance of information about a player  *)
type player_info



module type BotPlayer = sig
  type t
  val initialize: Board.card list -> Board.card list -> Board.card list -> 
    Board.card -> Board.card -> t
  val get_data : t -> string list 
  val update_data : t -> Board.card list -> t 
  val get_info: t -> player_info
  val get_character: t -> Board.card
  val get_hand: t -> Board.card list
  val get_current_room: t -> Board.card
  val get_accusation: t -> Board.solution option
  val ready_to_accuse: t -> bool
  val add_card_to_hand: t -> Board.card -> t
  val make_guess: Board.t -> t -> (Board.solution * t)
  val update_from_move: move -> t -> t
  val update_from_shown: Board.t -> (reveal option * Board.card list) -> t ->
    Board.solution  -> t  val card_request: Board.solution -> t -> reveal option
  val set_accusation: Board.t -> t -> t
  val update_room: Board.card -> t -> t
  val data_to_string: t -> string

end

module type UserPlayer = sig
  type t
  val initialize: Board.card -> Board.card -> t
  val get_data: t -> Board.card list
  val get_info: t -> player_info
  val get_character: t -> Board.card
  val get_hand: t -> Board.card list
  val get_current_room: t -> Board.card
  val add_card_to_hand: t -> Board.card -> t
  val update_from_move: move -> t -> t
  val update_from_shown: Board.t -> (reveal option * Board.card list) -> t ->
    Board.solution  -> t  val update_room: Board.card -> t -> t
  val card_request: Board.solution -> t -> reveal option

end

module EasyBot : BotPlayer 

module GameUser : UserPlayer

module MediumBot: BotPlayer

module HardBot: BotPlayer
