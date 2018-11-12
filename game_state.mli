(** The game_state is the dynamic representation of each player and info about
    the game as it changes with play. *)
open Command 

(** The type [turn] represents the ordering in which each [player] is designated 
    to take their [turn]. *)
type turn

(** The type [actor] is the module that defines the moves of a [user] or [bot]. 
*)
type actor = Easy of Bots.EasyBot.t | User of Bots.GameUser.t 
           | Medium of Bots.MediumBot.t | Hard of Bots.HardBot.t

(** The type [player] represents the [players] in the [game state]. *)
type player = {
  actor: actor;
  suspect: Board.card;
  turn: turn;
  user: bool;
  user_notes : (string * string) list
}

(** The abstract type of values representing the [game state]. *)
type t

(** [get_user_bool] returns [true] if the given [player] is the [user] and false
    otherwise. *)
val get_user_bool : player -> bool

(** [create_cards] returns a [card] list from a given string list. *)
val create_cards : string list -> Board.card list -> Yojson.Basic.json -> 
  Board.card list

(** [get_data_state] returns a string list of the data from the given [player].
*)
val get_data_state : player -> string list

(** [update_data_bot] returns an [actor] after updating the [data] for the 
    given [player] using the given [card] list. *)
val update_data_bot : player -> Board.card list -> actor

(** [deal_to_player] returns the given [actor] after dealing the [actor] 
    [cards]. *)
val deal_to_player : actor -> Yojson.Basic.json -> string -> actor 

(** [create_actor] returns an [actor] using the given string list for 
    information. *)
val create_actor : string list -> Yojson.Basic.json -> actor

(** [create_state] returns a [game state] using the given [solution], 
    [player] list, [winner], [turn], [losers], and [number of suggestions]. *)
val create_state : Board.solution ->
  player list -> Board.card option -> int -> Board.card list -> int -> t

(** [actor_to_string] returns a string of the given [actor]. *)
val actor_to_string : actor -> string 

(** [turn_to_int] returns the given [turn] as an integer. *)
val turn_to_int : turn -> int

(** [create_turn] returns a [turn] from the given integer. *)
val create_turn : int -> turn

(** [user_bool_string] returns the [user] field of the given [player] as a 
    string. *)
val user_bool_string : player -> string

(** [get_losers] returns the list of [Suspect cards] that have made a false 
    [accusation], and therefore have lost the game. *)
val get_losers : t -> Board.card list

(** [get_actor p] is the actor of player [p]  *)
val get_actor: player -> actor

(** [get_turn] returns the turn for the given [player]. *)
val get_turn : player -> turn 

(** [get_user_notes] returns a [card] list with all of the human user's notes 
    for a given game state [t]. *)
val get_user_notes : player -> (string * string) list 

(** [get_weapon_from_sol] returns the [Weapon card] that is part of the solution 
    of the game. *)
val get_weapon_from_sol : Board.solution -> Board.card

(** [get_room_from_sol] returns the [Room card] that is part of the solution of 
    the game. *)
val get_room_from_sol : Board.solution -> Board.card

(** [get_suspect_from_sol] returns the [Suspect card] that is part of the 
    [solution] of the game. *)
val get_suspect_from_sol : Board.solution -> Board.card

(** [check_user] returns true if the [player] is the [user] or false if it is a 
    [bot]. *)
val check_user : player -> bool

(** [get_suspect] returns the [Suspect Card] that correspeonds to the player 
    [p]. *)
val get_suspect : player -> Board.card

(** [replace] returns a string where all instances of [inp] have been replaced
    by [out]. *)
val replace : string -> string -> string -> string

(** [edit_user_note] edits a single user, player [p]'s user notes by adding or 
    removing [ascii_symbol] to card given by [card_string]. Adding or removing 
    is determined by bool [add]. *)
val edit_user_note: player -> string -> string -> bool -> player

(** [update_user_notes_list] updates the [player] list by updating the 
    [user_notes] of player [p] and returning the [player] list containing all 
    [players] in the game with [p]'s [user_notes] updated. Adding or removing is
    determined by bool[add]. *)
val update_user_notes_list: player -> player list -> string -> 
  string -> player list -> bool -> player list

(** [update_user_notes_state] updates the [game state] by updating the 
    [user_notes] of player[p] with given [ascii_symbol] for given [card_string].
*)
val update_user_notes_state: player -> t -> string -> string -> bool -> t

(** [get_user_character] returns the [Suspect card] of the user from the 
    [player] list given. 
    Raises : Failure if no matching [Suspect cards] are found. *)
val get_user_character : player list -> Board.card

(** [get_user_player] returns the [player] assigned to the human user in state 
    [t]. 
    Raises : Failure if no matching [Suspect cards] are found. *)
val get_user_player : player list -> player

(** [get_player_from_suspect] returns the [player] that corresponds to the 
    [Suspect Card] given. 
    Raises : Failure if no matching [Suspect cards] are found. *)
val get_player_from_suspect : Board.card -> player list -> player

(** [get_player_with_turn] returns the [player] that has the same [turn] as the
    one given. 
    Raises : Failure if it is no [player's] [turn]. *)
val get_player_with_turn : turn -> player list -> player 


(** [get_actor_string] returns the given [player] after retreiving its
    [Supect card] and converting it to a string. *)
val get_actor_string : player -> string 

(** [get_actors] returns a string list of all [players] in the given [player]
    list after retreiving their [Supect cards] and converting them to a string 
    list. *)
val get_actors : player list -> string list -> string list

(** [roll_dice] returns an integer between 1 and 6, after initializing the seed
    for random with the given [unit]. *)
val roll_dice : unit -> int 

(** [random_card] returns a tuple with a [card] and a [card] list that contains
    all of the [cards] in the given list minus the one that is first in the 
    tuple. *)
val random_card : Board.card list -> Board.card * Board.card list

(** [update_actor] returns the given [player] after updating its [actor] to
    the given [actor].  *)
val update_actor : player -> actor -> player

(** [update_player_actor] returns the given [player] list after finding the 
    given [player] and and calling [update_actor] on that [player] with the 
    given [actor]. *)
val update_player_actor : player list -> player -> actor -> player list 
  -> player list

(** [update_player] returns the given [player] list after replacing
    a [player] with the given [player] if they share the same [character]. *)
val update_player : player list -> player -> player list -> player list

(** [deal_to_players] returns a [player] list in which each [player] is randomly
    dealt equal amount of [cards] +- one from the given [card] list. 
    Raises : Failure if there is something wrong with the shuffle algorithm. *)
val deal_to_players : player list -> Board.card list -> player list 
  -> player list

(** [init_players] returns a [player] list in which each [player] has a 
    character, or [Suspect card], a current room or [Room card] and a [turn]. *)
val init_players : Board.card list -> Board.card list -> Board.card list -> 
  Board.card list-> player list -> Board.t -> level -> player list  

(** [init_state] returns a game state [t] in which there is a [solution], a 
    human user [Suspect card], a list of [players] that have been initialized 
    using [init_players], no [winner], no [user_notes], a current player, which
    is the human user [Suspect card], and no losers. *)
val init_state : Board.t -> level -> t

(** [user_player_string] returns the human user [player] in the form of a 
    string. 
    Raises : Failure if [user player] is not a [Suspect card]. *)
val user_player_string : t -> string

(** [user_current_room] returns the [Room card] that the user in the game 
    is currently located in.
    Raises : Failure [get_user] returned a [bot]. *)
val user_current_room : t -> Board.card

(** [current_player_room_str] returns the [room] that the current [player] is 
    in, in the form of a string. 
    Raises : Failure if [current user] [room] is not a [Room card]. *)
val current_player_room_str : t -> string

(** [lst_rooms] returns a string list of all the [rooms] that are available to 
    the current [player] to move to. This consists of all the [rooms] in the 
    [bd], except for the [room] they are currently in. *)
val lst_rooms : Board.card list -> t -> Board.room list -> string list

(** [string_rooms] returns a string of all the rooms that are added to the 
    string list when you call [lst_rooms] with the given state [t].  The string
    list is concatenated to a string with a common and space as a separator. *)
val string_rooms : Board.card list -> t -> string list -> string

(** [get_soltion] returns the [solution] for the given state [t]. *)
val get_solution : t -> Board.solution

(** [get_players] returns the list of [players] for the given state [t]. *)
val get_players : t -> player list 

(** [get_winner] returns Some [player] if a winner exists for the given state 
    [t] and None otherwise.  *)
val get_winner : t -> Board.card option

(** Gets a string option of the notes for a given card [card_string]. Returns
    None if card_string entered is invalid *)
val get_user_notes_for_card: string -> player -> string option

(** [get_current_player] returns the [player] who currently has their turn 
    [current_turn] for the given state [st]. 
    Raises : Failure if there is no current [player]. *)
val get_current_player : int -> player list -> player

(** [current_from_state] returns the [player] who's turn it currently is in the
    given [game_state]. *)
val current_from_state: t -> player

(** [current_card_from_state] returns the [Suspect card] of the player who's
    turn it currently is in the given [game_state]. *)
val current_card_from_state : t -> Board.card

(** [get_current_turn] returns the [turn] that is current for the given state
    [t]. *)
val get_current_turn : t -> int

(** [increment_turn] returns a [turn] after incrementing the given [turn] in 
    the given state [t]. *)
val increment_turn : int -> t -> int

(** [increment_user_num_suggestions] takes in a Game_state and returns 
    Game_state with num_suggestions incremented by 1. *)
val increment_user_num_suggestions: t -> t

(** [next_turn] returns the [turn] that is next in the given state [t]. *)
val next_turn : int -> player list -> int

(** [go_neighbor] returns the [player] who has the next [turn] after 
    the given [player]. *)
val go_neighbor : player -> player list -> player

(** [guess_random_card] returns a random [card] that is not in the [player's] 
    hand and has not been eliminated by them. If no such card of this type 
    exists, it chooses any card. *)
val guess_random_card : Board.card list -> Board.card list 
  -> Board.card

(** [init_user_notes] returns a tuple list where the first element in each tuple
    is a unique [card] from the given [board], and the second element in each 
    tuple is an empty string. *)
val init_user_notes : Board.t -> (string * string) list

(** [find_passage] is [None] if there is no existing [passage] in the current 
    room, which can be found by examining a list of all [passages]. 
    Otherwise, it is [Some Room room] that represents the room at the other end
    of the passage.*)
val find_passage : Board.passage list -> Board.card -> Board.card option

(** [move_player] returns the given [player] with their current [Room] updated
    to match the given [Room card]. *)
val move_player : player -> Board.card -> player

(** [get_current_room] returns the current [Room card] with the [Room] that the 
    [player] is in. *)
val get_player_room : player -> Board.card

(** [secret_passage] moves a player [p] to the room at the other end of a
    secret passage, if one exists in their current room. If not, the state does
    not change. Returns (new state, Some new room) or (new state, None) if they
    did not move. *)
val secret_passage : Board.t -> t -> player -> (t * Board.card option)

(** [card_list_to_string] returns a [card] list in the form of a string list. *)
val card_list_to_string : Board.card list -> string list -> string list

(** [get_user_eliminated] returns a string list of all the [players] who 
    have lost the game. 
    Raises : Failure if the [user] is a [bot]. *)
val get_user_eliminated: t -> string list

(** [get_num_suggestions] returns the number of suggestions made by 
    the player in a round. *)
val get_num_suggestions: t -> int

(** [move_party_from_guess] returns the given [player] list after moving the 
    player with the [Suspect card] that matches the given [solution] to the 
    [Room card] that matches the given [solution]. *)
val move_party_from_guess : player list -> Board.solution -> player list

(** [get_guess_str] returns the given [solution] in the form of a string. *)
val get_guess_str : Board.solution -> string list -> string

(** [get_players_hand] returns a string list of all the [cards] in the hand
    of the given [player]. *)
val get_players_hand : player -> string list

(** [get_players_cards] returns a list of all the [cards] in the hand of the 
    given [player]. *)
val get_players_cards : player -> Board.card list

(** [find_card] returns [Some card] if the string given is found in the given
    [card] list or [None] otherwise. *)
val find_card : string -> Board.card list -> Board.card option

(** [check_card] returns [Some card] if the string given matches one of the
    [cards] in the given [solution] and None otherwise. *)
val check_card : Board.solution -> string -> t -> Board.card option

(** [in_hand] returns true if any of the [cards] in the given [card] list match
    any of the cards in the [solution] and false otherwise. *)
val in_hand : Board.solution -> Board.card list -> bool

(** [check_for_guess] returns [Some card] if any of the [cards] in the given 
    [card] list match the cards in the given [solution] and [None] otherwise. *)
val check_for_guess : Board.solution -> Board.card list -> Board.card option

(** [ask_user] returns a [Some reveal] if the given [player] has a [card] found
    in the given [solution] and [None] otherwise. *)
val ask_user : Board.solution -> t -> player -> Bots.reveal option

(** [ask_neighbor] returns Some [card] if the given [player] has either the
    [Weapon card], [Room card], or [Suspect card] in the given [solution], and
    None otherwise. 
    Raises : Failure if a [user] is asked, instead of a [bot]. *)
val ask_neighbor : player -> Board.solution -> Bots.reveal option

(** [player_to_string] returns a string of the [Suspect card] of the given 
    [player]. *)
val player_to_string : player -> string 

(** [check_neighbors_cards] returns a tuple. The first component of the tuple is
    also a tuple that contains Some [card] and Some [Suspect card] if that 
    any of the other [players] has a [card] matching the given list of [cards], 
    or [None] otherwise.  The second component is the list of [Suspect cards] 
    that don't have any of the [cards] in the guess. 
    Raises : Failure if the [user] does not have a card in their hand. *)
val check_neighbors_cards : t -> player ->
  player list ->
  Board.solution ->
  Board.card list ->
  player ->
  (Bots.reveal option) * Board.card list

(** [user_guess] returns a string that represents what happened after a [user] 
    made a suggestion.  This entails what which AI bots did not have any 
    [cards], and which [bot] did have something, and what this [card] was. 
    Raises : Failure if the [player] is a [bot] instead of a [user]. *)
val user_guess : t -> Board.t -> Board.solution -> t * string

(** [bot_guess] returns [Some solution] if the current user, is a [bot], in the
    given [game state] and has not lost yet and returns [None] if the the [bot] 
    has lost. 
    Raises : Failure if the current user is the human user and not a [bot]. 
*)
val bot_guess : t -> Board.t -> Board.solution option * t

(** [medium_ai_turn] returns a tuple with the updated [game state] after a 
    medium level of difficulty [bot] takes a turn, and a string to recount
    what happened during the turn, ex: who showed what card, who didn't have
    a card, etc. 
    Raises : Failure if the [Medium bot] was not ready to make an [accusation] 
    but the [bot's] [get_accusation] function was called. *)
val medium_ai_turn : t -> Board.t  -> t * string

(** [easy_ai_turn] returns a tuple with the updated [game state] after a 
    easy level of difficulty [bot] takes a turn, and a string to recount
    what happened during the turn, ex: who showed what card, who didn't have
    a card, etc. 
    Raises : Failure if the [Easy bot] was not ready to make an [accusation] but 
    the [bot's] [get_accusation] function was called. *)
val easy_ai_turn : t -> Board.t -> t * string

(** [ai_turn] returns a tuple with the new game state [t] and a string that 
    can be printed to tell the user what happened, after an automated player 
    took their turn. 
    Raises : Failure if the current [player] is a [user] and not a [bot]. *)
val ai_turn : t -> Board.t -> t * string

(** [valid_weapon] returns true if the given string is a valid [Weapon Card] 
    found in the given state [t] and Board [bd]. *)
val valid_weapon : string -> Board.t -> bool

(** [valid_suspect] returns true if the given string is a valid [Suspect Card] 
    found in the given state [t] and Board [bd]. *)
val valid_suspect : string -> Board.t -> bool

(** [valid_room] returns true if the given string is a valid [Room Card] 
    found in the given state [t] and Board [bd]. *)
val valid_room : string -> Board.t -> bool

(** [valid_room] returns true if the given string is a valid card in 
    the given Board [bd] *)
val valid_card: string -> Board.t -> bool

(** [weapon_solution] returns the [weapon] that is found in the given [solution]
    as a string. 
    Raises : Failure if the [weapon] in the given [solution] is not a 
    [Weapon card]. *)
val weapon_solution : Board.solution -> string

(** [suspect_solution] returns the [suspect] that is found in the given 
    [solution] as a string. 
    Raises : Failure if the [suspect] in the given [solution] is not a 
    [Suspect card]. *)
val suspect_solution : Board.solution -> string

(** [room_solution] returns the [room] that is found in the given [solution]
    as a string. 
    Raises : Failure if the [room] in the given [solution] is not a 
    [Room card]. *)
val room_solution : Board.solution -> string

(** [valid_guess] returns true if the given string list is a valid [suggestion]
    for the given [bd] and state [t] and false otherwise. *)
val valid_guess : string list -> Board.t -> t -> bool

(** [valid_guess] returns true if the given string list is a valid [accusation]
    for the given [bd] and state [t] and false otherwise. *)
val valid_solution : string list -> Board.t -> t -> bool

(** [create_solution] returns a [solution] from a string list. 
    Requires: The string list has a length of 3 and is in the following format: 
    "Weapon"; "Suspect"; "Room". *)
val create_solution : string list -> Board.solution

(** [get_solution_str] returns a string that represents the [solution] for the 
    given game state [t] in the following format: "Weapon Suspect Room". *)
val get_solution_str : t -> string list -> string

(** [user_to_cards] returns a string list of each [card] that the human user
    has in their hand currently. 
    Raises : Failure if [get_user] returned a [bot]. *)
val user_to_cards : t -> string list

(** [all_cards_helper] returns a string list that consist of each [player], 
    followed by their respective [cards] in hand in the given game state [t], 
    and given an empty accumulator. *)
val all_cards_helper : t -> player list -> string list -> string list

(** [all_cards] calls [all_cards_helper] and returns a string list that consist 
    of each [player], followed by their respective [cards] in hand in the given
     game state [t]. *)
val all_cards : t -> string list

(** [get_winner_string] returns the string with the name of the [player] who 
    won, in a given game state [t]. *)
val get_winner_string: t -> string

(** [print_passages] returns a string that lists the secret [passages] on the
    board [t]. *)
val print_passages : Board.t -> string

(** [card_option_to_string] returns a string of a [card] option.  *)
val card_option_to_string: Board.card option -> string

(** [player_locations_string players] is a string [acc] showing the 
    locations of each [player] in [players]. *)
val player_locations_string: player list -> string -> string

(* [form_solution] turns a group of 3 [cards] into a [solution] format *)
val form_solution : Board.card -> Board.card -> Board.card -> Board.solution

(** [get_current_player st] Returns the [Suspect card] of the current [player].
*)
val get_current_player_card : t -> Board.card

(** [update_turn] returns the given game state [t] after updating the [turn] 
    to be the next [player]. *)
val update_turn : t -> t

val current_player_room : t -> Board.card

(** [current_player_string] returns the current [player], or rather whoever's 
    [turn] it is currently, in the form of a string. *)
val current_player_string : t -> string  

(** [valid_move] returns true if the user inputted a valid string representing
    the [room] they wish to move into, meaning the [room] is in the game and not 
    their current [room], and false otherwise. *)
val valid_move : string -> t -> Board.t -> bool

(** [update_current_room] returns the given [game state] after updating the 
    [Room card] of the current [player]. *)
val update_current_room: t -> Board.card -> t

(** [get_characters] returns a string list with all of the [players] in the 
    given [game state]. *)
val get_characters : t -> player list -> string list -> string list

(** [get_characters_to_string] returns a string with all of the [players] in 
    the given [game_state]. *)
val get_characters_to_string : t -> string

(** [notes_to_string] returns [user notes] as a string list. *)
val notes_to_string : (string * string) list -> string list -> t -> string list
