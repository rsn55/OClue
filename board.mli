(** The board is the abstract representation of all the cards and secret 
    passages in an instance of a game. *)

(** The type [weapon] represents the possible murder weapons. *)
type weapon = string

(** The type [suspect] represents the possible murderers. *)
type suspect = string

(** The type [room] represents the rooms in Mr. Boddy's homes, and the possible
    locations of the murder. *)
type room = string

(** The type [card] represents the cards that each player is dealt.  The three
    types of cards are: rooms, weapons, and suspects. *)
type card = Room of room | Weapon of weapon | Suspect of suspect

(** The type [passage] is a tuple representing a secret passage, or two Room
    cards that a player can travel between at any time without rolling.  *)
type passage = (card * card)

(** The type [solution] is a record that holds a weapon card, suspect card, and
    room card. One solution exists for the game Clue, and users can make 
    suggestions or accusations for the solution. *)
type solution = {weapon: card; suspect: card; room: card}

(** The abstract type of values representing a Board. *)
type t 

(** [all_rooms] is a string list of all the rooms found in the [json] file 
    given. 
    Requires: [json] is a valid JSON Board representation. *)
val all_rooms : Yojson.Basic.json -> room list 

(** [all_suspects] is a string list of all the suspects found in the [json] file 
    given. 
    Requires: [json] is a valid JSON Board representation. *)
val all_suspects : Yojson.Basic.json -> suspect list 

(** [all_rooms] is a string list of all the weapons found in the [json] file 
    given. 
    Requires: [json] is a valid JSON Board representation. *)
val all_weapons : Yojson.Basic.json -> weapon list 

(** [all_passages] is a string list of all the passages found in the [json] file
    given. 
    Requires: [json] is a valid JSON Board representation.*)
val all_passages : Yojson.Basic.json -> string list

(** [room_cards] returns a list of all [Room Cards] found in the given [Room] 
    list. *)
val room_cards : room list -> card list -> card list

(** [suspect_cards] returns a list of all [Suspect Cards] found in the given 
    [Suspect] list. *)
val suspect_cards : suspect list -> card list -> card list

(** [weapon_cards] returns a list of all [Weapon Cards] found in the given 
    [Weapon] list. *)
val weapon_cards : weapon list -> card list -> card list

(** [make_passages] returns a passage list by grouping together items in the 
    [passagelst] given. 
    Raises: Failure if the given list does not have an even length. *)
val make_passages : string list -> passage list -> passage list

(** [from_json] returns a Board [t] from a given json file name 
    string, [json_file]. *)
val from_json : string -> t

(** [get_rooms] returns a [card] list of all the [Room Cards] found in the 
    given Board [t]. *)
val get_rooms : t -> card list

(** [get_weapons] returns a [card] list of all the [Weapon Cards] found in the 
    given Board [t]. *)
val get_weapons : t -> card list

(** [get_suspects] returns a [card] list of all the [Suspect Cards] found in the 
    given Board [t]. *)
val get_suspects : t -> card list

(** [get_cards] returns a [card] list of all the Cards found in the 
    given Board [t]. *)
val get_cards : t -> card list

(** [get_passages] returns a passage (i.e. (card * card)) list of all the 
    secret passages found in the given Board [t]. *)
val get_passages : t -> passage list

(** [get_file_name] returns the file name string of a given board [bd] *)
val get_file_name: t -> string

(** [card_to_string] returns the contents of a [card] in the form of a string. 
*)
val card_to_string : card -> string