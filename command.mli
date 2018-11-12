(** Command contains all the commands a user can make from terminal.  *)

(** The type [solution] represents the suspect, weapon, and location that a user
    will either suggest or accuse. *)
type solution_lst = string list 

(** The type [level] specifies which level of difficulty a user chooses to 
    play. *)
type level = Easy | Medium | Hard | Saved

(** The type [card] represents the card that the user will show another player 
    if that card is mentioned in a suggestion.   *)
type card_str = string 

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a a card or solution. *)
type command = 
  | Roll 
  | Show of card_str
  | Suggestion of solution_lst
  | Accusation of solution_lst
  | Next 
  | Quit
  | Solution
  | Cards
  | Seen 
  | Notes
  | Write of (card_str * string)
  | Erase of (card_str * string)
  | Leaderboard of (level * string)
  | OtherCards
  | Passages
  | Sneak
  | NoCards
  | Locations
  | Options
  | Save 

(**[remove_spaces] remove any empty string elements ("") of [lst]. *)
val remove_spaces: string list -> string list -> string list

(**[create_command] parses a list of words [lst] into a command. 
    Raises: [empty] if the string given is empty, or only consists of 
    whitespaces.  
    Raises: [malformed] if the input string does not match any of the command 
    verbs, if show, suggestion, or accusation are not followed by a card or 
    solution, or if roll or next are followed by another non whitespace string.
*)
val create_command : string list -> command

(** [parse] returns a command consisting of a verb and may be followed by a card
    or a solution. 
    Raises: [empty] if the string given is empty, or only consists of 
    whitespaces.  
*)
val parse : string -> command

(** [create_level] parses a list of words [list] into a level. 
    Raises: [empty] if the string given is empty, or only consists of 
    whitespaces.  
    Raises: [malformed] if the input string does not match any of the levels, 
    or if more than one word is entered. *)
val create_level : string list -> level

(** [parse] returns a level consisting of one word. 
    Raises: [empty] if the string given is empty, or only consists of 
    whitespaces. 
*)
val parse_level : string -> level
