(** Main processes user inputs and advances the game.  *)

(** [score] defines a score for the leaderboard feature of the game. Stores
    [name] of user, their [score], and the [json_file] they played *)
type score = {
  name : string;
  score: int;
}

(** [compare_scores] compares two scores and orders based on their numeric 
    score. *)
val compare_scores: score -> score -> int 

(** [print_red] prints a string [s] in red and returns unit. *)
val print_red: string -> unit

(** [print_blue] prints a string [s] in blue and returns unit. *)
val print_blue: string -> unit

(** [read_roll_move] parses the users response to the question where they would 
    like to move to after rolling. [l] denotes the user's response and it takes 
    a Board [bd] and Game_state [st] to return the string back if it's a valid 
    move (isn't a room that doesn't exist or the current room, user is already 
    in).  *)
val read_roll_move: string -> Board.t -> Game_state.t -> string

(** [print_row_divider] prints out the row divider for the table. It takes in 
    [dim_width], which denotes the width in spaces of the table. Prints a plus 
    halfway into table row to display division between columns.  *)
val print_row_divider: int -> int -> string -> unit

(** [print_card_row] prints out the row for a single card and returns a unit 
    given the width dimension of the table [dim_width], the card name of card to
    print [card_name], the list of eliminated cards for a player, [eliminated]. 
*)
val print_card_row: int  -> int -> string -> string -> string list -> 
  Game_state.player -> unit

(** [print_card_row_with_divider] prints the entire card row with the dividers 
    sorrounding the entry in the table, taking in the [width], [eliminated], 
    [user], and [card_name]. *)
val print_card_row_with_divider: int -> string list -> Game_state.player -> 
  string -> unit

(** [print_tables] prints the entire table, given [width], the board [bd], and 
    game state [st]. *)
val print_tables: int -> Board.t -> Game_state.t -> unit

(** calls open_in or open_out depending on level [l] and [json_file]. *)
val assign_in_out_channel: Command.level -> string -> (string -> 'a) -> 'a

(** [get_scores_from_lines] converts a list of [lines] from the file into a list
    of scores. *)
val get_scores_from_lines: string list -> score list -> score list

(** [print_sorted_scores] prints the sorted scores [lst] into the write file 
    using an out channel [oc] *)
val print_sorted_scores: score list -> out_channel -> unit

(** [ignore_first_line] ignores the first line of a string list [lines] and
    returns the remaining list. *)
val ignore_first_line: string list -> string list

(** [read_file] reads the contents of a file with [input_channel]. Returns a 
    string list *)
val read_file: in_channel -> string list

(** [write_leaderboard] prints the leaderboard onto the corresponding file 
    depending on the level [l]. It takes in the name of user [name], number of 
    suggestions [num_suggestions], and the [json_file], which the user played. 
*)
val write_leaderboard: string -> int -> Command.level  -> string -> unit

(** [loop] is the recursive function that takes [bd], current [state], and level
    [l] as inputs and allows the user to continue playing the game, until they 
    quit. *)
val loop: Board.t -> Game_state.t -> Command.level -> bool -> unit

(** [init_description] prints the initial description of game, given by board 
    [bd]. *)
val init_description: Board.t -> string -> unit

(** [play_game f] starts the Clue game in file [f]. *)
val play_game: string -> unit

val saved_level : Command.level