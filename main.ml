open Board
open Game_state
open Command
open Bots 

let ascii_title = 
  "\n\n ██████╗  ██████╗██╗     ██╗   ██╗███████╗
██╔═══██╗██╔════╝██║     ██║   ██║██╔════╝
██║   ██║██║     ██║     ██║   ██║█████╗  
██║   ██║██║     ██║     ██║   ██║██╔══╝  
╚██████╔╝╚██████╗███████╗╚██████╔╝███████╗
 ╚═════╝  ╚═════╝╚══════╝ ╚═════╝ ╚══════╝
                                          \n\n"
  ^"CREATED BY RACHEL NASH, MEREDITH DOBRZYNSKI, AND KAUSHIK RAVIKUMAR AT "
  ^"CORNELL UNIVERSITY\n\n"

let notes_width = 80

let score_divider = ' '

type score = {
  name : string;
  score: int;
}

let compare_scores (score_a : score) (score_b : score) : int = 
  score_a.score - score_b.score


(** [file_name_list] returns a list of file names that are found in the 
    directory [handle].  A condition that must be met is that the file
    name ends in ".txt". *)
let rec file_name_list (acc: string list) (handle: Unix.dir_handle) : 
  string list = 
  match Unix.readdir handle with 
  | exception End_of_file -> acc
  | file_name -> 
    if (String.length file_name >= 4 && 
        ((String.sub file_name ((String.length file_name) - 4) 4) = 
         ".txt")) then 
      file_name_list (file_name::acc) handle else
      file_name_list acc handle 

let print_red (s : string) : unit = 
  ANSITerminal.(print_string [red] s)

let print_blue (s : string) : unit = 
  ANSITerminal.(print_string [blue] s)

let rec read_roll_move (l: string) (bd: Board.t) (st: Game_state.t) : string = 
  match parse l with
  | Quit -> print_blue "Thanks for playing.\n"; Pervasives.exit 0
  | exception _ -> if valid_move l st bd then l else (
      print_red "You entered an invalid command.\n";
      print_blue "Please type one of the following: \n"; 
      print_blue ((string_rooms (get_rooms bd) st []) ^ "\n\n");  
      print_string "> ";
      read_roll_move (read_line ()) bd st )
  | _ -> print_red "You entered an invalid command.\n";
    print_blue "Please type one of the following: \n"; 
    print_blue ((string_rooms (get_rooms bd) st []) ^ "\n\n"); 
    print_string "> "; 
    read_roll_move (read_line ()) bd st

let rec print_row_divider (dim_width : int) (count : int) (acc:string) : unit =
  if count = 0 then print_string acc 
  else if count mod (dim_width / 2) = 0 then 
    print_row_divider dim_width (count - 1) ("+" ^ acc)
  else
    print_row_divider dim_width (count - 1) ("-" ^ acc)

let rec print_card_row (dim_width : int) (count : int) (acc : string) 
    (card_name : string) (eliminated : string list) (user: player): unit = 
  let notes = 
    match get_user_notes_for_card card_name user with 
    | None -> ""
    | Some note -> note 
  in
  if count = 0 then print_string acc
  else if count = (dim_width / 2) - (dim_width / 4) then 
    print_card_row dim_width (count - String.length card_name) (card_name ^ acc) 
      card_name eliminated user
  else if count mod (dim_width / 2) = 0 then 
    print_card_row dim_width (count - 1) ("|" ^ acc) card_name eliminated user
  else if count = dim_width - (dim_width / 4) && ((notes <> "") 
                                                  || (List.mem card_name 
                                                        eliminated)) then 
    let full_notes = if List.mem card_name eliminated then ("X" ^ notes) else 
        notes in 
    print_card_row dim_width (count - String.length full_notes )
      (full_notes ^ acc) card_name eliminated user
  else print_card_row dim_width (count - 1) (" " ^ acc) card_name eliminated 
      user

let print_card_row_with_divider (width : int) (eliminated : string list) 
    (user : player)
    (card_name : string) : unit =
  print_row_divider width width "";
  print_string ("\n");
  print_card_row width width "" card_name eliminated user;
  print_string("\n")

let rec satisfies_character_count (l : (string * string) list) 
    (max_count : int) : bool = 
  match l with 
  | [] -> true
  | h :: t -> if String.length (snd h) > max_count then false
    else satisfies_character_count t max_count

let print_tables width (bd: Board.t) (st : Game_state.t) : unit = 
  let eliminated_cards = get_user_eliminated st in
  let user = get_user_player (get_players st) in
  let suspect_names = get_suspects bd |> List.map card_to_string in
  let weapon_names = get_weapons bd |> List.map card_to_string in
  let room_names = get_rooms bd |> List.map card_to_string in
  (* if (satisfies_character_count (get_user_notes user) (width /4)) then *)
  (print_string ("\n\n" ^ "Characters" ^ "\n\n");
   List.iter (print_card_row_with_divider width eliminated_cards user) 
     suspect_names;
   print_row_divider width width "";
   print_string ("\n\n" ^ "Weapons" ^ "\n\n");
   List.iter (print_card_row_with_divider width eliminated_cards user) 
     weapon_names;
   print_row_divider width width "";
   print_string ("\n\n" ^ "Rooms" ^ "\n\n");
   List.iter (print_card_row_with_divider width eliminated_cards user) 
     room_names;
   print_row_divider width width "")
(* else print_string("Error, notes exceeded maximum character count") *)

let generate_file_name (l : level) (json_file : string) : string =
  if l = Easy then "easy_leaderboard" ^ "_" ^ 
                   String.sub json_file 0 (String.length json_file - 5) ^ ".txt"
  else if l = Medium then "medium_leaderboard" ^ "_" ^ 
                          String.sub json_file 0 (String.length json_file - 5) 
                          ^ ".txt"
  else "hard_leaderboard" ^ "_" ^ 
       String.sub json_file 0 (String.length json_file - 5) ^ ".txt"

let assign_in_out_channel (l : level) (json_file : string) (f) = 
  let generated_file_name = (generate_file_name l json_file) in 
  f generated_file_name

let rec get_scores_from_lines (lines : string list) 
    (acc : score list) : score list =
  match lines with
  | [] -> acc
  | h :: t -> let split_string = String.split_on_char score_divider h in 
    if List.length split_string >= 2 then
      get_scores_from_lines t (
        { 
          name = List.nth split_string 0;
          score = int_of_string (List.nth split_string 1);
        }
        :: acc)
    else
      get_scores_from_lines t acc

let print_sorted_scores (lst : score list) (oc : out_channel) : unit = 
  output_string oc "Name #Suggestions\n";
  let rec loop_scores (scores_lst : score list) : unit =
    match scores_lst with
    | [] -> close_out oc
    | h :: t -> output_string oc (h.name ^ Char.escaped score_divider ^ 
                                  (string_of_int h.score) ^ "\n");
      loop_scores t
  in loop_scores lst

let ignore_first_line (lines : string list) : string list = 
  match lines with 
  | [] -> []
  | h :: t -> t

let rec print_list (lst : string list) (acc : string): unit = 
  match lst with
  | [] -> print_string acc
  | h :: t -> print_list t (acc ^ h ^ "\n")

let read_file (input_channel : in_channel) : string list  = 
  let read () = 
    try Some (input_line input_channel) with
      End_of_file -> None in
  let rec loop_lines acc = 
    match read () with
    | Some s -> loop_lines (s :: acc)
    | None -> close_in input_channel; List.rev acc in
  loop_lines []

let write_leaderboard (name : string) (num_suggestions : int)
    (l : level) (json_file : string) : unit = 
  let generated_file_name = generate_file_name l json_file in
  let lines = 
    if Sys.file_exists generated_file_name then
      read_file 
        (assign_in_out_channel l json_file (open_in))
    else [] in
  let output_channel = assign_in_out_channel l json_file (open_out) in
  let scores = get_scores_from_lines (ignore_first_line lines) [] in
  let updated_scores = {
    name = name;
    score = num_suggestions;
  } :: scores in
  let sorted_scores = List.sort (compare_scores) updated_scores in
  print_sorted_scores sorted_scores output_channel

(** [saved_level] returns the level of the game that is saved in the state.txt
    file. *)
let saved_level : Command.level = 
  let state_str = read_file (open_in "state.txt") in 
  let bot = List.nth state_str 2 in 
  let lst = String.split_on_char ' ' bot in 
  let x = List.nth lst 0 in 
  match x with 
  | "easy" -> Easy
  | "medium" -> Medium
  | "hard" -> Hard
  | _ ->   
    let bot = List.nth state_str 3 in 
    let lst = String.split_on_char ' ' bot in 
    let x = List.nth lst 0 in 
    match x with 
    | "easy" -> Easy
    | "medium" -> Medium
    | "hard" -> Hard
    | _ ->  failwith "two users"

(** [player_list_string] returns a string list of all the [players] with 
    all of their corresponding fields. *)
let rec player_list_string (players : player list) (acc : string list) 
  : string list = 
  match players with 
  | [] -> acc 
  | h::t -> let actor = actor_to_string (get_actor h) in 
    let suspect = card_to_string (get_suspect h) in 
    let turn = (string_of_int (turn_to_int (get_turn h))) in 
    let user = user_bool_string (h) in 
    let room = card_to_string (get_player_room h) in 
    let cards = card_list_to_string (get_players_cards h) [] in 
    let cards_string = String.concat " " cards in 
    player_list_string t ((actor ^ " " ^ suspect ^ " " ^ turn ^  " " ^user^
                           " " ^ room ^ " " ^cards_string)::acc)  

(** [check_winner] returns the string "None" if there is no winner, otherwise
    it returns the given string [s]. *)
let check_winner (s : string) : string = 
  if s = "" then "None" else s  

(** [data] returns a string list with each [player] followed by the data stored
    in that [player bot]. *)
let rec data (players : player list) (acc : string list) (st: Game_state.t)
  : string list  = 
  match players with 
  | [] -> acc 
  | h::t -> if (h = get_user_player (get_players st)) then data t acc st else 
      data t (((card_to_string (get_suspect h)) 
               ^ ": " ^(String.concat " " (get_data_state h)))::acc) st   

(** [state_to_string_lst] returns the given [st] and all of its corresponding
    fields as a string list. *)
let state_to_string_lst (st : Game_state.t) : string = 
  let solution_string = get_solution_str st [] in 
  let players = player_list_string (get_players st) [] in 
  let winner = get_winner_string (st) in 
  let winner = check_winner winner in 
  let turn = string_of_int (get_current_turn st) in 
  let losers = card_list_to_string (get_losers st) [] in 
  let num_suggestions = string_of_int (get_num_suggestions st) in 
  let notes = notes_to_string (get_user_notes 
                                 (get_user_player (get_players st))) [] st in 
  let data = data (get_players st) [] st in 
  solution_string ^ "\n" ^"Start" ^ "\n" ^(String.concat "\n" players) ^ "\n"
  ^ "Done"  ^  "\n" ^ winner ^ "\n"
  ^ turn ^ "\n" ^ (String.concat " " losers) ^ "\n" ^ num_suggestions ^ 
  "\n" ^ (String.concat "\n" notes) ^ "\n" ^ "Data" ^ "\n" ^
  (String.concat "\n" data)

(** [create_sol] returns a [solution] after reading the state.txt file and 
    converting the [solution] string written there.  *)
let create_sol : solution = 
  let state_str = read_file (open_in "state.txt") in 
  let solution = List.nth state_str 0 in 
  let lst = String.split_on_char ' ' solution in 
  { 
    weapon = Weapon (List.nth lst 0); 
    suspect = Suspect (List.nth lst 1); 
    room = Room (List.nth lst 2); 
  }

(** [players_helper] returns a string list with all of the [players] 
    and their information from a given string list [s]. *)
let rec players_helper (s : string list) (acc : string list) : string list = 
  match s with 
  | "Start"::t -> players_helper t acc 
  | "Done"::t -> acc 
  | h::t -> players_helper t (h::acc)  
  | _ -> acc 

(** [winner_helper] returns the integer that represents the line number where
    the [winner] string is located in the state.txt file. *)
let rec winner_helper (s: string list) (acc : int) : int = 
  match s with 
  | "Done"::t -> acc+1 
  | h::t -> winner_helper t acc+1 
  | _ -> acc 

(** [winner] returns [None] if there is no winner in the saved game, or 
    [Some Suspect winner] otherwise. *)
let winner : Board.card option = 
  let state_str = read_file (open_in "state.txt") in 
  let index = winner_helper state_str 0 in 
  let winner = List.nth state_str index in 
  if (winner = "None") then None 
  else Some (Suspect winner)

(** [current_turn] returns the string with the [current turn] from the saved
    [game state]. *)
let current_turn : string = 
  let state_str = read_file (open_in "state.txt") in 
  let index = winner_helper state_str 0 in 
  List.nth state_str (index+1)

(** [losers] returns the name of the losers in the saved [game state] or 
    an empty string if there are none. *)
let losers : string list = 
  let state_str = read_file (open_in "state.txt") in 
  let index = winner_helper state_str 0 in  
  if (List.nth state_str (index+2) = "") then [] 
  else String.split_on_char ' ' (List.nth state_str (index+2))

(** [suggestions] returns the number of [suggestions] that the [user] in the 
    saved [game state] made. *)
let suggestions : string = 
  let state_str = read_file (open_in "state.txt") in 
  let index = winner_helper state_str 0 in 
  List.nth state_str (index+3)

(** [players_helper] returns a string list with all of the [players] 
    and their information from the saved [game state]. *)
let players_string : string list = 
  let state_str = read_file (open_in "state.txt") in 
  players_helper (List.tl state_str) [] 

(** [deal_to_actor] returns the given [actor] after updating the [actor's hand] 
    to match the saved [game state]. *)
let rec deal_to_actor (a : actor) (j : Yojson.Basic.json) (acc : int) 
    (limit : int) (lst : string list) : Game_state.actor = 
  if acc = limit then a 
  else deal_to_actor (deal_to_player a j (List.nth lst acc)) j (acc-1) limit lst

(** [create_notes_helper] returns a [notes] list from a given string list. *)
let rec create_notes_helper (lst : string list) (init : int) (limit : int) 
    (acc : (string * string) list) : (string * string) list = 
  if init = limit then acc else 
    let entire_note = List.nth lst init in 
    let note_lst = String.split_on_char ' ' entire_note in 
    let card = List.nth note_lst 0 in 
    let symbols = String.concat " " (List.tl note_lst) in 
    create_notes_helper lst (init+1) limit 
      ((card, symbols)::acc)

(** [data_helper] returns the integer that represents the index of where the 
    [bots' data] starts in the saved [game state] file. *)
let rec data_helper (s: string list) (acc : int) : int = 
  match s with 
  | "Data"::t -> acc+1 
  | h::t -> data_helper t acc+1 
  | _ -> acc 

(** [create_notes] returns a [notes] listr from the saved [game state]. *)
let create_notes : (string * string) list = 
  let state_str = read_file (open_in "state.txt") in 
  let index = winner_helper state_str 0 in 
  create_notes_helper state_str (index+4) (data_helper state_str 0) []


(** [data_lst] returns a [card] list of all of the data for a given 
    [player bot]. *)
let data_lst (s : string) (json : Yojson.Basic.json) : card list = 
  let lst = String.split_on_char ' ' s in 
  let notes = List.tl lst in 
  create_cards notes [] json 

(** [update_bots] returns the given [player] list after updating the 
    data that the [bot] stores with information from the saved [game state]. *)
let rec update_bots (players : player list) (start : int) (acc : player list) 
    (s: string list) (json : Yojson.Basic.json) : Game_state.player list = 
  match players with 
  | [] -> acc 
  | h::t -> if (get_user_bool h) then 
      update_bots t start (h::acc) s json 
    else  
      let new_player = 
        {
          actor = update_data_bot h (data_lst (List.nth s start) json); 
          suspect = h.suspect;
          turn = get_turn h; 
          user = false; 
          user_notes = h.user_notes
        } in 
      update_bots t (start+1) (new_player::acc) s json 

(** [create_player] returns a [player] from a given string, with all of its 
    it's fields filled with the correct information.  *)
let create_player (s: string) (json : Yojson.Basic.json) : player = 
  let lst = String.split_on_char ' ' s in 
  let a = create_actor lst json in 
  let a2 = deal_to_actor a json ((List.length lst)-1) 4 lst in 
  {
    actor = a2; 
    suspect = Suspect (List.nth lst 1);
    turn = create_turn (int_of_string (List.nth lst 2)); 
    user = bool_of_string (List.nth lst 3);
    user_notes = create_notes; 
  }

(** [create_players_helper] returns a player list from a given string list. *)
let rec create_players_helper (lst : string list)
    (acc : player list) (json : Yojson.Basic.json) : player list = 
  match lst with 
  | [] -> acc 
  | h::t -> create_players_helper t ((create_player h json)::acc) json   

(** [create_players] returns a player list from the saved [game state]. *)
let create_players  (json : Yojson.Basic.json) : player list = 
  create_players_helper players_string [] json 

(** [create_state_in_main] returns a [game state] from the saved [game state]. 
*)
let create_state_in_main (json: Yojson.Basic.json) : Game_state.t = 
  let state_str = read_file (open_in "state.txt") in 
  let length = data_helper state_str 0 in 
  create_state create_sol 
    (update_bots (create_players json) length [] state_str json)
    winner (int_of_string current_turn) [] (int_of_string suggestions)

(** [loop] is the recursive function that takes [bd] and current [state] as 
    inputs and allows the user to continue playing the game, until they quit. *)
let rec loop (bd: Board.t) (state : Game_state.t) (l: level) (just_rolled: bool)
  : unit = 
  if ((user_player_string state) = (current_player_string state)) then (
    print_blue ("\nYou are player " ^ (current_player_string state) ^ ".\n" ^
                "You are currently in the " ^ (current_player_room_str state) ^ 
                ".\n\nType 'options' to see your available moves.\n"); 
    print_string "\n> ";
    match parse (read_line ()) with 
    | Roll -> 
      if just_rolled then (let message = "\nYou cannot roll again.\n" in 
                           print_red message; 
                           loop bd state l true) else (
        let num = roll_dice (Random.self_init()) in 
        print_blue ("\nYou rolled a " ^ (string_of_int num) ^ ". "); 
        if (num < 3) then (
          print_blue "You cannot move since you rolled below a 3.\n";
          loop bd state l true) 
        else (
          print_blue ("Since you rolled a 3 or above, please enter the\n" 
                      ^ "name of the room you would like to move to. You" 
                      ^ " cannot stay in the \nsame room. " ^  "Please type one"  
                      ^ " of the following: \n\n" ^ (string_rooms 
                                                       (get_rooms bd) state [])^
                      "\n\n" ^ "> ");
          let new_room = read_line () in 
          let roll_move = read_roll_move new_room bd state in
          print_blue ("\nYou moved to the " ^ 
                      (String.trim roll_move) ^ ".\n");
          loop bd (update_current_room state (Room (String.trim roll_move))) l 
            true))
    | Suggestion guess -> 
      ( match valid_guess guess bd state with 
        | true -> 
          let guess_result = user_guess state bd (create_solution guess) in 
          let new_state = increment_user_num_suggestions 
              (update_turn (fst guess_result)) in
          print_blue (snd guess_result); 
          loop bd new_state l false  
        | false -> 
          print_red "You entered an invalid command.\n"; 
          loop bd state l just_rolled) 
    | Accusation final -> 
      (match valid_solution final bd state with 
       | true -> print_blue "Congratulations, you won!\n";
         print_string ("\nWhat's your name?\n");
         let name = (read_line ()) in 
         write_leaderboard name 
           (get_num_suggestions state) l (get_file_name bd);
         Pervasives.exit 0 
       | false -> print_red "Your accusation was wrong. You lose.\n";
         Pervasives.exit 0 )
    | Quit ->  print_blue "Thanks for playing.\n"; Pervasives.exit 0 
    | Save -> 
      (match l with 
       | Easy ->  
         output_string (open_out "state.txt") (state_to_string_lst state); 
         print_blue "Thanks for saving and playing.\n"; Pervasives.exit 0 
       | _ -> print_red ("You cannot save this game. You must be on the" ^
                         " easy level.\n"); loop bd state l just_rolled) 
    | Cards -> 
      print_blue (String.concat " " (["Your cards:"]@(user_to_cards state))
                 ); loop bd state l just_rolled 
    | Notes -> 
      print_string ("\nIn order to add a custom note use command in format: " ^
                    "'write card asciisymbol' \n To remove a custom note use " ^
                    "command in format: 'erase card asciisymbol' \n X" ^
                    " symbolizes cards you've already seen.\n"); 
      print_tables notes_width bd state;
      loop bd state l just_rolled
    | Write (card_string, ascii_string) -> 
      if valid_card card_string bd then 
        (loop bd ((update_user_notes_state (get_user_player 
                                              (get_players state)) 
                     state card_string ascii_string) true) l just_rolled)
      else print_string "Please enter valid card name";
      loop bd state l just_rolled
    | Erase (card_string, ascii_string) -> 
      if valid_card card_string bd then
        loop bd ((update_user_notes_state (get_user_player (get_players state)) 
                    state card_string ascii_string) false) l just_rolled
      else 
        print_string "Please enter valid card name";
      loop bd state l just_rolled
    | Options ->  print_string "\nHere are your available moves:\n"; 
      let to_print = if just_rolled then 
          "\n1. If your current room contains a secret passage, you can sneak\n" 
          ^"through it by typing 'sneak'." 
          ^"\n2. You can make a suggestion for the  suspect, murder weapon, and"
          ^"\ncurrent room in this format: 'suggest Suspect Weapon CurrentRoom'"
          ^"\n3. You can make an accusation if you think you know the suspect, "
          ^"murder weapon, and room, in the following format: \n" 
          ^"'accuse Suspect Weapon Room'"
          ^"\n4. You can see your cards by typing: 'cards'" 
          ^"\n5. You can view all the secret passages by typing 'passages'"
          ^"\n6. You can view each player's location by typing 'locations'"
          ^"\n7. You can view your detective notes by typing 'notes'" 
          ^"\n8. You can quit the game by typing 'quit' " 
        else 
          "\n1. You can roll the dice by typing 'roll'. If you roll a 3 or\n" 
          ^ "above, you can move to any room you wish."
          ^"\n2. If your current room contains a secret passage, you can sneak" 
          ^"\n through it by typing 'sneak'." 
          ^"\n3. You can make a suggestion for the murder weapon, suspect, and"
          ^"\ncurrent room in this format: 'suggest Suspect Weapon CurrentRoom'"
          ^"\n4. You can make an accusation if you think you know the murder \n"
          ^"weapon, suspect, and room, in the following format: \n" 
          ^"'accuse Suspect Weapon Room'"
          ^"\n5. You can see your cards by typing: 'cards'" 
          ^"\n6. You can view all the secret passages by typing 'passages'"
          ^"\n7. You can view each player's location by typing 'locations'"
          ^"\n8. You can view your detective notes by typing 'notes'" 
          ^"\n9. You can view the leaderboard by typing 'leaderboard Level" ^ 
          " GameFile'"
          ^"\n10. You can quit the game by typing 'quit' " 
      in 
      let to_print2 = if (l = Easy) then 
          "or you can quit the game and save your progress by typing 'save'"
          ^ ".\n\n"
        else "\n\n" in 
      print_string (to_print^to_print2); loop bd state l just_rolled 
    | Leaderboard (level, json_file) -> 
      let generated_file_name = generate_file_name l json_file in
      if Sys.file_exists generated_file_name then
        print_list (read_file (assign_in_out_channel l json_file (open_in))) "";
      loop bd state l just_rolled
    | Passages -> print_blue ("Secret Passages: " ^ (print_passages bd)); 
      loop bd state l just_rolled
    | Sneak ->  let moved = secret_passage bd state 
                    (current_from_state state) in
      let print_out = if snd moved = None then 
          "There is no secret passage in your current room." else 
          "You went through a secret passage. You are now in the "^
          (snd moved |> card_option_to_string) ^"." in 
      if (snd moved = None) then print_red print_out else 
        print_blue print_out;
      loop bd (fst moved) l just_rolled   
    | Locations -> let loc = player_locations_string (get_players state) "" in 
      print_blue loc; loop bd state l just_rolled                
    | exception Malformed -> print_red "You entered an invalid command.\n";
      loop bd state l just_rolled 
    | exception Empty -> print_red "You entered an empty command.\n";
      loop bd state l just_rolled
    | _ -> loop bd state l just_rolled
  )   
  else (
    let ai_result = ai_turn state bd in 
    print_blue (snd ai_result); 
    match get_winner (fst ai_result) with
    | None -> loop bd (update_turn (fst ai_result)) l false
    | Some w ->  Pervasives.exit 0 
  )

let rec init_description bd f = 
  print_string 
    ("\nYou are an invited guest to Mr. Boddy's dinner party, but shortly \n"
     ^"after your arrival, he is found dead in one of the rooms, a victim of \n"
     ^"foul play. The object of this game is to discover the identity of the \n"
     ^"murderer, the murder weapon, and the room in which it took place.\n");
  print_string 
    ("\nThe first player to successfully deduce each of these facts wins. \n"
     ^"Players can get new information by moving into the rooms and making \n"
     ^"'suggestions' of what they believe to be the room, the person, and the\n"
     ^"weapon that make up the answer. This may reveal which cards are in \n"
     ^"other players' hands and which cards are missing and must, therefore,\n" 
     ^"be the solution.\n");
  print_string ("\nPlease enter 'easy', 'medium', or 'hard' to choose a level"
                ^ " of difficulty. Or type 'saved' to play the last saved" ^
                " game.\n> "); 
  let j = Yojson.Basic.from_file f in 
  match parse_level (read_line()) with 
  | Easy ->  
    let state = (init_state bd Easy) in 
    print_blue (String.concat " " (["\nYour cards:"]@(user_to_cards state)));
    loop bd state Easy false
  | Medium -> let state = (init_state bd Medium) in 
    print_blue (String.concat " " (["\nYour cards:"]@(user_to_cards state))); 
    loop bd state Medium false
  | Hard -> let state = (init_state bd Hard) in 
    print_blue (String.concat " " (["\nYour cards:"]@(user_to_cards state))); 
    loop bd state Hard false
  | Saved -> 
    let state = create_state_in_main j in 
    print_blue (String.concat " " (["\nYour cards:"]@(user_to_cards state))); 
    loop bd state saved_level false
  | exception Empty -> print_red "\nInvalid level. Please try again.\n"; 
    init_description bd f
  | exception Malformed -> print_red "\nInvalid level. Please try again.\n"; 
    init_description bd f

(** [play_game f] starts the Clue game in file [f]. *)
let rec play_game f =
  Random.self_init ();
  match f with 
  | "quit" -> Pervasives.exit 0;
  | f when not (Filename.is_relative f) -> 
    (print_red ("We do not support files outside current directory.\n"); 
     print_string  "> ";
     match read_line () with
     | exception End_of_file -> ()
     | file_name -> play_game file_name )
  | f when (Sys.file_exists f && 
            (Filename.extension f = ".json"))->
    let board = from_json f in 
    init_description board f 
  | f when not (Sys.file_exists f)  ->
    (print_red "That file does not exist.\n"; 
     print_string  "> ";
     match read_line () with
     | exception End_of_file -> ()
     | file_name -> play_game file_name )
  | f  ->
    (print_red ("You need to pick a JSON file.\n"); 
     print_string "> "; 
     match read_line () with
     | exception End_of_file -> ()
     | file_name -> play_game file_name )

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_blue ascii_title;
  print_string ("\nPlease enter the name of the board file you" ^ 
                " want to load. " ^ 
                "\n\n");
  print_string "> "; 
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()

