open Board
open Bots
open Command
open Str

type turn = int 
type actor = Easy of Bots.EasyBot.t | User of Bots.GameUser.t 
           | Medium of Bots.MediumBot.t  | Hard of Bots.HardBot.t

type player = {
  actor: actor;
  suspect: card;
  turn: turn;
  user: bool;
  user_notes : (string * string) list
}

type t = {
  solution: solution;
  players: player list;
  winner: card option;
  current_turn: int;
  losers : card list;
  num_suggestions: int;
}

module E = EasyBot
module U = GameUser
module M = MediumBot
module H = HardBot

let get_user_bool (p: player) : bool = 
  p.user

let get_data_state (p : player) : string list = 
  match p.actor with 
  | Easy a -> E.get_data a 
  | Medium b -> M.get_data b 
  | Hard c -> H.get_data c 
  | _ -> failwith "get data for user when it should be bot"  

let rec create_cards (s : string list) (acc : card list) 
    (json : Yojson.Basic.json) : card list = 
  match s with 
  | [] -> acc 
  | h::t -> 
    if (List.mem h (all_rooms json)) then create_cards t ((Room h)::acc) json  
    else if List.mem h (all_weapons json) then create_cards t ((Weapon h)::acc) 
        json 
    else create_cards t ((Suspect h)::acc) json 

let update_data_bot (p : player) (lst : card list) : actor  = 
  match p.actor with 
  | Easy a -> Easy (E.update_data a lst)
  | Medium b -> Medium (M.update_data b lst)
  | Hard c -> Hard (H.update_data c lst) 
  | _ -> failwith "get data for user when it should be bot"   

let create_actor (lst : string list) (json : Yojson.Basic.json)
  : actor = 
  if List.nth lst 0 = "easy" then 
    Easy (E.initialize (suspect_cards (all_suspects json) [])
            (room_cards (all_rooms json) []) 
            (weapon_cards (all_weapons json) [])
            (Suspect (List.nth lst 1)) (Room (List.nth lst 4)))
  else if List.nth lst 0 = "medium" then 
    Medium (M.initialize (suspect_cards (all_suspects json) []) 
              (room_cards (all_rooms json) []) 
              (weapon_cards (all_weapons json) [])
              (Suspect (List.nth lst 1)) (Room (List.nth lst 4)))
  else if List.nth lst 0 = "hard" then 
    Hard (H.initialize (suspect_cards (all_suspects json) []) 
            (room_cards (all_rooms json) []) 
            (weapon_cards (all_weapons json) [])
            (Suspect (List.nth lst 1)) (Room (List.nth lst 4)))
  else 
    User (U.initialize (Suspect (List.nth lst 1)) (Room (List.nth lst 4))) 

let actor_to_string (a : actor) : string = 
  match a with 
  | Easy a -> "easy"
  | Medium b -> "medium"
  | Hard c -> "hard"
  | User d -> "user"

let create_state (s: solution) (players : player list) (winner : card option) 
    (turn : int ) (losers : card list) (num_suggestions : int) : t = 
  {
    solution= s;
    players= players;
    winner= winner;
    current_turn = turn;
    losers = losers;
    num_suggestions = num_suggestions;
  }

let deal_to_player (p : actor) (json) (c : string) : actor = 
  if List.mem c (all_rooms json) then 
    match p with 
    | Easy b -> Easy (E.add_card_to_hand b (Room c))
    | User u -> User (U.add_card_to_hand u (Room c))
    | Medium m -> Medium (M.add_card_to_hand m (Room c))
    | Hard d -> Hard (H.add_card_to_hand d (Room c))
  else 
  if List.mem c (all_weapons json) then 
    match p with 
    | Easy b -> Easy (E.add_card_to_hand b (Weapon c))
    | User u -> User (U.add_card_to_hand u (Weapon c))
    | Medium m -> Medium (M.add_card_to_hand m (Weapon c))
    | Hard d -> Hard (H.add_card_to_hand d (Weapon c))
  else 
    match p with 
    | Easy b -> Easy (E.add_card_to_hand b (Suspect c))
    | User u -> User (U.add_card_to_hand u (Suspect c))
    | Medium m -> Medium (M.add_card_to_hand m (Suspect c))
    | Hard d -> Hard (H.add_card_to_hand d (Suspect c))


let turn_to_int (t : turn) : int = 
  t  

let create_turn (i : int) : turn = 
  i 

let user_bool_string (p : player) : string = 
  match p.user with 
  | true -> "true"
  | false -> "false"

let get_losers (st : t) : card list = st.losers

let get_actor (p : player) : actor = 
  p.actor

let get_turn (p : player) : turn =
  p.turn

let get_user_notes (p : player) : (string * string) list = 
  p.user_notes

let get_weapon_from_sol (s :solution): card =
  s.weapon

let get_room_from_sol (s :solution): card =
  s.room

let get_suspect_from_sol (s :solution) : card = 
  s.suspect

let check_user (p : player) : bool =
  p.user

let get_suspect (p : player) : card =
  p.suspect

let replace (inp : string) (out : string) =
  Str.global_replace (Str.regexp_string inp) out;;

let edit_user_note (p : player) (card_string : string) (ascii_symbol : string) 
    (add : bool) : player = 
  let rec iterate_notes (note_lst : (string * string) list)
      (acc : (string * string) list) : (string * string) list  =
    match note_lst with 
    | [] -> acc
    | (card, ascii) :: t -> 
      let new_note = if add then (ascii ^ ascii_symbol) else 
          (replace ascii_symbol ""  ascii) in
      if (card = card_string) then iterate_notes t ((card, new_note) :: acc) 
      else iterate_notes t ((card, ascii) :: acc)
  in 
  {
    actor = p.actor;
    suspect = p.suspect;
    turn = p.turn;
    user = p.user;
    user_notes = iterate_notes p.user_notes []
  }

let rec update_user_notes_list (p : player) (players : player list) 
    (card_string : string) (ascii_symbol : string)
    (acc : player list) (add : bool) : player list = 
  match players with
  | [] -> acc
  | h::t -> let edited_user_notes = 
              (edit_user_note h card_string ascii_symbol add) in
    if h.suspect = p.suspect then update_user_notes_list p t card_string 
        ascii_symbol ( edited_user_notes :: acc) add
    else update_user_notes_list p t card_string ascii_symbol (h :: acc) add

let rec update_user_notes_state (p : player)(st : t) (card_string : string) 
    (ascii_symbol : string) (add : bool) : t = 
  {
    solution = st.solution;
    players = update_user_notes_list p 
        st.players card_string ascii_symbol [] add;
    winner = st.winner;
    current_turn = st.current_turn;
    losers = st.losers;
    num_suggestions = st.num_suggestions;
  }

let init_user_notes (bd : Board.t) : (string * string) list = 
  let cards_list = List.map card_to_string (get_cards bd) in
  let rec iterate_cards (card_lst : string list)
      (acc : (string * string) list) : (string * string) list  =
    match card_lst with 
    | [] -> acc
    | h::t -> iterate_cards t ((h,"") :: acc)
  in iterate_cards cards_list []

let rec get_user_character (players : player list) : card =
  match players with
  | [] -> raise (Failure "no user player")
  | h::t -> if check_user h then h.suspect else get_user_character t

let rec get_user_player (players : player list) : player =
  match players with
  | [] -> raise (Failure "no user player")
  | h::t -> if check_user h then h else get_user_player t

let rec get_player_from_suspect (suspect : card) (players : player list) 
  : player =
  match players with 
  | [] -> raise (Failure "there is no suspect with this name")
  | h::t -> if get_suspect h = suspect then h else 
      get_player_from_suspect suspect t

let rec get_player_with_turn (turn : turn) (pl : player list) : player =
  match pl with 
  | [] -> raise (Failure "It is no player's turn.")
  | h::t -> if get_turn h = turn then h else get_player_with_turn turn t

let get_actor_string (player : player) = 
  match (get_actor player) with 
  | Easy a -> card_to_string (E.get_character a)
  | User b -> card_to_string (U.get_character b)
  | Medium m -> card_to_string (M.get_character m)
  | Hard h -> card_to_string (H.get_character h)

let rec get_actors (players : player list ) (acc : string list) = 
  match players with 
  | [] -> acc 
  | h::t -> match (get_actor h) with 
    | Easy a -> get_actors t ((card_to_string (E.get_character a))::acc) 
    | User b -> get_actors t ((card_to_string (U.get_character b))::acc) 
    | Medium m -> get_actors t ((card_to_string (M.get_character m))::acc) 
    | Hard d -> get_actors t ((card_to_string (H.get_character d))::acc) 

let roll_dice (u : unit) : int = 
  (Random.int 6) + 1

let random_card (l : card list) : (card * card list) = 
  let selected = List.nth l (Random.int (List.length l)) in
  (selected, List.filter (fun x -> x != selected) l)

let update_actor (p : player) (a : actor) : player = 
  { actor = a;
    suspect = p.suspect;
    turn = p.turn;
    user = p.user;
    user_notes = p.user_notes
  }

let rec update_player_actor (players : player list) (p : player) (a : actor) 
    (acc : player list) : player list = 
  match players with
  | [] -> acc
  | h::t -> if h.suspect = p.suspect then update_player_actor t p a 
        ({ actor = a;
           suspect = p.suspect;
           turn = p.turn;
           user = p.user;
           user_notes = p.user_notes
         } :: acc) else update_player_actor t p a (h::acc)

let rec update_player (players : player list) (p : player)
    (acc : player list) : player list = 
  match players with
  | [] -> acc
  | h::t -> if h.suspect = p.suspect then update_player t p (p :: acc) 
    else update_player t p (h::acc)

let rec update_actors_from_move (players : player list) (move : move) 
    (acc : player list) : player list =
  match players with
  | [] -> acc
  | h::t -> match h.actor with
    | User u -> 
      update_actors_from_move t move (update_actor h (User 
                                                        (U.update_from_move move
                                                           u ))::acc)
    | Easy e -> update_actors_from_move t move (update_actor h 
                                                  (Easy (E.update_from_move move
                                                           e ))::acc)
    | Medium m -> update_actors_from_move t move (update_actor h 
                                                    (Medium (M.update_from_move 
                                                               move m ))::acc)
    | Hard d -> update_actors_from_move t move (update_actor h
                                                  (Hard (H.update_from_move 
                                                           move d ))::acc)

let rec deal_to_players (acc : player list) (cards : card list) 
    (players : player list) : player list =
  match players with
  | _ when cards = [] -> players @ acc
  | [] when cards != [] -> deal_to_players [] cards acc
  | h::t -> let c = random_card cards in
    let new_actor =
      match h.actor with
      | Easy b -> Easy (E.add_card_to_hand b (fst c))
      | User u -> User (U.add_card_to_hand u (fst c) )
      | Medium m -> Medium (M.add_card_to_hand m (fst c) )
      | Hard d -> Hard (H.add_card_to_hand d (fst c) )
    in 
    let new_player = update_actor h new_actor in
    deal_to_players (acc @ [new_player]) (snd c) t
  | _ -> raise (Failure "something wrong with shuffle")

let rec init_players (suspects : card list) (all_suspects : card list) 
    (all_weapons : card list) (all_rooms : card list) (acc : player list)
    (bd : Board.t) (level : level) : player list = 
  match suspects with 
  | [] -> acc
  | h :: [] -> let new_player = {
      actor = User (U.initialize h (get_rooms bd |> random_card |> fst));
      suspect = h;
      turn = List.length suspects;
      user = true;
      user_notes = init_user_notes bd;
    } in new_player::acc
  | h :: t -> let random_sus = random_card suspects in
    let new_player = {
      actor = if (level = Easy) then 
          Easy (E.initialize all_suspects all_weapons all_rooms 
                  (fst random_sus) (get_rooms bd |> random_card |> fst))
        else if (level = Medium) then 
          Medium (M.initialize all_suspects all_weapons all_rooms 
                    (fst random_sus) (get_rooms bd |> random_card |> fst))
        else
          Hard (H.initialize all_suspects all_weapons all_rooms 
                  (fst random_sus) (get_rooms bd |> random_card |> fst));
      suspect = fst random_sus;
      turn = List.length suspects;
      user = false;
      user_notes = init_user_notes bd;
    } in
    init_players (snd random_sus) all_suspects all_weapons all_rooms 
      (new_player::acc) bd level 

let init_state (bd : Board.t) (level : level) : t = 
  let random_weapons = random_card (get_weapons bd) in 
  let random_rooms = random_card (get_rooms bd) in
  let random_suspects = random_card (get_suspects bd) in
  let players = init_players (get_suspects bd) (get_suspects bd) 
      (get_weapons bd) (get_rooms bd) [] bd level  
                |> deal_to_players [] (snd random_suspects) 
                |> deal_to_players [] (snd random_weapons)
                |>  deal_to_players [] (snd random_rooms) in
  let solution = {weapon = fst random_weapons; suspect = fst random_suspects;
                  room = fst random_rooms} in
  {solution = solution;
   players = players;
   winner = None;
   current_turn = 1; 
   losers = [];
   num_suggestions = 0; 
  }

let user_player_string (st : t) : string = 
  match get_suspect (get_user_player st.players) with 
  | Suspect s -> s 
  | _ -> raise (Failure "User player is not a suspect card")

let user_current_room (st : t) : card = 
  match (get_user_player st.players).actor with
  | User u -> U.get_current_room u
  | _ -> raise (Failure "get_user returned a bot")

let current_player_room_str (st : t) : string = 
  match user_current_room st with 
  | Room r -> r 
  | _ -> failwith "Current user room is not a room card"

let rec lst_rooms (rooms : card list) (st : t) (acc : string list) 
  : string list = 
  match rooms with 
  | [] -> acc 
  | (Room r)::t -> if (r = (current_player_room_str st)) then lst_rooms t st acc 
    else lst_rooms t st (r::acc)
  | _::t -> lst_rooms t st acc 

let string_rooms (rooms : card list) (st : t) (acc : string list) : string = 
  String.concat ", " (lst_rooms rooms st acc) 

let get_solution (st : t) : solution = 
  st.solution

let get_num_suggestions (st : t) : int = 
  st.num_suggestions

let get_players (st : t) : player list = 
  st.players

let get_winner (st : t) : card option = 
  st.winner

let get_user_notes_for_card (card_string : string) (p : player) 
  : string option = 
  let rec iterate_notes (user_notes : (string * string) list ) : string option =
    match user_notes with 
    | [] -> None
    | (card, ascii) :: t -> if card = card_string then Some ascii else 
        iterate_notes t
  in iterate_notes p.user_notes

let rec get_current_player (current_turn : int) (players : player list) 
  : player = 
  match players with
  | [] -> raise (Failure "no current player")
  | h::t -> if get_turn h = current_turn then h else 
      get_current_player current_turn t

let current_from_state (st : t) : player =
  get_current_player (st.current_turn) (st.players)

let current_card_from_state (st : t) : card =
  get_suspect (get_current_player (st.current_turn) (st.players))

let get_current_turn (st : t) : int = 
  st.current_turn

let increment_turn (current : int) (st: t) : int = 
  let new_num = current + 1 in
  if (new_num > List.length (get_players st)) then new_num mod List.length 
                                                     (get_players st)
  else new_num 

let increment_user_num_suggestions (st : t) : t = 
  { solution = get_solution st;
    players = get_players st;
    winner = st.winner;
    current_turn = get_current_turn st; 
    losers = st.losers;
    num_suggestions = (get_num_suggestions st) + 1; 
  }

let next_turn (current : int) (players : player list) : int = 
  let new_num = current + 1 in
  if (new_num > List.length players)then new_num mod List.length players
  else new_num 

let go_neighbor (p : player) (players : player list) : player = 
  get_player_with_turn (next_turn p.turn players) players

(* gets a random card that is not in the player's hand and has not been 
   eliminated by them. if no such card of this type exists, 
   it chooses any card. *)
let guess_random_card (eliminated : card list) (all_cards : card list) : card =
  let not_eliminated = List.filter (fun x -> not (List.mem x eliminated)) 
      all_cards in
  match not_eliminated with
  | [] -> fst (random_card all_cards)
  | h::t -> fst (random_card not_eliminated)

let init_user_notes (bd : Board.t) : (string * string) list = 
  let cards_list = List.map card_to_string (get_cards bd) in
  let rec iterate_cards (card_lst : string list)
      (acc : (string * string) list) : (string * string) list  =
    match card_lst with 
    | [] -> acc
    | h::t -> iterate_cards t ((h,"") :: acc)
  in iterate_cards cards_list []

let rec find_passage (p : passage list) (room : card) : card option =
  match p with
  | [] -> None
  | (r1,r2)::t -> if r1 = room then Some r2 else if 
      r2 = room then Some r1 else find_passage t room

let move_player (p : player) (room : card) : player = 
  match p.actor with
  | Easy b -> update_actor p (Easy (E.update_room room b))
  | User u -> update_actor p (User (U.update_room room u))
  | Medium m -> update_actor p (Medium (M.update_room room m))
  | Hard d ->  update_actor p (Hard (H.update_room room d))

let get_player_room (p : player) : card = 
  match p.actor with
  | Easy b -> E.get_current_room b
  | User u -> U.get_current_room u
  | Medium m -> M.get_current_room m
  | Hard d -> H.get_current_room d

let secret_passage (bd : Board.t) (st : t) (p : player) : (t * card option) =
  let passages = get_passages bd in 
  let curr = get_player_room p in
  match find_passage passages curr with
  | None -> (st, None)
  | Some r -> let moved_player = move_player p r in
    ({ solution = get_solution st;
       players = update_player (get_players st) moved_player [];
       winner = get_winner st;
       current_turn = st.current_turn;
       losers = st.losers;
       num_suggestions = (get_num_suggestions st); 
     }, Some r)

let rec card_list_to_string (c : card list) (acc : string list) : string list = 
  match c with 
  | [] -> acc
  | h::t -> card_list_to_string t (card_to_string h :: acc)

let get_user_eliminated (st: t): string list = 
  match (get_user_player st.players).actor with
  | User u -> card_list_to_string (U.get_data u ) []
  | _ -> raise (Failure "got bot as user")

let move_party_from_guess (players : player list) (guess : solution) 
  : player list =
  let moved_aux = (get_player_from_suspect guess.suspect players) in
  let moved_aux_act = (match moved_aux.actor with 
      | User p -> User (U.update_room guess.room p)
      | Easy e -> Easy (E.update_room guess.room e )
      | Medium m -> Medium (M.update_room guess.room m )
      | Hard d ->  Hard (H.update_room guess.room d ))
  in update_player_actor players moved_aux moved_aux_act []

let get_guess_str (guess : solution) (acc : string list) : string = 
  let lst = [card_to_string guess.suspect]@[ card_to_string guess.weapon]@
            [card_to_string guess.room]
  in String.concat " " lst 

let get_players_hand (p : player) : string list = 
  match p.actor with 
  | User u -> card_list_to_string (U.get_hand u) []
  | Easy b -> card_list_to_string (E.get_hand b) []
  | Medium m -> card_list_to_string (M.get_hand m) []
  | Hard d -> card_list_to_string (H.get_hand d) []

let get_players_cards (p : player) : card list = 
  match p.actor with 
  | User u -> (U.get_hand u)
  | Easy b -> (E.get_hand b)
  | Medium m -> (M.get_hand m)
  | Hard d -> (H.get_hand d)

let rec find_card (c : string) (cards : card list) : card option = 
  match cards with 
  | [] -> None 
  | h::t -> match h with 
    | (Room r) -> if (r = c) then Some h else find_card c t  
    | (Suspect s)  -> if (s = c) then Some h else find_card c t  
    | (Weapon w) -> if (w = c) then Some h else find_card c t 

let check_card (guess : solution) (c : string) (st : t) : card option = 
  let user_player = get_user_player st.players in 
  if ((card_to_string (get_weapon_from_sol guess)) = c) 
  || ((card_to_string (get_suspect_from_sol guess)) = c)
  || ((card_to_string (get_room_from_sol guess)) = c) 
  then find_card c (get_players_cards user_player) 
  else None 

let rec in_hand (guess : solution) (cards : card list) : bool = 
  if List.mem (guess.weapon) cards || List.mem (guess.room) cards ||
     List.mem (guess.suspect) cards then true 
  else false  

let rec check_for_guess (guess: solution) (cards : card list) : card option = 
  match cards with 
  | [] -> None 
  | h::t -> match h with 
    | (Room a) -> if (guess.room = Room a) then Some h else 
        check_for_guess guess t  
    | (Suspect b)  -> if (guess.suspect = Suspect b) then Some h else 
        check_for_guess guess t  
    | (Weapon c) -> if (guess.weapon = Weapon c) then Some h else 
        check_for_guess guess t  

let rec ask_user (guess : solution) (st : t) (current : player) 
  : reveal option = 
  let user_card = get_user_character st.players in
  let user_player = get_user_player st.players in 
  ANSITerminal.(print_string [blue] ("\nPlayer "));
  ANSITerminal.(print_string [blue] (card_to_string (get_suspect current)));
  ANSITerminal.(print_string [blue] (" suggested ( "));
  ANSITerminal.(print_string [blue] (get_guess_str guess []));
  ANSITerminal.(print_string [blue] (" ).\n"));
  ANSITerminal.(print_string [blue] 
                  ("You have the following cards in your hand: "));
  ANSITerminal.(print_string [blue] (String.concat " " 
                                       (get_players_hand user_player)));
  ANSITerminal.(print_string [blue] (".\n"));
  ANSITerminal.(print_string [blue] ("Please choose which one to reveal by" ^ "
  typing 'show [card]'\n"));
  print_string  "> ";
  (match parse (read_line ()) with 
   | Quit -> ANSITerminal.(print_string [blue]
                             "Thanks for playing.\n"); Pervasives.exit 0 
   | Show c -> 
     (match check_card guess c st with
      | None -> 
        (match check_for_guess guess (get_players_cards user_player) with 
         | None -> None 
         | Some c -> ANSITerminal.(print_string [blue] 
                                     ("Check the suggestion and your cards and" 
                                      ^ "try again.")); 
           ask_user guess st current)
      | Some revealed -> Some {shown=revealed;answerer=user_card} )
   | exception Malformed -> ANSITerminal.(print_string [blue] ("Error invalid" ^ 
                                                               "command.")); 
     ask_user guess st current
   | exception Empty -> ANSITerminal.(print_string [blue] 
                                        ("Error invalid command.")); 
     ask_user guess st current
   | _ -> ANSITerminal.(print_string [blue] ("Error invalid command.")); 
     ask_user guess st current) 

let rec ask_neighbor (p : player) (guess : solution) : reveal option =
  match p.actor with
  | User u -> failwith "dont ask user in ask_neighbor"
  | Easy b -> E.card_request guess b
  | Medium m -> M.card_request guess m
  | Hard d -> H.card_request guess d

let player_to_string (p : player) : string = 
  card_to_string p.suspect

let rec check_neighbors_cards (st : t) (current_player : player) 
    (players : player list) (guess : solution) (acc : card list) 
    (next_neighbor : player) : (reveal option * card list) =
  if get_suspect next_neighbor = get_suspect current_player then  
    (None, acc) else 
  if (get_suspect next_neighbor = get_user_character st.players 
      && in_hand guess (get_players_cards (get_user_player st.players))) then
    match ask_user guess st current_player with 
    | None -> failwith "user does not have a card in their hand"
    | Some rev -> (Some rev, acc)
  else
  if (get_suspect next_neighbor = get_user_character st.players 
      && not(in_hand guess (get_players_cards (get_user_player st.players)))) 
  then 
    let next = go_neighbor next_neighbor players in 
    check_neighbors_cards st current_player players guess 
      (get_suspect next_neighbor :: acc) next
  else 
    match ask_neighbor next_neighbor guess with 
    | None -> 
      let next = go_neighbor next_neighbor players in 
      check_neighbors_cards st current_player players guess 
        (get_suspect next_neighbor :: acc) next
    | Some rev -> 
      (Some rev, acc)

let user_guess (st : t) (bd : Board.t) (guess : solution) : (t * string) = 
  let u = get_user_player st.players in 
  let user_actor = match u.actor with
    | User y -> y
    | _ -> failwith "should be user"
  in
  let shown = check_neighbors_cards st u st.players guess [] 
      (go_neighbor u st.players) in 
  let user_move = {
    asker = get_suspect u;
    guess = guess;
    answerer = (match fst shown with 
        | None -> None
        | Some rev -> Some rev.answerer);
    asked = snd shown;
  } in
  let updated_suspect = move_party_from_guess st.players guess in
  let updated_from_move = update_actors_from_move updated_suspect user_move [] 
  in
  match shown with
  | (None,_) -> 
    ({solution = st.solution;
      players = updated_from_move;
      winner = st.winner;
      current_turn = st.current_turn;
      losers = st.losers;
      num_suggestions = get_num_suggestions st;
     }, String.concat " " ["You suggested ("; 
                           card_to_string guess.suspect;
                           card_to_string guess.weapon;
                           card_to_string guess.room; 
                           ") and no one had any cards that matched.\n"; 
                           card_to_string guess.suspect; "has been moved to"; 
                           card_to_string guess.room ;"\n\n"])
  | (Some rev, asked) -> 
    let shown_actor = U.update_from_shown bd (Some rev,asked) user_actor guess  
    in
    ({solution = st.solution;
      players = update_player_actor updated_from_move u (User shown_actor) [] ;
      winner = st.winner;
      current_turn = st.current_turn;
      losers = st.losers;
      num_suggestions = get_num_suggestions st;
     }, String.concat " " ["You suggested ("; 
                           card_to_string guess.suspect;
                           card_to_string guess.weapon;
                           card_to_string guess.room; 
                           ") and were shown the card";card_to_string rev.shown; 
                           "\n by player";
                           card_to_string rev.answerer; "after asking";
                           if asked = [] then "no one else.\n" else 
                             (String.concat " and " 
                                ((card_list_to_string asked [])) ^ ".\n");
                           card_to_string guess.suspect ^ " has been moved to "^ 
                           card_to_string guess.room ^ ".\n\n";
                          ])


let bot_guess (st : t) (bd : Board.t) : (solution option * t) = 
  let bot_player = get_current_player (st.current_turn) (st.players) in
  if List.mem bot_player.suspect st.losers then (None, st) else
    match bot_player.actor with
    | User u -> raise (Failure "called a bot_guess on a user")
    | Easy b -> let guess_tuple = (E.make_guess bd b) in
      let updated_final = update_player_actor 
          (move_party_from_guess st.players (fst guess_tuple)) bot_player 
          (Easy (snd guess_tuple)) [] in
      (Some (fst guess_tuple), {
          solution = st.solution;
          players = updated_final;
          winner = st.winner;
          current_turn = st.current_turn;
          losers = st.losers;
          num_suggestions = get_num_suggestions st; 
        })
    | Medium m -> let guess_tuple = (M.make_guess bd m) in
      let updated_final = update_player_actor 
          (move_party_from_guess st.players (fst guess_tuple)) bot_player 
          (Medium (snd guess_tuple)) [] in
      (Some (fst guess_tuple), {
          solution = st.solution;
          players = updated_final;
          winner = st.winner;
          current_turn = st.current_turn;
          losers = st.losers;
          num_suggestions = get_num_suggestions st;
        })
    | Hard d -> let guess_tuple = (H.make_guess bd d) in
      let updated_final = update_player_actor 
          (move_party_from_guess st.players (fst guess_tuple)) bot_player 
          (Hard (snd guess_tuple)) [] in
      (Some (fst guess_tuple), {
          solution = st.solution;
          players = updated_final;
          winner = st.winner;
          current_turn = st.current_turn;
          losers = st.losers;
          num_suggestions = get_num_suggestions st;
        })

let hard_ai_turn (st : t) (bd : Board.t) : (t*string) = 
  let guess = bot_guess st bd in 
  let bot = get_current_player (snd guess).current_turn (snd guess).players in
  let after_bot = (match bot.actor with 
      | Hard h -> h
      | _ -> failwith "not hard bot"
    ) in
  if H.ready_to_accuse after_bot then match H.get_accusation after_bot with
    | None -> failwith "was not ready to accuse"
    | Some a -> let correct = a = st.solution in 
      let result = if not correct then "which was incorrect, so they lost the 
      game.\n"
        else "which was correct, so they won the game.\n\n" in
      ({solution = st.solution;
        players = st.players;
        winner = if correct then Some bot.suspect else None;
        current_turn = st.current_turn;
        losers = if not correct then bot.suspect :: st.losers else st.losers;
        num_suggestions = get_num_suggestions st;
       }, String.concat " " ["Player"; card_to_string  bot.suspect; "accused ("; 
                             card_to_string a.suspect;
                             card_to_string a.weapon;
                             card_to_string a.room; ")" ; result;
                             (* H.data_to_string after_bot *)
                            ])
  else 
    match fst guess with 
    | None -> (st,"")
    | Some g -> 
      let shown = check_neighbors_cards st bot st.players g [] 
          (go_neighbor bot st.players) in 
      let updated_bot = H.update_from_shown bd shown after_bot g in 
      let updated_players = update_player_actor (snd guess).players bot 
          (Hard updated_bot) [] in
      let user_move = {
        asker = get_suspect bot;
        guess = g;
        answerer = (match fst shown with 
            | None -> None
            | Some rev -> Some rev.answerer);
        asked = snd shown;
      } in
      let updated_suspect = move_party_from_guess updated_players g in
      let updated_from_move = update_actors_from_move updated_suspect user_move 
          [] in
      match shown with 
      | (None, _) -> 
        ({solution = st.solution;
          players = updated_from_move ;
          winner = st.winner;
          current_turn = st.current_turn;
          losers = st.losers;
          num_suggestions = get_num_suggestions st; 
         },String.concat " " ["Player"; card_to_string bot.suspect;"suggested ("
                              ; 
                              card_to_string g.suspect;
                              card_to_string g.weapon;
                              card_to_string g.room; 
                              ") and no one had any cards that matched.\n"; 
                              card_to_string g.suspect; "has been moved to"; 
                              card_to_string g.room ;"\n\n";
                              (* H.data_to_string after_bot *)
                             ])
      | (Some rev, asked) ->
        ({ solution = st.solution;
           players = updated_from_move ;
           winner = st.winner;
           current_turn = st.current_turn;
           losers = st.losers;
           num_suggestions = get_num_suggestions st; 
         },String.concat " " ["Player"; card_to_string bot.suspect; 
                              "suggested ("; 
                              card_to_string g.suspect;
                              card_to_string g.weapon;
                              card_to_string g.room; 
                              ") and was shown a card\n by player"; 
                              card_to_string rev.answerer; "after asking";
                              if asked = [] then "no one else.\n" else 
                                (String.concat " and " 
                                   ((card_list_to_string asked [])) ^ ".\n");
                              card_to_string g.suspect ^ " has been moved to " ^ 
                              card_to_string g.room ^ ".\n\n";
                              (* H.data_to_string after_bot *)
                             ])

let medium_ai_turn (st : t) (bd : Board.t): (t*string) = 
  let guess = bot_guess st bd in 
  let bot = get_current_player (snd guess).current_turn (snd guess).players in
  let after_bot = (match bot.actor with 
      | Medium m -> m
      | _ -> failwith "not hard bot"
    ) in
  if M.ready_to_accuse after_bot then match M.get_accusation after_bot with
    | None -> failwith "was not ready to accuse"
    | Some a -> let correct = a = st.solution in 
      let result = if not correct then "which was incorrect, so they lost the 
      game.\n"
        else "which was correct, so they won the game.\n\n" in
      ({solution = st.solution;
        players = st.players;
        winner = if correct then Some bot.suspect else None;
        current_turn = st.current_turn;
        losers = if not correct then bot.suspect :: st.losers else st.losers;
        num_suggestions = get_num_suggestions st; 
       }, String.concat " " ["Player"; card_to_string  bot.suspect; "accused ("; 
                             card_to_string a.suspect;
                             card_to_string a.weapon;
                             card_to_string a.room; ")" ; result;])
  else match fst guess with 
    | None -> (st,"")
    | Some g -> 
      let shown = check_neighbors_cards st bot st.players g [] 
          (go_neighbor bot st.players) in 
      let updated_bot = M.update_from_shown bd shown after_bot g in 
      let updated_players = update_player_actor (snd guess).players bot 
          (Medium updated_bot) [] in
      let user_move = {
        asker = get_suspect bot;
        guess = g;
        answerer = (match fst shown with 
            | None -> None
            | Some rev -> Some rev.answerer);
        asked = snd shown;
      } in
      let updated_suspect = move_party_from_guess updated_players g in
      let updated_from_move = update_actors_from_move updated_suspect user_move 
          [] in
      match shown with 
      | (None, _) -> 
        ({solution = st.solution;
          players = updated_from_move ;
          winner = st.winner;
          current_turn = st.current_turn;
          losers = st.losers;
          num_suggestions = get_num_suggestions st; 
         },String.concat " " ["Player"; card_to_string bot.suspect;"suggested ("
                              ; 
                              card_to_string g.suspect;
                              card_to_string g.weapon;
                              card_to_string g.room; 
                              ") and no one had any cards that matched.\n"; 
                              card_to_string g.suspect; "has been moved to"; 
                              card_to_string g.room ;"\n\n";])
      | (Some rev, asked) ->
        ({ solution = st.solution;
           players = updated_from_move ;
           winner = st.winner;
           current_turn = st.current_turn;
           losers = st.losers;
           num_suggestions = get_num_suggestions st; 
         },String.concat " " ["Player"; card_to_string bot.suspect; 
                              "suggested ("; 
                              card_to_string g.suspect;
                              card_to_string g.weapon;
                              card_to_string g.room; 
                              ") and was shown a card\n by player"; 
                              card_to_string rev.answerer; "after asking";
                              if asked = [] then "no one else.\n" else 
                                (String.concat " and " 
                                   ((card_list_to_string asked [])) ^ ".\n");
                              card_to_string g.suspect ^ " has been moved to " ^ 
                              card_to_string g.room ^ ".\n\n";
                             ])

let easy_ai_turn (st : t) (bd : Board.t): (t*string) = 
  let guess = bot_guess st bd in 
  let bot = get_current_player (snd guess).current_turn (snd guess).players in
  let after_bot = (match bot.actor with 
      | Easy e -> e
      | _ -> failwith "not hard bot"
    ) in
  if E.ready_to_accuse after_bot then match E.get_accusation after_bot with
    | None -> failwith "was not ready to accuse"
    | Some a -> let correct = a = st.solution in 
      let result = if not correct then "which was incorrect, so they lost the 
      game.\n"
        else "which was correct, so they won the game.\n\n" in
      ({solution = st.solution;
        players = st.players;
        winner = if correct then Some bot.suspect else None;
        current_turn = st.current_turn;
        losers = if not correct then bot.suspect :: st.losers else st.losers;
        num_suggestions = get_num_suggestions st; 
       }, String.concat " " ["Player"; card_to_string  bot.suspect; "accused ("; 
                             card_to_string a.suspect;
                             card_to_string a.weapon;
                             card_to_string a.room; ")" ; result;])
  else match fst guess with 
    | None -> (st,"")
    | Some g -> 
      let shown = check_neighbors_cards st bot st.players g [] 
          (go_neighbor bot st.players) in 
      let updated_bot = E.update_from_shown bd shown after_bot g in 
      let updated_players = update_player_actor (snd guess).players bot 
          (Easy updated_bot) [] in
      let user_move = {
        asker = get_suspect bot;
        guess = g;
        answerer = (match fst shown with 
            | None -> None
            | Some rev -> Some rev.answerer);
        asked = snd shown;
      } in
      let updated_suspect = move_party_from_guess updated_players g in
      let updated_from_move = update_actors_from_move updated_suspect user_move 
          [] in
      match shown with 
      | (None, _) -> 
        ({solution = st.solution;
          players = updated_from_move;
          winner = st.winner;
          current_turn = st.current_turn;
          losers = st.losers;
          num_suggestions = get_num_suggestions st; 
         },String.concat " " ["Player"; card_to_string bot.suspect;"suggested ("
                              ; 
                              card_to_string g.suspect;
                              card_to_string g.weapon;
                              card_to_string g.room; 
                              ") and no one had any cards that matched.\n"; 
                              card_to_string g.suspect; "has been moved to"; 
                              card_to_string g.room ;"\n\n";])
      | (Some rev, asked) ->
        ({ solution = st.solution;
           players = updated_from_move;
           winner = st.winner;
           current_turn = st.current_turn;
           losers = st.losers;
           num_suggestions = get_num_suggestions st; 
         },String.concat " " ["Player"; card_to_string bot.suspect; 
                              "suggested ("; 
                              card_to_string g.suspect;
                              card_to_string g.weapon;
                              card_to_string g.room; 
                              ") and was shown a card \n by player"; 
                              card_to_string rev.answerer; "after asking";
                              if asked = [] then "no one else.\n" else 
                                (String.concat " and " 
                                   ((card_list_to_string asked [])) ^ ".\n");
                              card_to_string g.suspect ^ " has been moved to " ^ 
                              card_to_string g.room ^ ".\n\n";
                             ])

let ai_turn (st : t) (bd : Board.t) : (t * string) = 
  let bot = get_current_player st.current_turn st.players in
  if List.mem (get_suspect bot) st.losers then 
    (st ,  "Player "^(card_to_string bot.suspect)^"'s turn is skipped because"^
           " they already lost the game.") else
    match bot.actor with 
    | Easy b -> easy_ai_turn st bd
    | Medium m -> medium_ai_turn st bd
    | User u -> failwith "AI call for user" 
    | Hard d -> hard_ai_turn st bd

let valid_weapon (s : string) (bd : Board.t) : bool = 
  List.mem (Weapon s) (get_weapons bd)  

let valid_suspect (s : string) (bd : Board.t) : bool= 
  List.mem (Suspect s) (get_suspects bd)  

let valid_room (s : string) (bd : Board.t) : bool = 
  List.mem (Room s) (get_rooms bd)

let valid_card (s : string) (bd : Board.t) : bool = 
  let card_lst = card_list_to_string (get_cards bd) [] in
  List.mem s card_lst

let weapon_solution (s : solution) : string = 
  match s.weapon with 
  | Weapon x -> x 
  | _ -> failwith "Weapon is not a weapon card"

let suspect_solution (s : solution) : string = 
  match s.suspect with 
  | Suspect x -> x 
  | _ -> failwith "Suspect is not a suspect card"

let room_solution (s : solution) : string = 
  match s.room with 
  | Room x -> x
  | _ -> failwith "Room is not a room card"

let valid_guess (lst : string list) (bd : Board.t) (st : t) : bool = 
  if (List.length lst < 2) then false else 
    (valid_weapon (List.nth lst 1) bd && valid_suspect (List.nth lst 0) bd 
     && (List.nth lst 2) = current_player_room_str st)

let valid_solution (lst : string list) (bd : Board.t) (st : t) : bool = 
  if (List.length lst < 2) then false else 
    ((List.nth lst 0 = suspect_solution st.solution) &&
     (List.nth lst 1 = weapon_solution st.solution) &&
     (List.nth lst 2 = room_solution st.solution))

let create_solution (str : string list) : solution = 
  { suspect =  Suspect (List.nth str 0); 
    weapon = Weapon (List.nth str 1); 
    room = Room (List.nth str 2); 
  }

let get_solution_str (st : t) (acc : string list) : string = 
  let lst = [suspect_solution st.solution]@[weapon_solution st.solution]@
            [room_solution st.solution]
  in String.concat " " lst 

let user_to_cards (st : t) : string list = 
  let user = get_user_player st.players in
  match user.actor with 
  | User u -> card_list_to_string (U.get_hand u) []
  | _ -> failwith "get_user returned a bot"

let rec all_cards_helper (st : t) (players : player list) (acc : string list)
  : string list  = 
  match players with 
  | [] -> acc 
  | h::t -> all_cards_helper st t (acc@["//"]@[card_to_string h.suspect]@[":"]
                                   @(get_players_hand h)) 

let all_cards (st : t) : string list  = 
  all_cards_helper st (get_players st) [] 

let get_winner_string (st : t) : string = 
  match st.winner with
  | None -> ""
  | Some w ->  card_to_string w

let print_passages (bd : Board.t): string = 
  let rec pass_print_help (passages: passage list) (acc: string): string = 
    match passages with
    | [] -> acc
    | (r1,r2)::t -> pass_print_help t (String.concat "  " [
        ((card_to_string r1)^"<->"^(card_to_string r2));
        acc])
  in pass_print_help (get_passages bd) ""

let card_option_to_string (c : card option) : string =
  match c with 
  | None -> ""
  | Some Weapon w -> w
  | Some Room r -> r
  | Some Suspect s -> s

let rec player_locations_string (players : player list) (acc : string): string = 
  match players with 
  | [] -> acc
  | p::t -> player_locations_string t ((get_suspect p |> card_to_string)
                                       ^" is in the " 
                                       ^(get_player_room p |> card_to_string)
                                       ^". " ^ acc)

let form_solution (w : card) (s : card) (r : card) : solution =
  {weapon=w;suspect=s;room=r;}                                     

let get_current_player_card (st : t) : card =
  get_suspect (get_current_player st.current_turn st.players)

let update_turn (st : t) : t =  
  {solution = get_solution st;
   players = get_players st; 
   winner = get_winner st;
   current_turn = (increment_turn (get_current_turn st) st);
   losers = st.losers;
   num_suggestions = get_num_suggestions st; 
  }

let current_player_room (st : t) : card = 
  let p = (get_current_player st.current_turn st.players) in 
  match p.actor with 
  | User u -> U.get_current_room u
  | Easy e -> E.get_current_room e
  | Medium m -> M.get_current_room m
  | Hard d -> H.get_current_room d

let current_player_string (st : t) : string = 
  get_suspect (get_current_player st.current_turn st.players) |> card_to_string

let valid_move (str : string) (st : t) (bd : Board.t) : bool = 
  let valid_rooms = lst_rooms (get_rooms bd) st [] in
  let new_str = String.trim str in 
  List.mem new_str valid_rooms 

let update_current_room (st : t) (room : card) : t = 
  let p = move_player (get_current_player st.current_turn st.players) room in
  {solution = get_solution st;
   players = update_player st.players p []; 
   winner = get_winner st;
   current_turn = get_current_turn st;
   losers = st.losers;
   num_suggestions = get_num_suggestions st; 
  }

let rec get_characters (st : t) (players : player list) (acc : string list) = 
  match players with 
  | [] -> acc 
  | h::t -> get_characters st t (card_to_string(get_suspect h)::acc)  

let get_characters_to_string (st : t) = 
  String.concat ", " (get_characters st st.players []) 

let get_data (p : player) = 
  match p.actor with 
  | User z -> (U.get_data z)  
  | _ -> failwith "bot has notes"

let rec notes_to_string (notes : (string * string) list) (acc : string list) 
    (st: t) = 
  match notes with 
  | [] -> acc 
  | (x, y)::t -> 
    if ((List.mem x (card_list_to_string 
                       (get_data (get_user_player (get_players st))) [])) 
        && (not (List.mem x (card_list_to_string 
                               (get_players_cards (get_user_player 
                                                     (get_players st))) []))))
    then notes_to_string t ((x ^ " " ^ "X" ^ y)::acc) st 
    else
      notes_to_string t ((x ^ " " ^ y)::acc) st   