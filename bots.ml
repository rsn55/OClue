open Board

(** A move from one room to another. *)
type room_change = (room * room) option

(** All that a player can observe about a move made by another player.  *)
type move = {
  asker: card;
  guess: solution;
  answerer: card option;
  asked: card list;
}

(** When a player [answerer] reveals a card [shown] to another  *)
type reveal = {
  shown: card;
  answerer: card;
}

(** Instance of information about a player  *)
type player_info = {
  character: card;
  hand: card list;
  current_room: card;
  accusation: solution option
}



let rec create_set (acc : 'a list) (lst : 'a list) : 'a list = 
  match lst with
  |[] -> acc
  |h::t -> if (List.mem h acc) then (create_set acc t) else 
      create_set (h::acc) t

(** The simplest implementation of data storage: a list of eliminated cards. *)

let roll_dice (u : unit) : int = 
  (Random.int 6) + 1

let random_card (l : card list) : (card * card list) = 
  let selected = List.nth l (Random.int (List.length l)) in
  (selected, List.filter (fun x -> x != selected) l)

(* gets a random card that is not in the player's hand and has not been 
   eliminated by them. if no such card of this type exists, 
   it chooses any card. *)
let guess_random_card (eliminated : card list) (all_cards : card list) : card =
  let not_eliminated = List.filter (fun x -> not (List.mem x eliminated)) 
      all_cards in
  match not_eliminated with
  | [] -> fst (random_card all_cards)
  | h::t -> fst (random_card not_eliminated)

module type BotPlayer = sig
  type t
  val initialize: card list -> card list -> card list -> card -> card -> t
  val get_data : t -> string list 
  val update_data : t -> card list -> t 
  val get_info: t -> player_info
  val get_character: t -> card
  val get_hand: t -> card list
  val get_current_room: t -> card
  val get_accusation: t -> solution option
  val ready_to_accuse: t -> bool
  val add_card_to_hand: t -> card -> t
  val make_guess: Board.t -> t -> (solution * t)
  val update_from_move: move -> t -> t
  val update_from_shown: Board.t -> (reveal option * Board.card list) -> t ->
    Board.solution  -> t
  val card_request: solution -> t -> reveal option
  val set_accusation: Board.t -> t -> t
  val update_room: card -> t -> t
  val data_to_string: t -> string
end

module type UserPlayer = sig
  type t
  val initialize: card -> card -> t
  val get_data: t -> card list
  val get_info: t -> player_info
  val get_character: t -> card
  val get_hand: t -> card list
  val get_current_room: t -> card
  val add_card_to_hand: t -> card -> t
  val update_from_move: move -> t -> t
  val update_from_shown: Board.t -> (reveal option * Board.card list) -> t 
    -> Board.solution -> t
  val update_room: card -> t -> t
  val card_request: solution -> t -> reveal option

end

(** The easiest level of AI, only remembers cards directly shown to it.  *)
module EasyBot: BotPlayer = struct
  type t = {data: card list; info: player_info}


  let add_card_to_hand (p : t) (c : card) : t = 
    {data = c :: p.data;
     info = {
       character = p.info.character;
       hand = c :: p.info.hand;
       current_room = p.info.current_room;
       accusation = p.info.accusation
     } 
    }


  (** Initialize the player info  *)
  let initialize (suspects : card list) (weapons : card list) 
      (rooms : card list) (character : card) (room : card) : t = 
    {data=[];
     info={
       character = character;
       hand = [];
       current_room = room;
       accusation = None
     }
    }

  let rec card_list_to_string (c : card list) (acc : string list) 
    : string list = 
    match c with 
    | [] -> acc
    | h::t -> card_list_to_string t (card_to_string h :: acc)

  let get_data (player : t) : string list = 
    card_list_to_string (player.data) [] 

  let update_data (p : t) (lst : card list) = 
    {data = lst;
     info={
       character = p.info.character;
       hand = p.info.hand;
       current_room = p.info.current_room;
       accusation = p.info.accusation
     }
    }

  let get_info (player : t) : player_info = 
    player.info

  let get_current_room (player : t) : card = 
    player.info.current_room

  (** The character that is representing a player [t] *)
  let get_character (player : t) : card = 
    (player.info).character

  (** The character that is representing a player [t] *)
  let get_hand (player : t) : card list = 
    (player.info).hand

  (** Move player from one room to another. *)
  let update_room (room : card) (pl : t) : t =
    {data = pl.data;
     info = {
       character = pl.info.character;
       hand = pl.info.hand;
       current_room = room;
       accusation = pl.info.accusation
     }
    }

  (* find passage *)
  let rec find_passage (p : passage list) (room : card) : card option =
    match p with
    | [] -> None
    | (r1,r2)::t -> if r1 = room then Some r2 else if 
        r2 = room then Some r1 else find_passage t room

  (** chooses if bot should enter secret passage *)
  let decide_passage (bd : Board.t) (pl : t) : card =
    match find_passage (get_passages bd) (get_current_room pl) with
    | None -> (get_current_room pl)
    | Some r -> if not (List.mem r (pl.data)) then r 
      else (get_current_room pl)

  let find_solution (bd : Board.t) (p : t) : solution =
    let r = guess_random_card (p.data) (get_rooms bd) in 
    let w = guess_random_card (p.data) (get_weapons bd) in 
    let s = guess_random_card (p.data) (get_suspects bd) in 
    {suspect=s;room=r;weapon=w}

  let set_accusation (bd : Board.t) (p : t) : t =
    if List.length (get_suspects bd) + List.length (get_weapons bd) +
       List.length (get_rooms bd) - 3 = List.length p.data then
      {data = p.data;
       info = {
         character = p.info.character;
         hand = p.info.hand;
         current_room = p.info.current_room;
         accusation = Some (find_solution bd p)
       } 
      } else p

  let get_accusation (p : t) : solution option = p.info.accusation

  let ready_to_accuse (p : t) : bool = match p.info.accusation with 
    | None -> false
    | Some a -> true

  let make_guess (bd : Board.t) (p : t) : (solution * t) =
    let guess_room = if roll_dice (Random.self_init()) >= 3 then
        guess_random_card (p.data) (get_rooms bd) else 
        decide_passage bd p in
    let guess_suspect = guess_random_card (p.data) (get_suspects bd) in 
    let guess_weapon = guess_random_card (p.data) (get_weapons bd) in 
    let turn_guess = {weapon=guess_weapon;suspect=guess_suspect;room=guess_room} 
    in (turn_guess, set_accusation bd (update_room guess_room p))

  (** Updates data after observing another player's [move]  *)
  let update_from_move (move : move) (player : t) : t = 
    player

  (** Updates data after being shown a card [shown]  *)
  let update_from_shown (bd : Board.t) 
      (shown : (reveal option * Board.card list)) (player: t) 
      (guess: solution) : t =
    match fst shown with
    | None -> player
    | Some a ->
      set_accusation bd { data = a.shown :: player.data;
                          info = player.info }

  let rec card_list_to_string (lst : card list) (acc : string) : string = 
    match lst with 
    | [] -> acc 
    | h::t -> card_list_to_string t (String.concat ", " 
                                       ((card_to_string h)::[acc])) 

  (** A response to another player's card request [guess]. *)
  let card_request (guess : solution) (player : t) : reveal option =
    if List.mem (guess.weapon) player.info.hand then
      Some {shown=guess.weapon;
            answerer=get_character player} else if 
      List.mem (guess.room) player.info.hand then
      Some {shown=guess.room;
            answerer=get_character player} else if
      List.mem (guess.suspect) player.info.hand then
      Some {shown=guess.suspect;
            answerer=get_character player} else
      None

  let data_to_string (p:t) = 
    card_list_to_string p.data ""
end

type probability = {
  rooms: (card * float) list;
  weapons:(card * float) list;
  suspects: (card * float) list;
}

module HardData = struct
  type t = {
    players_known_cards:(card * card list) list;
    players_dont_have: (card * card list) list;
    shown_cards_to: (card * card list) list;
    has_one_of: (card * card list list) list;
    eliminated: card list;
    all_suspects: card list;
    all_weapons: card list;
    all_rooms: card list;
    probability: probability
  }

  let rec cnt_occur (c: card) (cat: (card * card list) list) (acc: int)
    : int =
    match cat with
    | []->acc
    | (a,b)::t -> if List.mem c b then cnt_occur c t (acc+1) else
        cnt_occur c t acc

  let rec count_known_of_type (known: card list)
      (card_type: string) : int =
    let rec count_rooms (lst: card list) (acc: int) : int =
      match lst with
      | [] -> acc
      | Room r::t -> count_rooms t (acc+1)
      | h::t -> count_rooms t acc
    in 
    let rec count_weapons(lst: card list) (acc: int) : int =
      match lst with
      | [] -> acc
      | Weapon r::t -> count_weapons t (acc+1)
      | h::t -> count_weapons t acc
    in 
    let rec count_suspects (lst: card list) (acc: int) : int =
      match lst with
      | [] -> acc
      | Suspect r::t -> count_suspects t (acc+1)
      | h::t -> count_suspects t acc
    in 
    if card_type = "Room" then count_rooms known 0 else 
    if card_type = "Weapon" then count_weapons known 0 else 
      count_suspects known 0

  let max_cards (cat:string) (d:t) : float =
    let num_suspects = float_of_int (List.length d.all_suspects) in
    let num_weapons = float_of_int (List.length d.all_weapons) in
    let num_rooms = float_of_int (List.length d.all_rooms) in
    match cat with
    | "room" -> (num_rooms -. 1.0) /. num_suspects |> ceil
    | "weapon" -> (num_weapons -. 1.0) /. num_suspects |> ceil
    | "suspect" -> (num_suspects -. 1.0) /. num_suspects |> ceil
    | _ -> failwith "malformed"

  let rec diff (l1: card list) (l2: card list) (acc: card list): card list =
    match l1 with 
    | [] -> acc
    | h::t -> if List.mem h l2 then diff t l2 acc else diff t l2 (h::acc)

  let rec get_cards_of (cat : (card * card list) list) (p : card) : card list =
    match cat with 
    | [] -> failwith "no such player in list"
    | h::t -> if fst h = p then snd h else get_cards_of t p

  let poss_in_hand (d:t) (p:card) (c:card) (no: card list): card list = 
    match c with 
    | Room r -> diff (diff 
                        (diff d.all_rooms (get_cards_of d.players_dont_have p) 
                           []) 
                        no []) (get_cards_of d.players_known_cards p) []
    | Weapon w -> diff (diff 
                          (diff d.all_weapons 
                             (get_cards_of d.players_dont_have p)
                             []) no [])  
                    (get_cards_of d.players_known_cards p) []
    | Suspect s -> diff (diff (diff d.all_suspects 
                                 (get_cards_of d.players_dont_have p) []) no [])
                     (get_cards_of d.players_known_cards p) []

  let get_non_eliminated (d:t) (c: card) : card list =
    match c with 
    | Room r -> diff d.all_rooms d.eliminated []
    | Weapon w -> diff d.all_weapons d.eliminated []
    | Suspect s -> diff d.all_suspects d.eliminated []


  (* worst case scenario num of possible spots a card could be in
     for calculating probability *)
  let indiv_prob (c: card) (d:t) : float =
    let dont_have = cnt_occur c d.players_dont_have 0 in
    let num_players = List.length d.all_suspects in
    if List.mem c d.eliminated then 0.0 
    else if dont_have = num_players then 1.0 
    else if List.length (get_non_eliminated d c) = 1 then 1.0
    else 1.0 /.((float_of_int num_players)-.(float_of_int dont_have)+.1.0)

  (* takes 2 cards and chooses the one with highest probability  *)
  let compare_prob (c1:card) (c2:card) (d:t) : card =
    let prob1 = indiv_prob c1 d in 
    let prob2 = indiv_prob c2 d in 
    if prob1 > prob2 then c1 else c2

  let rec max_prob (prob_lst: (card * float) list) :  (card * float) =
    let rec max_help prob maxlst mx  = 
      match prob with
      | [] -> (guess_random_card [] maxlst, mx)
      | (c,p)::t -> if p > mx then max_help t [c] p else
        if p = mx then max_help t (c::maxlst) p else
          max_help t maxlst mx
    in 
    max_help prob_lst [] 0.0

  let rec second_max (prob_lst: (card * float) list) (max: card) 
      (sec: (card * float)): (card * float) =
    match prob_lst with
    | [] -> sec
    | (c,p)::t -> if p > (snd sec) && max <> (fst sec) 
      then second_max t max (c,p) else second_max t max sec

  let rec get_hand_room (hand: card list) : card option =
    match hand with 
    | [] -> None
    | Room r::t -> Some (Room r)
    | h::t -> get_hand_room t

  let rec get_hand_weapon (hand: card list) : card option =
    match hand with 
    | [] -> None
    | Weapon r::t -> Some (Weapon r)
    | h::t -> get_hand_weapon t

  let rec get_hand_suspect (hand: card list) : card option =
    match hand with 
    | [] -> None
    | Suspect r::t -> Some (Suspect r)
    | h::t -> get_hand_suspect t

  let flip (u: unit) : bool = 
    let flipped = Random.int 1 in 
    if flipped = 1 then true else false

  let probable_guess (hand: card list) (d:t) : solution =
    let prob = d.probability in
    let room_pick = (let mx = max_prob prob.rooms in 
                     match snd mx with
                     | 1.0 -> (match get_hand_room hand with 
                         | None -> fst mx
                         | Some r -> r)
                     | p when p >= 0.3 -> if flip (Random.self_init()) then 
                         fst (second_max prob.rooms (fst mx) 
                                (Room "error", 0.0))
                       else fst mx
                     | _ -> fst mx
                    ) in 
    let weapon_pick = (let mx = max_prob prob.weapons in 
                       match snd mx with
                       | 1.0 -> (match get_hand_weapon hand with 
                           | None -> fst mx
                           | Some r -> r)
                       | p when p >= 0.3 -> if flip (Random.self_init()) then 
                           fst (second_max prob.weapons (fst mx) 
                                  (Weapon "error", 0.0))  else fst mx
                       | _ -> fst mx
                      ) in
    let sus_pick = (let mx = max_prob prob.suspects  in 
                    match snd mx with
                    | 1.0 -> (match get_hand_suspect hand with 
                        | None -> fst mx
                        | Some r -> r)
                    | p when p >= 0.3 -> if flip (Random.self_init()) then 
                        fst (second_max prob.suspects (fst mx)
                               (Suspect "error", 0.0)) else fst mx
                    | _ -> fst mx
                   ) in
    {room = room_pick;
     weapon = weapon_pick;
     suspect = sus_pick;
    }

  let probable_solution (d:t) : (solution * float) =
    let prob = d.probability in
    let room_pick = max_prob prob.rooms  in 
    let weapon_pick = max_prob prob.weapons  in 
    let sus_pick = max_prob prob.suspects in 
    ({room = fst room_pick;
      weapon = fst weapon_pick;
      suspect = fst sus_pick;
     }, (snd room_pick) *. (snd weapon_pick) *. (snd sus_pick))

  let rec calc_lst_prob (cards: (card * float) list) (d:t)
      (acc: (card * float) list): (card * float) list =
    match cards with
    | [] -> acc
    | h::t -> calc_lst_prob t d ((fst h,indiv_prob (fst h) d)::acc)


  let calculate_probability (d:t) : t =
    let new_prob = {
      rooms = calc_lst_prob d.probability.rooms d [];
      weapons = calc_lst_prob d.probability.weapons d [];
      suspects = calc_lst_prob d.probability.suspects d [];
    } in {d with probability = new_prob;}


  let rec player_list_init (suspects : card list) 
      (acc : (card * 'a list) list) : (card * 'a list) list = 
    match suspects with
    | [] -> acc
    | h::t -> player_list_init t ((h,[])::acc)

  let rec add_card_to_data_list (playerlst : (card * card list) list) 
      (p : card) (c : card) (acc : (card * card list) list) 
    : (card * card list) list =
    match playerlst with
    | [] -> acc
    | h::t -> if fst h = p then 
        add_card_to_data_list t p c ((fst h, create_set [] (c::snd h) )::acc) 
      else
        add_card_to_data_list t p c (h::acc)

  let rec add_cards_to_data_list (playerlst : (card * card list) list) 
      (p : card) (c : card list) (acc : (card * card list) list) 
    : (card * card list) list =
    match playerlst with
    | [] -> acc
    | h::t -> if fst h = p then 
        add_cards_to_data_list t p c ((fst h, create_set [] (c @ snd h))::acc)
      else add_cards_to_data_list t p c (h::acc)

  let rec replace_players_cards playerlst p (c:card list) acc =
    match playerlst with
    | [] -> acc
    | h::t -> if fst h = p then replace_players_cards t p c ((fst h, c):: acc)
      else replace_players_cards t p c (h :: acc)

  let rec lst_to_prob (cards: card list) (acc: (card * float) list) 
    : (card * float) list =
    match cards with
    | [] -> acc
    | h::t -> lst_to_prob t ((h,0.0)::acc)

  let empty suspects weapons rooms p : t = 
    let player_list = player_list_init suspects [] in
    let player_has_none = replace_players_cards player_list p 
        (suspects@weapons@rooms) [] in
    {
      players_known_cards = player_list;
      players_dont_have = player_has_none;
      shown_cards_to = [];
      eliminated = [];
      all_suspects = suspects;
      all_weapons = weapons;
      all_rooms = rooms;
      has_one_of = player_list;
      probability = {
        rooms = lst_to_prob rooms [];
        weapons = lst_to_prob weapons [];
        suspects =  lst_to_prob suspects [];
      }
    }


  let rec add_card_to_all_players_except (cat: (card * card list) list) 
      (p:card) (c: card)(acc: (card * card list) list) : (card * card list) 
      list =
    match cat with 
    | [] -> acc
    | h::t -> if fst h = p then add_card_to_all_players_except t p c (h::acc)
      else
        add_card_to_all_players_except t p c
          ((fst h, create_set [] (c::snd h) )::acc)

  let rec remove_dont_helper cat p c acc : (card * card list) list = 
    let rec remove lst el acc =
      match lst with
      | [] -> acc
      | h::t -> if h=el then remove t el acc else remove t el (h::acc)
    in
    match cat with
    | [] -> acc
    | h::t -> if fst h = p then remove_dont_helper t p c
          ((fst h, remove (snd h) c [])::acc) else
        remove_dont_helper t p c (h::acc)

  let remove_from_dont (d:t) (p: card) (c: card) : t = 
    let new_dont = remove_dont_helper d.players_dont_have p c [] in
    {d with players_dont_have = new_dont; }

  let remove (i: 'a) (lst : 'a list) : 'a list = 
    let rec rmv_help l acc = 
      match l with 
      | [] -> acc
      | h::t -> if h=i then rmv_help t acc else rmv_help t (h::acc) in 
    rmv_help lst []

  let add_has_one_of (d:t) (p:card) (cards: card list) : t = 
    let rec has_help old_has acc : (card * card list list) list =
      match old_has with 
      | [] -> acc 
      | (sus,has)::t -> if sus=p then has_help t ((sus,cards::has)::acc) else 
          has_help t ((sus,has)::acc)
    in 
    {d with has_one_of = has_help d.has_one_of []}

  let delete_has_one_of (d_has:(card * card list list) list) (p:card) (c: card): 
    (card * card list list) list = 
    let rec del_if_con (l: card list list) (acc: card list list) =
      match l with 
      | [] -> acc 
      | h::t -> if List.mem c h || List.length h = 0 then del_if_con t acc else 
          del_if_con t (h::acc) in 
    let rec edit_if_con (l: card list list) (acc: card list list) =
      match l with 
      | [] -> acc 
      | h::t -> if List.mem c h then edit_if_con t ((remove c h)::acc)
        else edit_if_con t (h::acc) in 
    let rec has_help old_has acc : (card * card list list) list =
      match old_has with 
      | [] -> acc 
      | (sus,has)::t -> if sus=p then has_help t 
            ((sus,del_if_con has [])::acc) else 
          has_help t ((sus,edit_if_con has [])::acc)
    in 
    has_help d_has []

  let card_location_known (d : t) (p : card) (c : card) : t =
    let new_known = add_card_to_data_list d.players_known_cards p c [] in
    let new_dont = add_card_to_all_players_except d.players_dont_have p c [] in
    let new_has = delete_has_one_of d.has_one_of p c in
    calculate_probability 
      {d with players_known_cards = new_known;
              players_dont_have = new_dont;
              eliminated = create_set [] (c :: d.eliminated);
              has_one_of = new_has;}

  let dont_implies_has (d:t) (p:card) (dont:card) : t = 
    let rec edit_if_con (l: card list list) (acc: card list list) =
      match l with 
      | [] -> acc 
      | h::t -> if List.mem dont h then edit_if_con t ((remove dont h)::acc)
        else edit_if_con t (h::acc) in 
    let rec get_ones (old: (card list list)) implied : card list = 
      match old with 
      | [] -> implied 
      | h::t -> if List.length h = 1 then get_ones t ((List.hd h)::implied) else
          get_ones t implied
    in
    let rec get_implied old_has : card list =
      match old_has with 
      | [] -> failwith "no such player"
      | (sus,has)::t -> if sus=p then get_ones (edit_if_con has []) []
        else get_implied t 
    in 
    let rec apply_known td has : t = 
      match has with
      | [] -> td 
      | h::t -> apply_known (card_location_known td p h) t 
    in 
    apply_known d (get_implied d.has_one_of)

  let dont_have_known (d : t) (p : card) (c : card list ) : t =
    let new_dont = add_cards_to_data_list d.players_dont_have p c [] in
    let rec dont_have_all (cards: card list) (td: t) : t = 
      match cards with 
      | [] -> td 
      | h::t -> dont_have_all t (dont_implies_has td p h)
    in
    dont_have_all c (calculate_probability
                       {d with players_dont_have = new_dont;})

  let showed_card_to (d : t) (p : card) (c : card) : t =
    let new_shown = add_card_to_data_list d.shown_cards_to c p [] in
    {d with shown_cards_to = new_shown;}
end

module HardBot: BotPlayer = struct

  type t = {data: HardData.t; info: player_info}

  let add_card_to_hand (p:t) (c:card): t = 
    {data = HardData.remove_from_dont
         (HardData.card_location_known p.data p.info.character c) 
         p.info.character c;
     info = {
       character = p.info.character;
       hand = c :: p.info.hand;
       current_room = p.info.current_room;
       accusation = p.info.accusation
     } 
    }

  (** Initialize the player info  *)
  let initialize suspects weapons rooms character room: t = 
    {data= HardData.empty suspects weapons rooms character;
     info={
       character = character;
       hand = [];
       current_room = room;
       accusation = None
     }
    }

  let get_data (player : t) : string list = 
    []

  let update_data (player: t) (lst : card list) = 
    player 

  let get_info (player : t) : player_info = 
    player.info

  let get_current_room (player : t) : card = 
    player.info.current_room

  (** The character that is representing a player [t] *)
  let get_character (player : t) : card = 
    (player.info).character

  (** The character that is representing a player [t] *)
  let get_hand (player : t) : card list = 
    (player.info).hand

  (** Move player from one room to another. *)
  let update_room (room : card) (pl : t) : t =
    {data = pl.data;
     info = {
       character = pl.info.character;
       hand = pl.info.hand;
       current_room = room;
       accusation = pl.info.accusation
     }
    }

  (* find passage *)
  let rec find_passage (p : passage list) (room : card) : card option =
    match p with
    | [] -> None
    | (r1,r2)::t -> if r1 = room then Some r2 else if 
        r2 = room then Some r1 else find_passage t room

  (** chooses if bot should enter secret passage *)
  let decide_passage (bd : Board.t) (pl : t) : card =
    match find_passage (get_passages bd) (get_current_room pl) with
    | None -> (get_current_room pl)
    | Some r -> HardData.compare_prob r pl.info.current_room pl.data

  let find_solution (bd : Board.t) (p : t) : solution option =
    let sol = HardData.probable_solution p.data in 
    if snd sol >= 0.49 then Some (fst sol) else None

  let set_accusation (bd : Board.t) (p : t) : t =
    {data = p.data;
     info = {
       character = p.info.character;
       hand = p.info.hand;
       current_room = p.info.current_room;
       accusation = find_solution bd p
     } 
    } 

  let get_accusation (p: t) : solution option = p.info.accusation

  let ready_to_accuse (p : t) : bool = 
    match p.info.accusation with 
    | None -> false
    | Some a -> true

  let make_guess (bd: Board.t) (p: t): (solution * t) =
    let best_guess = HardData.probable_guess p.info.hand p.data in 
    let guess_room = if roll_dice (Random.self_init()) >= 3 then
        best_guess.room else 
        decide_passage bd p in
    let guess_suspect = best_guess.suspect in
    let guess_weapon =  best_guess.weapon in
    let turn_guess = {weapon=guess_weapon;suspect=guess_suspect;room=guess_room} 
    in (turn_guess, set_accusation bd (update_room guess_room p))

  let rec add_cards_to_unknown (p : t) (asked : card list) (guess : solution) 
    : t =
    match asked with
    | [] -> p
    | h::t -> add_cards_to_unknown { 
        data = HardData.dont_have_known p.data h 
            [guess.weapon; guess.suspect; guess.room]; 
        info=p.info;
      } t guess

  let rec add_card_lst_to_unknown (p : t) (asked : card list)
      (cards : card list) : t =
    match asked with
    | [] -> p
    | h::t -> add_card_lst_to_unknown { 
        data = HardData.dont_have_known p.data h cards; 
        info=p.info;
      } t cards

  let rec any_mem (c : card list) (lst : card list) : bool =
    match c with
    | [] -> false
    | h::t -> if List.mem h lst then true else any_mem t lst

  let rec check_poss_cards (guess : card list) (doesnt : card list)
      (acc: card list) : card list =
    match guess with 
    | [] -> acc
    | h::t -> if List.mem h doesnt then check_poss_cards t doesnt acc else 
        check_poss_cards t doesnt (h::acc)

  let rec check_answerer (p : t) (sus : card) (guess : solution) : t =
    let guess_lst = [guess.weapon; guess.suspect; guess.room] in
    let doesnt_have = HardData.get_cards_of p.data.players_dont_have sus in
    if any_mem guess_lst
        (HardData.get_cards_of p.data.players_known_cards sus)then p else 
      match check_poss_cards guess_lst doesnt_have [] with 
      | [] -> failwith "answer must have one of these"
      | h::[] -> {p with data = (HardData.card_location_known p.data sus h) }
      | e -> {p with data = HardData.add_has_one_of p.data sus e}

  (** Updates data after observing another player's [move]  *)
  let update_from_move (move: move) (p: t) : t = 
    if move.asker = p.info.character then p else
      let added_unknown = add_cards_to_unknown p move.asked move.guess in
      match move.answerer with
      | None -> added_unknown
      | Some a -> check_answerer added_unknown a move.guess

  let card_eliminated (p:t) (c:card): bool =
    List.mem c p.data.eliminated

  (** Updates data after being shown a card [shown]  *)
  let update_from_shown (bd : Board.t) 
      (shown : (reveal option * Board.card list)) (p: t) (guess: solution) : t =
    match fst shown with
    | None -> add_card_lst_to_unknown p (snd shown) 
                [guess.suspect;guess.weapon]
    | Some rev ->
      let card_known =  HardData.card_location_known p.data 
          rev.answerer rev.shown in 
      let set_asked_unknown = add_cards_to_unknown 
          { data = card_known;info = p.info } (snd shown) guess in 
      set_accusation bd set_asked_unknown

  let rec card_list_to_string (lst : card list) (acc : string) = 
    match lst with 
    | [] -> acc 
    | h::t -> card_list_to_string t (String.concat ", " 
                                       ((card_to_string h)::[acc])) 

  (** A response to another player's card request [guess]. *)
  let card_request (guess : solution) (player : t) : reveal option =
    if List.mem (guess.weapon) player.info.hand then
      Some {shown=guess.weapon;
            answerer=get_character player} else if 
      List.mem (guess.room) player.info.hand then
      Some {shown=guess.room;
            answerer=get_character player} else if
      List.mem (guess.suspect) player.info.hand then
      Some {shown=guess.suspect;
            answerer=get_character player} else
      None

  let rec med_data_str (cat: (card * card list) list) (acc:string) : string =
    match cat with
    | [] -> acc
    | h::t -> med_data_str t (card_to_string (fst h))^": "
              ^(card_list_to_string (snd h) "")^" // "^acc

  let rnd_float_str (f: float) : string =
    let fl = (floor (f*.100.0)) /. 100.0 in string_of_float fl

  let rec prob_data_str (prob: (card * float) list) (acc:string) : string =
    match prob with
    | [] -> acc
    | h::t -> prob_data_str t (card_to_string (fst h))^": "
              ^(rnd_float_str (snd h) )^" // "^acc    

  let rec has_one_str (has: (card * card list list) list) (acc:string)
    : string =
    let rec card_lst_lst_str (has_cards: card list list) (a:string) : string = 
      match has_cards with 
      | [] -> a 
      | h::t -> card_lst_lst_str t ((card_list_to_string h "")^" & "^a)
    in
    match has with
    | [] -> acc
    | h::t -> has_one_str t (card_to_string (fst h))^": "
              ^(card_lst_lst_str (snd h) "")^" // "^acc    

  let solution_to_string (sol: solution) : string = 
    match sol with 
    | {room=Room r;weapon=Weapon w;suspect=Suspect s} 
      -> r^" "^w^" "^s
    | _ -> failwith "not proper solution"

  let data_to_string (p: t) : string = 
    (* let sol = HardData.probable_solution p.data in *)
    "\n\nPlayer "^(card_to_string p.info.character)^":\n"
    ^"Known cards: "^(med_data_str p.data.players_known_cards "")^"\n"
    ^"Don't have cards: "^(med_data_str p.data.players_dont_have "")^"\n"
    ^"Eliminated: "^(card_list_to_string p.data.eliminated "")^"\n"^
    (* "Will accuse: "^(match p.info.accusation with
        | None -> "no\n"
        | Some s -> (solution_to_string s)^"\n")^
       "Probable Solution: "^(solution_to_string (fst sol))^" with prob "^
       (string_of_float (snd sol))^"\n"^
       "Room Probability: "^(prob_data_str p.data.probability.rooms "")^"\n"^
       "Weapon Probability: "^(prob_data_str p.data.probability.weapons "")^"\n"
       ^
       "Suspect Probability: "^(prob_data_str p.data.probability.suspects "") *)
    "Has one of: "^(has_one_str p.data.has_one_of "")
    ^"\n\n\n"
end

module MediumData = struct
  type t = {
    players_known_cards:(card * card list) list;
    players_dont_have: (card * card list) list;
    shown_cards_to: (card * card list) list;
    eliminated: card list;
    all_suspects: card list;
    all_weapons: card list;
    all_rooms: card list;
  }

  let rec player_list_init (suspects : card list) 
      (acc : (card * card list) list) : (card * card list) list = 
    match suspects with
    | [] -> acc
    | h::t -> player_list_init t ((h,[])::acc)

  let rec add_card_to_data_list (playerlst : (card * card list) list) 
      (p : card) (c : card) (acc : (card * card list) list) 
    : (card * card list) list =
    match playerlst with
    | [] -> acc
    | h::t -> if fst h = p then 
        add_card_to_data_list t p c ((fst h, create_set [] (c::snd h) )::acc) 
      else
        add_card_to_data_list t p c (h::acc)

  let rec add_cards_to_data_list (playerlst : (card * card list) list) 
      (p : card) (c : card list) (acc : (card * card list) list) 
    : (card * card list) list =
    match playerlst with
    | [] -> acc
    | h::t -> if fst h = p then 
        add_cards_to_data_list t p c ((fst h, create_set [] (c @ snd h))::acc)
      else add_cards_to_data_list t p c (h::acc)

  let rec replace_players_cards playerlst p (c:card list) acc =
    match playerlst with
    | [] -> acc
    | h::t -> if fst h = p then replace_players_cards t p c ((fst h, c):: acc)
      else replace_players_cards t p c (h :: acc)

  let empty suspects weapons rooms p : t = 
    let player_list = player_list_init suspects [] in
    let player_has_none = replace_players_cards player_list p 
        (suspects@weapons@rooms) [] in
    {
      players_known_cards = player_list;
      players_dont_have = player_has_none;
      shown_cards_to = [];
      eliminated = [];
      all_suspects = suspects;
      all_weapons = weapons;
      all_rooms = rooms;
    }

  let rec get_cards_of (cat : (card * card list) list) (p : card) : card list =
    match cat with 
    | [] -> failwith "no such player in list"
    | h::t -> if fst h = p then snd h else get_cards_of t p

  let rec add_card_to_all_players_except (cat: (card * card list) list) 
      (p:card) (c: card)(acc: (card * card list) list) : (card * card list) 
      list =
    match cat with 
    | [] -> acc
    | h::t -> if fst h = p then add_card_to_all_players_except t p c (h::acc)
      else
        add_card_to_all_players_except t p c
          ((fst h, create_set [] (c::snd h) )::acc)

  let rec remove_dont_helper cat p c acc : (card * card list) list = 
    let rec remove lst el acc =
      match lst with
      | [] -> acc
      | h::t -> if h=el then remove t el acc else remove t el (h::acc)
    in
    match cat with
    | [] -> acc
    | h::t -> if fst h = p then remove_dont_helper t p c
          ((fst h, remove (snd h) c [])::acc) else
        remove_dont_helper t p c (h::acc)

  let remove_from_dont (d:t) (p: card) (c: card) : t = 
    let new_dont = remove_dont_helper d.players_dont_have p c [] in
    {players_known_cards = d.players_known_cards;
     players_dont_have = new_dont;
     shown_cards_to = d.shown_cards_to;
     eliminated = d.eliminated;
     all_suspects = d.all_suspects;
     all_weapons = d.all_weapons;
     all_rooms = d.all_rooms; }

  let card_location_known (d : t) (p : card) (c : card) : t =
    let new_known = add_card_to_data_list d.players_known_cards p c [] in
    let new_dont = add_card_to_all_players_except d.players_dont_have p c [] in
    {players_known_cards = new_known;
     players_dont_have = new_dont;
     shown_cards_to = d.shown_cards_to;
     eliminated = create_set [] (c :: d.eliminated);
     all_suspects = d.all_suspects;
     all_weapons = d.all_weapons;
     all_rooms = d.all_rooms; }

  let dont_have_known (d : t) (p : card) (c : card list ) : t =
    let new_dont = add_cards_to_data_list d.players_dont_have p c [] in
    {players_known_cards = d.players_known_cards;
     players_dont_have = new_dont;
     shown_cards_to = d.shown_cards_to;
     eliminated = d.eliminated;
     all_suspects = d.all_suspects;
     all_weapons = d.all_weapons;
     all_rooms = d.all_rooms; }

  let showed_card_to (d : t) (p : card) (c : card) : t =
    let new_shown = add_card_to_data_list d.shown_cards_to c p [] in
    {players_known_cards = d.players_known_cards;
     players_dont_have = d.players_dont_have;
     shown_cards_to = new_shown;
     eliminated = d.eliminated;
     all_suspects = d.all_suspects;
     all_weapons = d.all_weapons;
     all_rooms = d.all_rooms; }
end 

module MediumBot: BotPlayer = struct

  type t = {data: MediumData.t; info: player_info}

  let add_card_to_hand (p:t) (c:card): t = 
    {data = MediumData.remove_from_dont
         (MediumData.card_location_known p.data p.info.character c) 
         p.info.character c;
     info = {
       character = p.info.character;
       hand = c :: p.info.hand;
       current_room = p.info.current_room;
       accusation = p.info.accusation
     } 
    }

  (** Initialize the player info  *)
  let initialize suspects weapons rooms character room: t = 
    {data= MediumData.empty suspects weapons rooms character;
     info={
       character = character;
       hand = [];
       current_room = room;
       accusation = None
     }
    }

  let get_data (player : t) : string list = 
    []

  let update_data (player: t) (lst : card list) = 
    player 


  let get_info (player : t) : player_info = 
    player.info

  let get_current_room (player : t) : card = 
    player.info.current_room

  (** The character that is representing a player [t] *)
  let get_character (player : t) : card = 
    (player.info).character

  (** The character that is representing a player [t] *)
  let get_hand (player : t) : card list = 
    (player.info).hand

  (** Move player from one room to another. *)
  let update_room (room : card) (pl : t) : t =
    {data = pl.data;
     info = {
       character = pl.info.character;
       hand = pl.info.hand;
       current_room = room;
       accusation = pl.info.accusation
     }
    }

  (* find passage *)
  let rec find_passage (p : passage list) (room : card) : card option =
    match p with
    | [] -> None
    | (r1,r2)::t -> if r1 = room then Some r2 else if 
        r2 = room then Some r1 else find_passage t room

  (** chooses if bot should enter secret passage *)
  let decide_passage (bd : Board.t) (pl : t) : card =
    match find_passage (get_passages bd) (get_current_room pl) with
    | None -> (get_current_room pl)
    | Some r -> if not (List.mem r (pl.data.eliminated)) then r 
      else (get_current_room pl)

  let find_solution (bd : Board.t) (p : t) : solution =
    let r = guess_random_card (p.data.eliminated) (get_rooms bd) in 
    let w = guess_random_card (p.data.eliminated) (get_weapons bd) in 
    let s = guess_random_card (p.data.eliminated) (get_suspects bd) in 
    {suspect=s;room=r;weapon=w}

  let set_accusation (bd : Board.t) (p : t) : t =
    if List.length (get_suspects bd) + List.length (get_weapons bd) +
       List.length (get_rooms bd) - 3 = List.length p.data.eliminated then
      {data = p.data;
       info = {
         character = p.info.character;
         hand = p.info.hand;
         current_room = p.info.current_room;
         accusation = Some (find_solution bd p)
       } 
      } else p

  let set_acc_solution (p:t) (sol: card list) : t =
    let rec get_sol_from_list (c: card list) (g: solution) : solution =
      match c with
      | [] -> g
      | Room r :: t -> get_sol_from_list t 
                         {weapon=g.weapon;suspect=g.suspect;room=Room r}
      | Weapon w :: t ->  get_sol_from_list t
                            {weapon=Weapon w;suspect=g.suspect;room=g.room}
      | Suspect s :: t ->  get_sol_from_list t
                             {weapon=g.weapon;suspect=Suspect s;room=g.room}
    in
    {data = p.data;
     info = {
       character = p.info.character;
       hand = p.info.hand;
       current_room = p.info.current_room;
       accusation = Some (get_sol_from_list sol {weapon=Weapon "error";
                                                 suspect= Suspect "error";
                                                 room= Room "error"})
     } 
    }
  let set_acc_direct (p:t) (guess: solution) : t =
    {data = p.data;
     info = {
       character = p.info.character;
       hand = p.info.hand;
       current_room = p.info.current_room;
       accusation = Some guess}}


  let get_accusation (p: t) : solution option = p.info.accusation

  let ready_to_accuse (p : t) : bool = 
    match p.info.accusation with 
    | None -> false
    | Some a -> true

  let rec cards_no_one_has (data: (card * card list) list)
    : card list =
    let rec union_lists lst1 lst2 acc =
      match lst1 with
      | [] -> acc
      | h::t -> if List.mem h lst2 then union_lists t lst2 (h::acc) 
        else union_lists t lst2 acc
    in
    let rec no_one_helper cat sol =
      match cat with
      | [] -> sol
      | h::t -> no_one_helper t (union_lists (snd h) sol [])
    in
    match data with
    | [] -> []
    | h::t -> no_one_helper t (snd h)

  let make_guess (bd: Board.t) (p: t): (solution * t) =
    let no_one_has = cards_no_one_has p.data.players_dont_have in
    let new_p = if List.length no_one_has = 3 then 
        set_acc_solution p no_one_has else p in
    let guess_room = if roll_dice (Random.self_init()) >= 3 then
        guess_random_card (new_p.data.eliminated) (get_rooms bd) else 
        decide_passage bd new_p in
    let guess_suspect = guess_random_card (new_p.data.eliminated) 
        (get_suspects bd) in 
    let guess_weapon = guess_random_card (new_p.data.eliminated) 
        (get_weapons bd) in 
    let turn_guess = {weapon=guess_weapon;suspect=guess_suspect;room=guess_room} 
    in (turn_guess, set_accusation bd (update_room guess_room new_p))

  let rec add_cards_to_unknown (p : t) (asked : card list) (guess : solution) 
    : t =
    match asked with
    | [] -> p
    | h::t -> add_cards_to_unknown { 
        data = MediumData.dont_have_known p.data h 
            [guess.weapon; guess.suspect; guess.room]; 
        info=p.info;
      } t guess

  let rec add_card_lst_to_unknown (p : t) (asked : card list)
      (cards : card list) : t =
    match asked with
    | [] -> p
    | h::t -> add_card_lst_to_unknown { 
        data = MediumData.dont_have_known p.data h cards; 
        info=p.info;
      } t cards

  let rec any_mem (c : card list) (lst : card list) : bool =
    match c with
    | [] -> false
    | h::t -> if List.mem h lst then true else any_mem t lst

  let check_if_two_thirds (guess : solution) (doesnt_have : card list)
    : card option =
    if List.mem guess.weapon doesnt_have && List.mem guess.suspect doesnt_have 
    then
      Some guess.room else if 
      List.mem guess.weapon doesnt_have && List.mem guess.room doesnt_have then
      Some guess.suspect else if 
      List.mem guess.suspect doesnt_have && List.mem guess.room doesnt_have then
      Some guess.weapon else None

  let rec check_answerer (p : t) (sus : card) (guess : solution) : t =
    let doesnt_have = MediumData.get_cards_of p.data.players_dont_have sus in
    if any_mem [guess.weapon; guess.suspect; guess.room]
        (MediumData.get_cards_of p.data.players_known_cards sus)then p else 
      match check_if_two_thirds guess doesnt_have with 
      | None -> p
      | Some c -> {data = MediumData.card_location_known p.data sus c ;
                   info = p.info}

  (** Updates data after observing another player's [move]  *)
  let update_from_move (move: move) (p: t) : t = 
    if move.asker = p.info.character then p else
      let added_unknown = add_cards_to_unknown p move.asked move.guess in
      match move.answerer with
      | None -> added_unknown
      | Some a -> check_answerer added_unknown a move.guess

  let card_eliminated (p:t) (c:card): bool =
    List.mem c p.data.eliminated

  (** Updates data after being shown a card [shown]  *)
  let update_from_shown (bd : Board.t) 
      (shown : (reveal option * Board.card list)) (p: t) (guess: solution) : t =
    match fst shown with
    | None -> if not (card_eliminated p guess.room) then
        set_acc_direct p guess else 
        add_card_lst_to_unknown p (snd shown) 
          [guess.suspect;guess.weapon]
    | Some rev ->
      let card_known =  MediumData.card_location_known p.data 
          rev.answerer rev.shown in 
      let set_asked_unknown = add_cards_to_unknown 
          { data = card_known;info = p.info } (snd shown) guess in 
      set_accusation bd set_asked_unknown

  let rec card_list_to_string (lst : card list) (acc : string) = 
    match lst with 
    | [] -> acc 
    | h::t -> card_list_to_string t (String.concat ", " 
                                       ((card_to_string h)::[acc])) 

  (** A response to another player's card request [guess]. *)
  let card_request (guess : solution) (player : t) : reveal option =
    if List.mem (guess.weapon) player.info.hand then
      Some {shown=guess.weapon;
            answerer=get_character player} else if 
      List.mem (guess.room) player.info.hand then
      Some {shown=guess.room;
            answerer=get_character player} else if
      List.mem (guess.suspect) player.info.hand then
      Some {shown=guess.suspect;
            answerer=get_character player} else
      None

  let rec med_data_str (cat: (card * card list) list) (acc:string) : string =
    match cat with
    | [] -> acc
    | h::t -> med_data_str t (card_to_string (fst h))^": "
              ^(card_list_to_string (snd h) "")^" // "^acc

  let data_to_string (p: t) : string = 
    "Player "^(card_to_string p.info.character)^":\n"
    ^"Known cards: "^(med_data_str p.data.players_known_cards "")^"\n"
    ^"Don't have cards: "^(med_data_str p.data.players_dont_have "")^"\n"
    ^"Eliminated: "^(card_list_to_string p.data.eliminated "")^"\n"^
    "Will accuse: "^(match p.info.accusation with
        | None -> "no\n\n\n"
        | Some {room=Room r;weapon=Weapon w;suspect=Suspect s} 
          -> s^" "^w^" "^s^"\n\n\n"
        | _ -> failwith "not proper solution" )
end

module GameUser: UserPlayer = struct
  type t = {data: card list; info: player_info}

  let add_card_to_hand (p : t) (c : card) : t = 
    {data = c :: p.data;
     info = {
       character = p.info.character;
       hand = c :: p.info.hand;
       current_room = p.info.current_room;
       accusation = p.info.accusation
     } 
    }

  (** Initialize the player info  *)
  let initialize (character : card ) (room : card) : t = 
    {data=[];
     info={
       character = character;
       hand = [];
       current_room = room;
       accusation = None
     }
    }

  let get_info (player : t) : player_info = 
    player.info

  let get_data (player : t) : card list = 
    player.data

  let get_current_room (player : t) : card = 
    player.info.current_room

  (** The character that is representing a player [t] *)
  let get_character (player : t) : card = 
    (player.info).character

  (** The character that is representing a player [t] *)
  let get_hand (player : t) : card list = 
    (player.info).hand

  (** Move player from one room to another. *)
  let update_room (room : card) (pl : t) : t =
    {data = pl.data;
     info = {
       character = pl.info.character;
       hand = pl.info.hand;
       current_room = room;
       accusation = pl.info.accusation
     }
    }

  (** Updates data after observing another player's [move]  *)
  let update_from_move (move : move) (player : t) : t = 
    player

  (** Updates data after being shown a card [shown]  *)
  let update_from_shown (bd : Board.t) 
      (shown : (reveal option * Board.card list)) (player: t) (guess: solution) 
    : t =
    match fst shown with
    | None -> player
    | Some rev ->
      { data = rev.shown :: player.data;
        info = player.info }

  (** A response to another player's card request [guess]. *)
  let card_request (guess : solution) (player : t) : reveal option =
    failwith "Do not card request the user"

end

