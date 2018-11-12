open Yojson.Basic.Util

type weapon = string
type suspect = string
type room = string
type card = Room of room | Weapon of weapon | Suspect of suspect
type passage = (card * card)
type solution = {weapon: card; suspect: card; room: card}

type t = {weapons: card list; suspects: card list; rooms: card list; 
          passages: passage list; file_name: string}

let all_rooms (json : Yojson.Basic.json) : room list = 
  json |> member "rooms" |> to_list |> filter_string

let all_suspects (json : Yojson.Basic.json) : suspect list = 
  json |> member "suspects" |> to_list |> filter_string

let all_weapons (json : Yojson.Basic.json) : weapon list =
  json |> member "weapons" |> to_list |> filter_string

let all_passages (json : Yojson.Basic.json) : string list =
  json |> member "passages" |> to_list |> flatten |> filter_string

let rec room_cards (roomlst : room list) (acc : card list) : card list = 
  match roomlst with 
  | [] -> acc 
  | h::t -> room_cards t (Room h::acc)

let rec suspect_cards (suspectlst : suspect list) (acc : card list) 
  : card list = 
  match suspectlst with 
  | [] -> acc 
  | h::t -> suspect_cards t (Suspect h ::acc) 

let rec weapon_cards (weaponlst : weapon list) (acc : card list) : card list  = 
  match weaponlst with 
  | [] -> acc 
  | h::t -> weapon_cards t  (Weapon h ::acc) 

let rec make_passages (passagelst : string list) (acc : passage list) 
  : passage list =
  match passagelst with 
  | [] -> acc 
  | h::s::t -> make_passages t ((Room h,Room s) ::acc) 
  | _ -> raise (Failure "The JSON needs to list passages as pairs.")

let from_json (json_file : string) : t = 
  let j = Yojson.Basic.from_file json_file in
  { weapons = weapon_cards (all_weapons j) [];
    suspects = suspect_cards (all_suspects j) [];
    rooms = room_cards (all_rooms j) []; 
    passages = make_passages (all_passages j) [];
    file_name = json_file
  }

let get_rooms (bd : t) : card list =
  bd.rooms

let get_weapons (bd : t) : card list = 
  bd.weapons

let get_suspects (bd : t) : card list = 
  bd.suspects

let get_cards (bd : t) : card list = 
  bd.suspects @ bd.weapons @ bd. rooms

let get_passages (bd : t) : passage list =
  bd.passages

let get_file_name (bd : t) : string = 
  bd.file_name

let card_to_string (c : card) : string = 
  match c with 
  | Weapon w -> w
  | Room r -> r
  | Suspect s -> s