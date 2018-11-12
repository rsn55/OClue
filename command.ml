type solution_lst = string list  
type card_str = string 
exception Empty
exception Malformed

type level = | Easy | Medium | Hard | Saved

let create_level (lst : string list) : level = 
  match lst with 
  | [] -> raise Empty
  | "easy"::[] -> Easy
  | "medium"::[] -> Medium
  | "hard"::[] -> Hard 
  | "saved"::[] -> Saved
  | _ -> raise Malformed 

let rec remove_spaces (lst : string list) (acc : string list) : string list = 
  match lst with 
  | [] -> acc 
  | h::t -> if (h = "") then remove_spaces t acc else remove_spaces t (h::acc) 

let parse_level (str : string) : level = 
  let trim_string = String.trim str in 
  let string_lst = String.split_on_char ' ' trim_string in 
  let new_lst = List.rev (remove_spaces string_lst []) in 
  match new_lst with 
  | [] -> raise (Empty)
  | _ -> create_level new_lst 

type command = 
  | Roll  
  | Show of card_str
  | Suggestion of solution_lst
  | Accusation of solution_lst
  | Quit
  | Cards
  | Notes 
  | Write of (card_str * string)
  | Erase of (card_str * string)
  | Leaderboard of (level * string)
  | Passages
  | Sneak
  | NoCards
  | Locations
  | Options
  | Save 

let create_command (lst : string list) : command = 
  match lst with 
  | [] -> raise Empty 
  | "roll"::[] -> Roll
  | "show"::x::[] -> Show x
  | "suggest"::x::y::z::[] -> Suggestion (x::y::[z])
  | "accuse"::x::y::z::[] -> Accusation (x::y::[z])
  | "quit"::[] -> Quit
  | "cards"::[] -> Cards
  | "notes"::[] -> Notes
  | "write" :: x::y::[] -> Write (x, y)
  | "erase" :: x::y::[] -> Erase (x,y)
  | "leaderboard" :: x :: y :: [] -> Leaderboard ((parse_level x),y)
  | "passages"::[] -> Passages
  | "sneak"::[] -> Sneak
  | "nocards"::[] -> NoCards
  | "locations"::[] -> Locations
  | "options"::[] -> Options
  | "save"::[] -> Save 
  | _ -> raise Malformed 

let parse (str : string) : command = 
  let trim_string = String.trim str in 
  let string_lst = String.split_on_char ' ' trim_string in 
  let new_lst = List.rev (remove_spaces string_lst []) in 
  match new_lst with 
  | [] -> raise (Empty)
  | _ -> create_command new_lst 