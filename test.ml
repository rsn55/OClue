open OUnit2
open Board
open Command
open Game_state
open Yojson.Basic.Util

let clue_map = from_json "clue.json"
let room_lst = all_rooms (Yojson.Basic.from_file "mini.json") 
let suspect_lst = all_suspects (Yojson.Basic.from_file "mini.json")
let weapons_lst = all_weapons (Yojson.Basic.from_file "mini.json")
let room_cards = get_rooms (from_json "mini.json") 
let suspect_cards = get_suspects 
    (from_json "mini.json") 
let weapon_cards = get_weapons (from_json "mini.json") 
let passage_lst = ["Study";"Kitchen";"Conservatory";"Lounge"]
let passages = [ (Room "Conservatory", Room "Lounge"); 
                 (Room "Study", Room "Kitchen");]

let board_tests =
  [
    "adv1" >:: (fun _ -> assert_equal room_lst 
                   ["Study";"Hall";"Lounge"] );
    "adv2" >:: (fun _ -> assert_equal suspect_lst 
                   ["Plum";"Green";"White"] );
    "adv3" >:: (fun _ -> assert_equal weapons_lst 
                   ["Wrench";"Knife";"Rope"] );
    "adv4" >:: (fun _ -> assert_equal "Lounge" 
                   (card_to_string (List.nth room_cards 0)));
    "adv5" >:: (fun _ -> assert_equal "Hall" 
                   (card_to_string (List.nth room_cards 1)));
    "adv6" >:: (fun _ -> assert_equal "Study" 
                   (card_to_string (List.nth room_cards 2)));
    "adv7" >:: (fun _ -> assert_equal "Plum" 
                   (card_to_string (List.nth suspect_cards 2)));
    "adv8" >:: (fun _ -> assert_equal "Green" 
                   (card_to_string (List.nth suspect_cards 1)));
    "adv9" >:: (fun _ -> assert_equal "White" 
                   (card_to_string (List.nth suspect_cards 0)));
    "adv10" >:: (fun _ -> assert_equal "Wrench" 
                    (card_to_string (List.nth weapon_cards 2)));
    "adv11" >:: (fun _ -> assert_equal "Knife" 
                    (card_to_string (List.nth weapon_cards 1)));
    "adv12" >:: (fun _ -> assert_equal "Rope" 
                    (card_to_string (List.nth weapon_cards 0)));
    "adv13" >:: (fun _ -> assert_equal 
                    (get_passages (from_json "mini.json")) []);
    "adv14" >:: (fun _ -> assert_equal 
                    (get_cards (from_json "mini.json")) 
                    (suspect_cards@weapon_cards@room_cards));
    "adv15" >:: (fun _ -> assert_equal 
                    (all_passages (Yojson.Basic.from_file "clue.json")) 
                    passage_lst);
    "adv16" >:: (fun _ -> assert_equal 
                    (make_passages passage_lst []) passages);
  ]

let command_tests =
  [
    "com1" >:: (fun _ -> assert_equal (parse "roll") 
                   Roll );
    "com2" >:: (fun _ -> assert_equal (parse "next") 
                   Next );
    "com3" >:: (fun _ -> assert_raises Empty (fun _ -> parse " "));
    "com4" >:: (fun _ -> assert_equal (remove_spaces ["hi"; ""; ""; "sup"] []) 
                   ["sup";"hi"]);
    "com5" >:: (fun _ -> assert_raises Malformed (fun _ -> parse 
                                                     "quit something"));
    "com6" >:: (fun _ -> assert_equal (parse "show card") 
                   (Show "card"));
    "com7" >:: (fun _ -> assert_equal (parse "suggest Suspect Weapon Room") 
                   (Suggestion ["Suspect"; "Weapon"; "Room"]));
    "com8" >:: (fun _ -> assert_raises Malformed (fun _ -> parse 
                                                     "suggest Suspect"));
    "com9" >:: (fun _ -> assert_raises Malformed 
                   (fun _ -> parse 
                       "suggest Suspect Weapon Room Extra"));
    "com10" >:: (fun _ -> assert_equal (parse "accuse Suspect Weapon Room") 
                    (Accusation ["Suspect"; "Weapon"; "Room"]));
    "com11" >:: (fun _ -> assert_raises Malformed (fun _ -> parse 
                                                      "accuse Suspect"));
    "com12" >:: (fun _ -> assert_raises Malformed 
                    (fun _ -> parse 
                        "accuse Suspect Weapon Room Extra"));
    "com13" >:: (fun _ -> assert_equal (parse "quit") 
                    Quit);
    "com14" >:: (fun _ -> assert_raises Malformed 
                    (fun _ -> parse 
                        "quit Game"));
    "com15" >:: (fun _ -> assert_equal (parse "cards") 
                    Cards);
    "com16" >:: (fun _ -> assert_equal (parse "notes") 
                    Notes);
    "com17" >:: (fun _ -> assert_equal (parse "passages") 
                    Passages);
    "com18" >:: (fun _ -> assert_equal (parse "sneak") 
                    Sneak);
    "com19" >:: (fun _ -> assert_equal (parse "locations") 
                    Locations);
    "com20" >:: (fun _ -> assert_equal (parse "options") 
                    Options);
    "com21" >:: (fun _ -> assert_equal (parse_level "easy") 
                    Easy);
    "com22" >:: (fun _ -> assert_equal (parse_level "medium") 
                    Medium);
    "com23" >:: (fun _ -> assert_equal (parse_level "hard") 
                    Hard);
    "com24" >:: (fun _ -> assert_raises Malformed 
                    (fun _ -> parse 
                        "easyish"));
    "com25" >:: (fun _ -> assert_raises Malformed 
                    (fun _ -> parse 
                        "easy medium"));
    "com26" >:: (fun _ -> assert_raises Empty 
                    (fun _ -> parse 
                        "     "));
    "com27" >:: (fun _ -> assert_raises Empty 
                    (fun _ -> parse 
                        ""));
    "com28" >:: (fun _ -> assert_equal (create_level ["hard"]) 
                    Hard);
    "com29" >:: (fun _ -> assert_raises Empty
                    (fun _ -> create_level []));
    "com30" >:: (fun _ -> assert_raises Malformed
                    (fun _ -> create_level ["easy"; "hard"]));
  ]

let state = init_state clue_map Easy
let players = get_players state
let notes = get_user_notes_for_card "Green" (List.nth players 0)  
let room1 = List.nth (get_rooms clue_map) 0 
let player1 = List.nth players 0 

let game_state_tests =
  [ 
    "state1" >:: (fun _ -> assert_equal 
                     (roll_dice (Random.self_init ()) >= 0 && roll_dice 
                        (Random.self_init ()) <= 6) true);
    "state2" >:: (fun _ -> assert_equal (increment_turn 6 state) 1);
    "state3" >:: (fun _ -> assert_equal (get_winner state) None);
    "state4" >:: (fun _ -> assert_equal (List.length players) 6);
    "state5" >:: (fun _ -> assert_equal (valid_move "Lounge" state clue_map) 
                     true);
    "state6" >:: (fun _ -> assert_equal (valid_weapon "blah" clue_map) false);
    "state7" >:: (fun _ -> assert_equal (valid_suspect "White" clue_map) true);
    "state8" >:: (fun _ -> assert_equal (get_losers state) []);
    "state9" >:: (fun _ -> assert_equal (List.mem (get_weapon_from_sol 
                                                     (get_solution state)) 
                                           (get_cards clue_map)) true);
    "state10" >:: (fun _ -> assert_equal (List.mem (get_suspect_from_sol 
                                                      (get_solution state)) 
                                            (get_cards clue_map)) true);    
    "state11" >:: (fun _ -> assert_equal (List.mem (get_room_from_sol 
                                                      (get_solution state)) 
                                            (get_cards clue_map)) true);
    "state12" >:: (fun _ -> assert_equal (List.mem (get_suspect 
                                                      (List.nth players 0)) 
                                            (get_cards clue_map)) true);
    "state13" >:: (fun _ -> assert_equal (check_user (get_user_player players)) 
                      true);
    "state14" >:: (fun _ -> assert_equal (get_current_turn state) 1); 
    "state15" >:: (fun _ -> assert_equal notes (Some "")); 
    "state16" >:: (fun _ -> assert_equal (get_actor_string (List.nth players 0)) 
                      (card_to_string (get_user_character players))); 
    "state17" >:: (fun _ -> assert_equal (user_player_string state) 
                      (card_to_string (get_user_character players))); 
    "state18" >:: (fun _ -> assert_equal (current_player_room_str state) 
                      (card_to_string (user_current_room state))); 
    "state19" >:: (fun _ -> assert_equal (get_winner state) None); 
    "state20" >:: (fun _ -> assert_equal 
                      ((next_turn (get_current_turn state) players) - 
                       (get_current_turn state)) 1); 
    "state21" >:: (fun _ -> assert_equal ((get_current_turn state) + 1) 
                      (next_turn (get_current_turn state) players)); 
    "state22" >:: (fun _ -> assert_equal (find_passage (get_passages clue_map) 
                                            (get_room_from_sol 
                                               (get_solution state))) None);
    "state23" >:: (fun _ -> assert_equal (get_player_room (move_player player1 
                                                             room1)) 
                      room1); 
    "state24" >:: (fun _ -> assert_equal (find_card "Hello" 
                                            (get_cards clue_map)) None); 
    "state25" >:: (fun _ -> assert_equal (check_card 
                                            (get_solution state) "Hello" state) 
                      None); 
    "state26" >:: (fun _ -> assert_equal (find_passage passages (Room "Study")) 
                      (Some (Room "Kitchen"))); 
    "state26" >:: (fun _ -> assert_equal (find_passage passages 
                                            (Room "Kitchen")) 
                      (Some (Room "Study"))); 
    "state27" >:: (fun _ -> assert_equal (find_passage passages 
                                            (Room "Conservatory")) 
                      (Some (Room "Lounge"))); 
    "state28" >:: (fun _ -> assert_equal (find_passage passages (Room "Lounge")) 
                      (Some (Room "Conservatory"))); 
    "state28" >:: (fun _ -> assert_equal (find_passage passages (Room "Hall")) 
                      None); 
  ]

let suite =
  "test suite"  >::: List.flatten [
    board_tests;
    command_tests;
    game_state_tests;
  ]

let _ = run_test_tt_main suite
