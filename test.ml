(* TEST PLAN: We automatically tested Player, Command, Board, State, and
   manually tested Graph, Main, and State. We automatically tested the modules 
   above due to the diffuculty of testing something that has to meet a certain 
   requirement to work, which is diffucult to achieve through making function 
   calls here in test.ml. We wrote them by following the principle of glass-box 
   testing, where we tested our funcitons based on how we implementated our 
   functions. For instance, to test our command.ml file, we looked at the 
   [command] variant we wrote and tested unique possibilities including all edge
   cases. We believe that this methodology of testing will cover all possible 
   combinations of cases. By demonstrating the functionality of basic components 
   of the game, we are certain that it can be extended to other functions 
   through out the game and that is proven to be true during manual testing. *)


open OUnit
open Board 
open Player
open Command
open State


let player1 = player 1
let player2 = change_jail_turn player1 1
let player3 = change_balance player1 100
let player4 = add_money player3 100
let player5 = cycle_status player4
let player6 = cycle_status player5

let player_tests = [
  (* tests for number of players *)
  "players 2" >:: 
  (fun _ -> assert_equal 
      [1;2] (List.map (fun x -> Player.get_player_id x) (players 2))
  );

  (* tests for get functions *)
  "player1 id" >:: 
  (fun _ -> assert_equal 
      1 (get_player_id player1)
  );

  "player1 balance" >::
  (fun _ -> assert_equal 
      0 (get_balance player1)
  );

  "player1 get_jail_turn" >:: 
  (fun _ -> assert_equal 
      0 (get_jail_turn player1)
  );

  "player1 get status" >:: 
  (fun _ -> assert_equal 
      Pre (get_status player1)
  );

  (* money functions *)
  "player3 change balance" >:: 
  (fun _ -> assert_equal 
      100 (get_balance player3)
  );

  "player4 add money" >:: 
  (fun _ -> assert_equal 
      200 (get_balance player4)
  );

  (* change player's jail turn *)
  "player2 change_jail_turn" >:: 
  (fun _ -> assert_equal 
      1 (get_jail_turn player2)
  );

  (* cycle player's status *)
  "player4 -> player5" >:: 
  (fun _ -> assert_equal 
      Post (get_status player5)
  );
  "player5 -> player6" >::
  (fun _ -> assert_equal 
      Pre (get_status player6)
  );
]

(** Tests for exceptions*)
let make_command_exception_test 
    (name : string) 
    (expected_exception : exn)
    (parse_string : string) : test = 
  name >:: (fun _ -> 
      assert_raises 
        expected_exception (fun() -> Command.parse parse_string))

let command_tests = [
  (* commands with no phrase*)

  "command yes" >:: 
  (fun _ -> assert_equal 
      Yes (parse " YEs  ")
  );

  "command quit" >:: 
  (fun _ -> assert_equal 
      Quit (parse "Quit")
  );
  "command roll" >:: 
  (fun _ -> assert_equal 
      Roll (parse "roLl")
  );

  "command trade" >:: 
  (fun _ -> assert_equal 
      (Trade ["1"]) (parse "traDe 1")
  );

  "command bid" >:: 
  (fun _ -> assert_equal 
      (Bid ["20"]) (parse "bid 20")
  );

  "command add money" >:: 
  (fun _ -> assert_equal 
      (Add ["100"]) (parse "add 100")
  );

  "command add property" >:: 
  (fun _ -> assert_equal 
      (Add ["Olin"]) (parse "add Olin")
  );

  "command build property" >:: 
  (fun _ -> assert_equal 
      (Build ["Olin"]) (parse "build Olin")
  );

  "command unbuild property" >:: 
  (fun _ -> assert_equal 
      (Unbuild ["Olin"]) (parse "UNbuild Olin")
  );

  make_command_exception_test "command Malformed"
    (Malformed) "None";

  make_command_exception_test "command Malformed"
    (Malformed) "add number";

  make_command_exception_test "command Malformed"
    (Malformed) "yes no";

  make_command_exception_test "command Malformed"
    (Malformed) "trade Olin";

  make_command_exception_test "command Malformed"
    (Empty) "";

  make_command_exception_test "command Malformed"
    (Empty) "   ";

]


(*testing board functions*)
let board1 = board

(*changing color set to be owned by player 1*)
let board2 = change_ownership board1 "Olin" 1
let board3 = change_ownership board2 "Statler" 1
let board4 = change_ownership board3 "Duffield" 1
let board5 = change_ownership board4 "Klarman" 1

(* building on Olin; disclaimer: not allowed in actual game due to not allowing
   stacking of buildings*)
let board6 = build_property board5 "Olin"
let board7 = build_property board6 "Olin"
let board8 = build_property board7 "Olin"
let board9 = build_property board8 "Olin"
(* Olin has only one hotel *)
let board10 = build_property board9 "Olin"

let board11 = remove_property board10 "Olin"
let board12 = remove_property board11 "Olin"
let board13 = remove_property board12 "Olin"
let board14 = remove_property board13 "Olin"
(* No buildings left on Olin*)
let board15 = remove_property board14 "Olin"

let board_tests = [

  "get_grid on Go" >:: 
  (fun _ -> assert_equal 
      (Other ("o1", Go)) 
      (get_grid_only board1 "o1")
  );

  "get_grid on Olin" >:: 
  (fun _ -> assert_equal 
      (Land ("Olin", Green, 150 ,0, [])) 
      (get_grid_only board1 "Olin")
  );

  "get_grid on a random chest" >:: 
  (fun _ -> assert_equal 
      (Other ("o5", Chest)) 
      (get_grid_only board1 "o5")
  );

  "get_color on Olin" >:: 
  (fun _ -> assert_equal 
      (Green) 
      (get_color board1 "Olin")
  );

  "get_color on Day" >:: 
  (fun _ -> assert_equal 
      (Blue) 
      (get_color board1 "Day")
  );

  "get_price on Olin" >:: 
  (fun _ -> assert_equal 
      (150) 
      (get_price board1 "Olin")
  );

  "get_price on Day" >:: 
  (fun _ -> assert_equal 
      (400) 
      (get_price board1 "Day")
  );

  "get_ownership on Day" >:: 
  (fun _ -> assert_equal 
      (0) 
      (get_ownership board1 "Day")
  );

  "get_building on Olin" >:: 
  (fun _ -> assert_equal 
      ([]) 
      (get_building board1 "Day")
  );

  "get_forward from go, 4 steps" >:: 
  (fun _ -> assert_equal 
      ("o3", false) 
      (get_forward board1 "o1" 4)
  );

  "get_forward from Olin, 12 steps" >::
  (fun _ -> assert_equal 
      ("o10", false) 
      (get_forward board1 "Olin" 12)
  );

  "change_ownership, check_ownership on Olin" >:: 
  (fun _ -> assert_equal 
      (1) 
      (get_ownership board2 "Olin")
  );

  "change_ownership, check_ownership on Statler" >::
  (fun _ -> assert_equal 
      (1) 
      (get_ownership board3 "Statler")
  );

  "change_ownership, check_ownership on Duffield" >::
  (fun _ -> assert_equal 
      (1) 
      (get_ownership board4 "Duffield")
  );

  "change_ownership, check_ownership on Klarman" >::
  (fun _ -> assert_equal 
      (1) 
      (get_ownership board5 "Klarman")
  );

  "own_color on green" >:: 
  (fun _ -> assert_equal 
      (true) 
      (own_color board5 Green 1)
  );

  "player_property of player 1" >:: 
  (fun _ -> assert_equal 
      (["Statler";"Olin";"Duffield";"Klarman"]) 
      (player_property board5 1)
  );

  "build on Olin" >:: 
  (fun _ -> assert_equal 
      ([House;House]) 
      (get_building board7 "Olin")
  );

  "build on Olin" >:: 
  (fun _ -> assert_equal 
      ([House;House;House;House]) 
      (get_building board9 "Olin")
  );

  "build a hotel on Olin" >:: 
  (fun _ -> assert_equal 
      ([Hotel]) 
      (get_building board10 "Olin")
  );

  "remove hotel from Olin" >:: 
  (fun _ -> assert_equal 
      ([House;House;House]) 
      (get_building board12 "Olin")
  );

  "remove everything from Olin" >:: 
  (fun _ -> assert_equal 
      ([]) 
      (get_building board15 "Olin")
  );
]

(*state tests, hard to test in testing suite because need to roll to update 
  state, and rolling is randomized.*)
(*make state with 2 players*)
let state1 = State.state 2
(*exchange balance of players in state*)
let state2 = State.exchange_balance state1 1 2 100


let state_tests = [
  "get_all_players" >:: 
  (fun _ -> assert_equal 
      ([1;2]) 
      (List.map (fun (x,y) -> Player.get_player_id x) 
         (get_all_players state1 ))
  );

  "current player" >:: 
  (fun _ -> assert_equal 
      (1) 
      ((fun (x,y) -> Player.get_player_id x) (current_player state1))
  );

  "exchange bal" >:: 
  (fun _ -> assert_equal 
      (620) 
      ((fun (x,y) -> Player.get_balance x) (current_player state2))
  );
]

let suite = 
  "test suite for Monopoly" >::: List.flatten [
    player_tests;
    board_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite