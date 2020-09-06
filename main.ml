open Command
open State



(** [bid st contenders highest_bidder amount] is the state after bidding is 
 *  finished. *)
let rec bid st contenders highest_bidder amount : State.t = 
  if List.length contenders = 0 then 
    begin
      if Player.get_player_id highest_bidder = 0 
      then begin
        (* case where there are no contenders and bank keeps property *)
        print_endline "No player chose to bid. Bidding ends now...\n";
        st
      end 
      else begin
        (* case where player wins *)
        print_endline ("Player " ^ 
                       (string_of_int (Player.get_player_id highest_bidder)) ^ 
                       " has won the bid with $" ^(string_of_int amount)^"\n");
        State.bid st (Player.get_player_id highest_bidder) amount 
          (State.get_curr_bid_property st)
      end
    end 
  else
    begin
      (* there are still contenders *)
      match contenders with
      | h::t -> 
        begin
          print_string ("\nPlayer "^(string_of_int 
                                       (Player.get_player_id h))^"'s bid: ");
          match Command.parse (read_line ()) with
          | Forfeit -> bid st t highest_bidder amount 
          | Bid i -> 
            begin
              let offered_amt = int_of_string (String.concat " " i) in
              (* offer bid > current bid and balance can pay for offered bid *)
              if offered_amt > amount && Player.get_balance h >= offered_amt
              then 
                begin
                  if Player.get_player_id highest_bidder <> 0 then 
                    bid st (t@[highest_bidder]) h offered_amt
                  else 
                    bid st t h offered_amt
                end else 
                begin
                  if int_of_string (String.concat " " i) > amount 
                  then 
                    begin
                      print_endline "You do not have enough money. Please try 
                      again.";
                      bid st (h::t) highest_bidder amount
                    end
                  else 
                    begin
                      print_endline "Enter an amount larger than the current 
                      bid.";
                      bid st (h::t) highest_bidder amount
                    end
                end
            end
          | exception Empty -> bid st contenders highest_bidder amount
          | exception _ -> 
            begin
              print_endline "Unrecognized command, please try again.";
              bid st contenders highest_bidder amount
            end
          | _ -> begin
              print_endline "Unrecognized command, please try again.";
              bid st contenders highest_bidder amount
            end
        end
      | [] -> failwith "No contenders. Error with main.ml bid"
    end

(** [trade tradet state] is the current state of [tradet] after user input. *) 
let rec trade tradet state = begin 
  begin
    let current_player = State.tradet_current_player tradet in 
    print_string ("Player " ^ (string_of_int current_player) ^ ": ")
  end;
  begin
    (* only add, remove, switch, confirm allowed *)
    match Command.parse (read_line ()) with
    | Add item -> 
      begin
        match State.add_trade (String.concat " " item) tradet state with
        | Legalt t -> trade t state
        | Illegalt s -> print_endline s; trade tradet state
      end 
    | Remove item -> 
      begin
        match State.remove_trade (String.concat " " item) tradet with
        | Legalt t -> trade t state
        | Illegalt s -> print_endline s; trade tradet state
      end 
    | Switch -> 
      begin
        match (State.switch tradet) with
        | Legalt t -> trade t state
        | Illegalt s -> print_endline s; trade tradet state
      end
    | Confirm -> 
      begin
        print_endline "Trade confirmed.\n";
        tradet
      end
    | exception Empty -> trade tradet state
    | exception _ -> 
      begin
        print_endline "Unrecognized command, please try again."; 
        trade tradet state
      end
    | _ -> begin
        print_endline "Please use one of the commands above. "; 
        trade tradet state
      end
  end
end

(** [purgatory t in_trade] is the new [t] after entering "purgatory" where you 
 *  sell/mortgage to not go below 0 in balance. [in_trade] true if in state 
 *  where you can't afford to buy a property; false when in debt to other 
 *  player or the bank. *)
let rec purgatory t in_trade = 
  Graph.show t;
  let curr_player = current_player t in
  let curr_bal = Player.get_balance (fst curr_player) in
  if in_trade 
  then begin
    if curr_bal >= 
       (Board.get_price (State.get_board t) (snd curr_player)) then 
      begin
        print_endline "You have enough money to buy property";
        print_endline "Type 'back' to go back";
      end
    else begin
      print_endline "You do not have enough money to buy property";
      print_endline "Do you want to view your properties, view info, sell \
                     buildings, mortgage properties, trade, or go back?";
    end
  end
  else begin
    let curr_grid_id = snd curr_player in
    let i = price_match t curr_grid_id in 
    if curr_bal >= List.nth (List.assoc curr_grid_id Board.al_info) i 
    then begin
      print_endline "You have enough money to pay your debt";
      print_endline "Type 'back' to go back";
    end
    else begin
      print_endline "You do not have enough money to pay off debt";
      print_endline "Do you want to view your properties, view info, sell \
                     buildings, mortgage properties, trade, or resign?";
    end
  end;
  match Command.parse (read_line ()) with
  | Property -> begin
      State.print_current_player_property t;
      purgatory t in_trade
    end
  | Info id -> begin
      State.info t (String.concat " " id);
      purgatory t in_trade
    end
  | Unbuild id -> begin
      match sell_property_state t (String.concat " " id) with 
      | Legal z -> purgatory z in_trade 
      | Illegal s -> begin
          print_endline s;
          purgatory t in_trade
        end
    end
  | Mortgage id -> begin
      match (State.mortgage_state (String.concat " " id) t) with
      | Legal z -> purgatory z in_trade
      | Illegal s -> begin 
          print_endline s; 
          purgatory t in_trade
        end
    end
  | Trade player_id -> begin
      match State.form_tradet (String.concat " " player_id) t with
      | Legalt z -> 
        begin 
          print_endline ("You are entering a trade phase with Player " ^
                         (String.concat " " player_id) ^".");
          print_endline "You may add or remove money or property by using \
                         add and remove";
          print_endline "Type Switch to switch to the other player.";
          print_endline ("When all the items are added, Player" ^ 
                         (String.concat " " player_id) ^ " should ");
          print_endline "type confirm to confirm the trade.\n";
          purgatory (confirm_trade (trade z t) t) in_trade
        end
      | Illegalt s -> print_endline s; purgatory t in_trade
    end
  | Resign -> if in_trade then begin 
      print_endline "You may not resign right now";
      purgatory t in_trade 
    end
    else State.resign t
  | Back -> 
    if in_trade then t 
    else begin
      let curr_grid_id = snd curr_player in
      let i = price_match t curr_grid_id in 
      let debt = List.nth (List.assoc curr_grid_id Board.al_info) i in
      if curr_bal >= debt
      then State.add_bal t (Player.get_player_id (fst curr_player)) debt
      else purgatory t false
    end
  | _ -> purgatory t in_trade
  | exception Empty -> purgatory t in_trade
  | exception _ -> print_endline "Unrecognized command, please try again."; 
    purgatory t in_trade

(** [purchase_help t] prompts you to buy or sell when the current player lands 
 *  on an unowned grid. *)
let rec purchase_help t = 
  let curr_player = State.current_player t in
  let curr_player_bal = Player.get_balance (fst curr_player) in
  let curr_grid_id = snd curr_player in
  print_endline ("\nYou have $" ^ (string_of_int curr_player_bal) ^ ".");
  print_endline ("Would you like to purchase " ^ curr_grid_id ^ "?");
  match Command.parse (read_line ()) with
  | No -> 
    begin 
      print_endline "Instructions to bid: ";
      print_endline "Type 'bid' followed by a number to bid that amount; \
                     you may not trade, sell buildings, mortgage, etc. during \
                     this stage. Type 'forfeit' to give up on your bid.";
      bid t (get_all_playerst t) (Player.player 0) 0 
    end
  | Yes -> begin
      match buy_property t with 
      | Legal x -> x
      | Illegal s -> purchase_help (purgatory t true)
    end
  | _ -> purchase_help t
  | exception Empty -> purchase_help t
  | exception _ -> print_endline "Unrecognized command, please try again."; 
    purchase_help t

(** [purchase t] is the new [t] after a player can purchase. *)
let purchase t_new t_old=
  Graph.show t_new;
  if List.length (get_all_players t_new) = List.length (get_all_players t_old) 
  then
    match check_pchase t_new with 
    | None -> t_new
    | Some true -> purchase_help t_new
    | Some _ -> t_new
  else t_new

(** [landing_help t] is the new [t] after a player lands on a new grid. *)
let landing_help t =
  Graph.show t;
  match handle_landing t with 
  | Legal z -> z
  | Illegal s -> purgatory t false

(** [play st] is the new [t] after taking user input. *)
let rec play st = 
  begin
    Graph.show st
  end;
  if List.length (State.get_all_playerst st) = 1  
  then begin
    print_endline ("\nCongratulations! Player "^ string_of_int 
                     (Player.get_player_id 
                        (List.nth 
                           (State.get_all_playerst st) 0 )) 
                   ^" has won the game!\n");
    match Command.parse (read_line ()) with 
    | exception _ -> Stdlib.exit 0
    | _ -> Stdlib.exit 0
  end
  else 
    begin
      begin
        let i = State.current_player st in 
        match i with
        | (player, grid) -> let id = Player.get_player_id player in 
          let jail_turn = Player.get_jail_turn player in 
          if jail_turn > 0 then begin
            print_endline ("Player " ^ string_of_int id ^ " is currently \
                                                           in jail.");
            print_endline "Before you roll type 'bail' to leave jail for \
                           $50 or try to roll doubles.";
            print_string ("Player " ^ string_of_int id ^ "'s turn: ") end
          else print_string ("Player " ^ string_of_int id ^ "'s turn: ");
      end;
      begin
        match Command.parse (read_line ()) with
        | Quit -> 
          begin
            print_endline "\nThanks for playing!\n";
            Stdlib.exit 0
          end
        | Resign -> play (State.resign st)
        | Roll -> 
          begin
            match (State.roll st) with 
            | Legal t -> play (purchase (landing_help t) t)
            | Illegal s-> print_endline s; play st
          end
        | Bail -> 
          begin
            match (State.post_bail st) with 
            | Legal t -> play t
            | Illegal s -> print_endline s; play st
          end
        | End -> 
          begin
            match (State.next_turn st) with 
            | Legal t -> print_endline "\n"; play t 
            | Illegal s -> begin
                print_endline s;
                play st
              end
          end
        | Property -> 
          begin
            State.print_current_player_property st;
            play st
          end
        | Info id -> 
          begin
            State.info st (String.concat " " id);
            play st
          end
        | Build id -> 
          begin
            match build_property_state st (String.concat " " id) with 
            | Legal t -> begin
                play t
              end
            | Illegal s -> 
              begin
                print_endline s;
                play st
              end
          end
        | Unbuild id -> 
          begin
            match sell_property_state st(String.concat " " id) with 
            | Legal t -> play t
            | Illegal s -> begin
                print_endline s;
                play st
              end
          end
        | Mortgage id -> begin 
            match (State.mortgage_state (String.concat " " id) st) with
            | Legal t -> play t
            | Illegal s -> begin
                print_endline s; 
                play st
              end
            | exception _ -> print_endline "No players in line. Not possible."; 
              play st
          end
        | Unmortgage id -> begin
            match State.unmortgage_state (String.concat " " id) st with
            | Legal t -> play t
            | Illegal s -> begin
                print_endline s; 
                play st
              end
            | exception _ -> print_endline "No players in line. Not possible."; 
              play st
          end
        | Trade player_id -> begin
            match State.form_tradet (String.concat " " player_id) st with
            | Legalt t -> 
              begin 
                print_endline ("You are entering a trade phase with Player " ^
                               (String.concat " " player_id) ^".");
                print_endline "You may add or remove money or property by using\
                               add and remove";
                print_endline "Type Switch to switch to the other player.";
                print_endline ("When all the items are added, Player" ^ 
                               (String.concat " " player_id) ^ " should ");
                print_endline "type confirm to confirm the trade.\n";
                play (confirm_trade (trade t st) st) 
              end
            | Illegalt s -> print_endline s; play st
          end
        | exception Empty -> play st
        | exception Malformed -> begin
            print_endline "Unrecognized command, please try again.";
            play st
          end
        | _ -> begin
            print_endline "Unrecognized command, please try again.";
            play st
          end
      end
    end

(** [play_start i] starts the game with [i] players. *)
let play_start i = play (State.state i)


let main () =
  print_endline "Welcome to Monopoly!";

  (* How to play *)
  print_endline "\nTo play, here are some possible commands.";
  print_endline "Type 'roll' to move your player, 'end' to end your turn, \
                 'bail' to bail out of jail, 'property' to check the properies \
                 you own, 'info' followed by a property to check its \ 
                 information, 'build/unbuild' followed by a property you own \ 
                 to build houses and hotels, 'mortgage/unmortgage' to \
                 temporarily mortgage/unmortgage a property";


  print_endline "How many players? (Max 6)";
  print_string  "> "; 
  let num_players = read_line () in
  match int_of_string (num_players) with
  | exception _ -> print_endline "Not a valid player number. Exiting...";
  | i -> if i > 1 && i < 7 
    then ignore (play_start i)
    else print_endline "Not a valid player number. Exiting..."


let () = main ()