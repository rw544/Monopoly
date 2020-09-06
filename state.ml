open Player
open Board

type doubles = int
type t = {
  board : Board.t;
  players : (Player.t * Board.grid_id) list;
  doubles : doubles
}
type result = Legal of t | Illegal of string


let state i = 
  let players = (Player.players i) in
  let playerxloc = List.map (fun x -> (x, "o1")) players in
  {board = Board.board;
   players = playerxloc;
   doubles = 0;}

let current_player t = 
  match t.players with 
  | h::t -> h
  | [] -> failwith "no players in player list"

let get_all_players t = t.players

let get_all_playerst t =
  List.map (fun (x,y) -> x) t.players

let get_board t =
  t.board

let print_current_player_property (st:t) =
  let lst =
    match current_player st with
    | (a,b) -> let plr_id = get_player_id a in 
      player_property st.board plr_id
  in print_endline (("Your properties: ") ^ (String.concat " " lst))

(** [add_balance player_id n prev nxt] adds a balance of [n] to the player 
 *  corresponding to [player_id]. *)
let rec add_balance (player_id:Player.id) (n:Player.money) prev nxt = 
  match nxt with
  | [] -> failwith "No players of such player_id"
  | (a,b)::tl -> 
    if player_id = get_player_id a 
    then prev @ ((change_balance a (get_balance a + n), b)::tl)
    else add_balance player_id n (prev@[(a,b)]) tl

let add_bal (t:t) (player_id:Player.id) (n:Player.money) : t = 
  let players = (add_balance player_id n [] t.players) in 
  {board = t.board;
   players = players;
   doubles = 0;}

let exchange_balance (t:t) player1_id player2_id (n:Player.money) : t = 
  let player1_t = (add_bal t player1_id (-1 * n)) in 
  (add_bal (player1_t) (player2_id) n)

let info t grid_id =
  let grid = List.assoc grid_id Board.al_info in
  begin
    print_endline ("\nProperty: " ^ grid_id);
    print_endline ("Rent: " ^ string_of_int (List.nth grid 0));
    print_endline ("Rent with color set: " ^ string_of_int (List.nth grid 1));
    print_endline ("With 1 House: " ^ string_of_int (List.nth grid 2));
    print_endline ("With 2 Houses: " ^ string_of_int (List.nth grid 3));
    print_endline ("With 3 Houses: " ^ string_of_int (List.nth grid 4));
    print_endline ("With 4 Houses: " ^ string_of_int (List.nth grid 5));
    print_endline ("With Hotel: " ^ string_of_int (List.nth grid 6));
    print_endline ("Mortgage Value: " ^ string_of_int (List.nth grid 7));
    print_endline ("House Cost: " ^ string_of_int (List.nth grid 8));
    print_endline ("Hotel Cost: " ^ string_of_int (List.nth grid 9));
    print_endline ""
  end

let price_match st grid = 
  match Board.get_grid_only st.board grid with
  | Other (_,_) -> failwith "match to other" 
  | Land (id, color, _, owner, buildings) -> 
    if own_color st.board color owner then 
      match buildings with
      | [House] -> 2
      | [House;House] -> 3
      | [House;House;House] -> 4
      | [House;House;House;House] -> 5
      | [Hotel] -> 6
      | [] -> 1
      | _ -> failwith ""
    else 0

let handle_landing st = 
  let players = st.players in 
  match players with 
  | h::t ->
    let (a,b) = h in 
    begin
      match Board.get_grid_only st.board b with  
      | Other (_, Chest) -> 
        begin 
          Random.self_init ();
          let money = (Random.int 60) + 10 in 
          begin 
            print_endline ("You earned $" ^ string_of_int money ^ 
                           " from chest.");
            Legal {
              board = st.board;
              players = (add_money a money, b)::t;
              doubles = st.doubles;
            } 
          end
        end
      | Other (_, GoJail) ->
        begin 
          print_endline ("You landed on Go to Jail.");
          Legal {
            board = st.board;
            players = ((change_jail_turn a 1), "o4")::t;
            doubles = st.doubles;
          }
        end
      | Land (id,color,_,owner,buildings) -> 
        begin 
          if owner > 0 && (get_player_id a) <> owner 
          then let i = 
                 begin
                   if (own_color st.board color owner) then 
                     match buildings with
                     | [House] -> 2
                     | [House;House] -> 3
                     | [House;House;House] -> 4
                     | [House;House;House;House] -> 5
                     | [Hotel] -> 6
                     | [] -> 1
                     | _ -> failwith ""
                   else 0
                 end in 
            let amount_due = List.nth (List.assoc b Board.al_info) i in
            let play_bal = get_balance a in
            if amount_due > play_bal 
            then Illegal "You don't have money to pay the rent."
            else begin 
              print_endline ("You paid Player " ^ (string_of_int owner) ^ " $"
                             ^ (string_of_int amount_due) ^ 
                             " for renting at their property.");
              Legal (exchange_balance 
                       st (Player.get_player_id a) owner amount_due)
            end
          else Legal st
        end
      | Other (_,_) -> Legal st
    end
  | [] -> failwith "No Players"







let post_bail t = 
  match t.players with 
  | [] -> failwith "impossible, no players"
  | (a,b)::tl ->
    if get_balance a < 50 || get_jail_turn a = 0 
    then Illegal "You cannot bail while low on money or not in jail."
    else Legal {
        board = t.board;
        players = ((change_jail_turn (add_money a (-50)) 0),b)::tl;
        doubles = t.doubles;
      }

(** [roll_free t a b tl n1 n2] is a new state where the player rolls a [n1] and 
 *  [n2], and steps the appropiate amount. *)
let roll_free t a b tl n1 n2 = 
  let landing = get_forward t.board b (n1+n2) in 
  let add_200 = if snd landing = false then 0 else 200 in
  if n1 <> n2 && (fst landing) <> "o10" then 
    {board = t.board;
     players = (add_money (cycle_status a) add_200, fst landing) :: tl;
     doubles = 0;} 
  else if n1 = n2 && t.doubles < 2 && (fst landing) <> "o10" then
    {board = t.board;
     players = (add_money a add_200, fst landing)::tl;
     doubles = t.doubles + 1;}
  else
    begin
      print_endline ("You sped or landed on Go to Jail.");
      (* double roll 3 times or land on o10 (go to Jail) *)
      {board = t.board;
       (* "o4" is the grid_id for Jail *)
       players = (cycle_status (change_jail_turn a 1),"o4")::tl;
       doubles = 0;}
    end

(** [roll_jail t a b tl n1 n2] is a new state where the player rolls a 
 *  [n1] and [n2] in Jail, and steps the appropiate amount. *)
let roll_jail t a b tl n1 n2 = 
  if n1 <> n2 
  then 
    (if get_jail_turn a >= 3 
     then 
       (match post_bail t with 
        | Illegal s -> Legal {
            board = t.board; 
            players = 
              (add_money 
                 (cycle_status 
                    (change_jail_turn a ((get_jail_turn a)+1))) 10, "o4")::tl;
            doubles = 0;}
        | Legal z -> 
          (match z.players with
           | [] -> failwith "impossible no players"
           | (x,y)::tail -> 
             let landing = get_forward z.board y (n1+n2) in 
             let add_200 = if snd landing = false then 0 else 200 in
             Legal {
               board = z.board;
               players = (add_money (cycle_status (change_jail_turn x 0)) 
                            add_200, fst landing)::tail;
               doubles = 0;
             })
       )
     else
       Legal {board = t.board;
              players = (cycle_status (change_jail_turn a 
                                         ((get_jail_turn a)+1)),"o4")::tl;
              doubles = 0;})
  else 
    let landing = get_forward t.board b (n1+n2) in 
    let add_200 = if snd landing = false then 0 else 200 in
    Legal {board = t.board;
           players = (add_money (cycle_status (change_jail_turn a 0)) add_200, 
                      fst landing)::tl;
           doubles = 0;}

let roll t = 
  (Random.self_init ());
  let num1 = ((Random.int 6) + 1) in
  (Random.self_init ());
  let num2 = ((Random.int 6) + 1) in
  match t.players with
  | [] -> failwith "No players, raised in state.ml roll"
  | (a,b)::tl -> 
    match get_status a with 
    | Post -> Illegal "You have already rolled."
    | Pre -> 
      print_endline ("Your rolls: " ^ string_of_int num1 ^ 
                     ", " ^ string_of_int num2);
      if get_jail_turn a = 0 
      then Legal (roll_free t a b tl num1 num2)
      else roll_jail t a b tl num1 num2 




let next_turn t = 
  match t.players with 
  | [] -> failwith "No players, raised in state.ml next turn"
  | (a,b)::tl -> 
    match get_status a with 
    | Pre -> Illegal "You have a roll available."
    | Post -> Legal {board = t.board;
                     players = tl@[(cycle_status a,b)];
                     doubles = 0;}

let check_pchase (t:t) =
  match current_player t with 
  | (a,b) -> 
    match get_ownership t.board b with
    | exception _ -> None
    | 0 -> Some true 
    | _ -> Some false




(** [check_ownership t] is true if the player that currently has turn owns the 
 *  property. *)
let check_ownership t =
  let curr_player = current_player t in 
  match curr_player with 
  | (a,b) -> if get_ownership t.board b = get_player_id a then true else false

let buy_property t =
  match t.players with 
  | [] -> failwith "impossible to have no players"
  | (a,b)::tl -> 
    let updt_bal = get_balance a-(get_price t.board b) in
    let updt_player = change_balance a updt_bal in 
    if updt_bal < 0 
    then Illegal ""
    else Legal {board = change_ownership t.board b (get_player_id a); 
                players = (updt_player,b)::tl;
                doubles = t.doubles;}




let change_ownership_state (t:t) (grid_id:grid_id) (player_id:Player.id) = 
  let new_board = change_ownership t.board grid_id player_id in 
  {board = new_board; 
   players = t.players;
   doubles = t.doubles;}

let rec property_count list rtn = 
  match list with 
  | [] -> List.rev rtn
  | (id,p)::tl -> 
    let x = match p with 
      | [] -> 0
      | [House] -> 1
      | [House;House] -> 2
      | [House;House;House] -> 3
      | [House;House;House;House] -> 4
      | [Hotel] -> 5
      | _ -> 99
    in property_count tl (x::rtn)

let rec property_number list grid_id acc =
  match list with 
  | [] -> acc
  | (id,p)::tl -> 
    if id = grid_id then acc else property_number tl grid_id (acc+1)

let check_hotel t grid_id =
  match build_property t.board grid_id with 
  | exception _ -> true
  | _ -> false



let is_int (item:string) =
  match int_of_string item with
  | i -> Some i
  | exception _ -> None

let check_hotel t grid_id =
  match build_property t.board grid_id with 
  | exception _ -> true
  | _ -> false

let check_house t grid_id  = 
  match remove_property t.board grid_id with
  | exception _ -> true 
  | _ -> false


let check_stack t color grid_id buying =
  let color_list = color_buildings t.board color [] in
  let counted_list = property_count color_list [] in 
  let prop_number = property_number color_list grid_id 0 in
  let num_props = List.nth counted_list prop_number in 
  if buying 
  then List.length (List.filter (fun x -> num_props > x) counted_list) > 0 
  else List.length (List.filter (fun x -> num_props < x) counted_list) > 0 


let build_property_state (t:t) (grid_id:grid_id) = 
  match is_int grid_id with 
  | Some n -> Illegal "Please enter a valid property name"
  | None ->  
    let prop_list = List.map (fun (x,y) -> x) Board.al_info in
    if not (List.mem grid_id prop_list) 
    then Illegal "Please enter a valid property name"
    else begin
      match t.players with 
      | (a,b)::tl -> 
        if get_player_id a <> get_ownership t.board grid_id 
        then Illegal "You don't own this property or it is mortgaged."
        else begin
          let c = get_color t.board grid_id in 
          if color_not_mort t.board c (get_player_id a) 
          then begin
            if check_stack t c grid_id true 
            then Illegal "Can't stack properties"
            else begin
              let cost = List.nth (List.assoc grid_id Board.al_info) 8 in 
              let updated_bal = (get_balance a)-cost in
              if updated_bal < 0 then Illegal "Not enough money to build"
              else begin
                if check_hotel t grid_id then Illegal "Already built hotel, \
                                                       can't build further."
                else
                  Legal {
                    board = build_property t.board grid_id;
                    players = ((change_balance a updated_bal), b)::tl;
                    doubles = t.doubles;
                  }
              end
            end
          end
          else Illegal "You don't own the color set or one of your properties \
                        in the color set is mortgaged."
        end
      | _ -> Illegal "impossible, no players raised in build_property_state"
    end



let sell_property_state t grid_id =
  match is_int grid_id with 
  | Some n -> Illegal "Please enter a valid property name"
  | None -> 
    let prop_list = List.map (fun (x,y) -> x) Board.al_info in
    if not (List.mem grid_id prop_list) 
    then Illegal "Please enter a valid property name"
    else begin
      match t.players with 
      | (a,b)::tl -> 
        if get_player_id a <> get_ownership t.board grid_id 
        then Illegal "You don't own this property or it is mortgaged"
        else begin
          let c = get_color t.board grid_id in 
          if check_stack t c grid_id false 
          then Illegal "Not allowed, selling leads to stack" 
          else begin
            if check_house t grid_id 
            then Illegal "You have no houses built here"
            else let new_playert = add_money a              
                     ((List.nth (List.assoc grid_id Board.al_info) 8)/2) in  
              Legal {    
                board = remove_property t.board grid_id;
                players = (new_playert,b)::tl;  
                doubles = t.doubles;    
              }
          end
        end
      | [] -> Illegal "impossible, no players raised in sell_property_state"
    end




type tradet = {
  money : int list;
  properties : grid_id list list;
  offerp : Player.id;
  acceptp : Player.id;
  side : int;
}

type trade_result = Legalt of tradet | Illegalt of string

let rec all_players_id player_list id_list =
  match player_list with 
  | [] -> id_list 
  | (a,b)::tl -> all_players_id tl ((get_player_id a)::id_list)

(** [get_balance_1p (player_id:Player.id) prev nxt] gets the balance of the 
 *  player corresponding to [player_id]. *)
let rec get_balance_1p (player_id:Player.id) prev nxt = 
  match nxt with
  | [] -> failwith "No players of such player_id"
  | (a,b)::tl -> 
    if player_id = get_player_id a 
    then get_balance a
    else get_balance_1p player_id (prev@[(a,b)]) tl

let tradet_current_player tradet = 
  if tradet.side = 0 then tradet.offerp else tradet.acceptp

let form_tradet player t =
  let curr_player = get_player_id (fst (current_player t)) in 
  match is_int player with 
  | None -> Illegalt "Invalid, enter a player id"
  | Some n -> let player_id_list = (all_players_id t.players []) in
    if n <> curr_player && List.mem n player_id_list
    then 
      Legalt {
        money = [0;0];
        properties = [[];[]];
        offerp = curr_player;
        acceptp = n;
        side = 0;}
    else Illegalt "Invalid player id"

let add_trade (item:string) tradet t =
  match is_int item with
  | Some n -> 
    let p1money = List.nth tradet.money 0 in
    let p2money = List.nth tradet.money 1 in
    (if tradet.side = 0 
     then 
       let balance = get_balance (fst (current_player t)) in
       (if (balance - (n+p1money)) >= 0 
        then Legalt {
            money = [(n+p1money);(List.nth tradet.money 1)];
            properties = tradet.properties;
            offerp = tradet.offerp;
            acceptp = tradet.acceptp;
            side = tradet.side;}
        else Illegalt "not enough money for offering player")
     else 
       let balance = get_balance_1p tradet.acceptp [] t.players in 
       (if (balance - (n+p2money)) >= 0 
        then Legalt {
            money = [(List.nth tradet.money 0);(n+p2money)];
            properties = tradet.properties;
            offerp = tradet.offerp;        
            acceptp = tradet.acceptp;
            side = tradet.side}
        else Illegalt "not enough money for receiving player"))
  | None -> 
    match get_grid_only t.board item with 
    | Other (_,_) -> Illegalt "not tradeable"
    | Land (_,_,_,o,p) -> 
      if List.length p <> 0
      then Illegalt "cannot trade properties with houses/hotels built"
      else    
        let p1props = List.nth tradet.properties 0 in
        let p2props = List.nth tradet.properties 1 in
        (if tradet.side = 0
         then  
           (if o <> tradet.offerp 
            then Illegalt "property not owned by offering player"
            else 
              (if List.mem item p1props
               then Illegalt "already offered"
               else Legalt {
                   money = tradet.money;
                   properties = [(item::p1props);p2props];   
                   offerp = tradet.offerp;        
                   acceptp = tradet.acceptp;
                   side = tradet.side;}))
         else 
           (if o <> tradet.acceptp
            then Illegalt "property not owned by receiving player"
            else 
              (if List.mem item p2props
               then Illegalt "already offered"
               else Legalt {
                   money = tradet.money;
                   properties = [p1props;(item::p2props)];   
                   offerp = tradet.offerp;        
                   acceptp = tradet.acceptp;
                   side = tradet.side;}))
        )


let remove_trade (item:string) tradet =
  match is_int item with 
  | Some n -> 
    (if tradet.side = 0 
     then
       let updated_bal = (List.nth tradet.money 0) - n in 
       (if updated_bal < 0 
        then Illegalt "cannot remove more than offered"
        else Legalt {
            money = [updated_bal;(List.nth tradet.money 1)];
            properties = tradet.properties;   
            offerp = tradet.offerp;        
            acceptp = tradet.acceptp;
            side = tradet.side;})
     else
       let updated_bal = (List.nth tradet.money 1) - n in 
       (if updated_bal < 0 
        then Illegalt "cannot remove more than offered"
        else Legalt {
            money = [(List.nth tradet.money 0);updated_bal];
            properties = tradet.properties;   
            offerp = tradet.offerp;        
            acceptp = tradet.acceptp;
            side = tradet.side;}))
  | None ->
    let p1props = List.nth tradet.properties 0 in
    let p2props = List.nth tradet.properties 1 in
    if tradet.side = 0
    then 
      (if List.mem item p1props 
       then let updated = List.filter (fun x -> x <> item) p1props in 
         Legalt {
           money = tradet.money;
           properties = [updated;p2props];   
           offerp = tradet.offerp;        
           acceptp = tradet.acceptp;
           side = tradet.side;}
       else Illegalt "property not offered")
    else 
      (if List.mem item p2props 
       then let updated = List.filter (fun x -> x <> item) p2props in
         Legalt {
           money = tradet.money;
           properties = [p1props;updated];   
           offerp = tradet.offerp;        
           acceptp = tradet.acceptp;
           side = tradet.side;}
       else Illegalt "property not offered")


let switch tradet =
  match tradet.side with
  | 0 -> Legalt {
      money = tradet.money;
      properties = tradet.properties;   
      offerp = tradet.offerp;        
      acceptp = tradet.acceptp;
      side = 1;}
  | 1 -> Legalt {
      money = tradet.money;
      properties = tradet.properties;   
      offerp = tradet.offerp;        
      acceptp = tradet.acceptp;
      side = 0;}
  | _ -> failwith "not possible"


let rec trade_props t prop_list player_id =
  match prop_list with
  | [] -> t
  | hd::tl -> trade_props (change_ownership_state t hd player_id) tl player_id


let confirm_trade tradet t =
  let moneyp1 = List.nth tradet.money 0 in
  let moneyp2 = List.nth tradet.money 1 in 
  let propertiesp1 = List.nth tradet.properties 0 in
  let propertiesp2 = List.nth tradet.properties 1 in
  let p1 = tradet.offerp in
  let p2 = tradet.acceptp in
  let st1 = exchange_balance t p1 p2 moneyp1 in 
  let st2 = exchange_balance st1 p2 p1 moneyp2 in
  let st3 = trade_props st2 propertiesp1 p2 in 
  trade_props st3 propertiesp2 p1



let mortgage_state grid_id t =
  match is_int grid_id with 
  | Some n -> Illegal "Please enter a valid property name"
  | None ->  
    let prop_list = List.map (fun (x,y) -> x) Board.al_info in
    if not (List.mem grid_id prop_list) 
    then Illegal "Please enter a valid property name"
    else
      let (curr_playert,pos) = (current_player t) in 
      if get_ownership t.board grid_id = get_player_id curr_playert
      then 
        let c = get_color t.board grid_id in 
        if check_built t.board c [] && 
           own_color t.board c (get_player_id curr_playert) 
        then Illegal "You must sell your buildings of this color set before \
                      you mortgage"
        else
          match t.players with 
          | [] -> failwith "impossible no players raised in mortgage_state"
          | hd::tl -> Legal {
              board = change_ownership t.board grid_id 
                  ((get_player_id curr_playert) * -1);
              players = ((add_money curr_playert 
                            (List.nth (List.assoc grid_id Board.al_info) 7)), 
                         pos)::tl;
              doubles = t.doubles;}
      else Illegal "You don't own this property or it is already mortgaged"


let unmortgage_state grid_id t =
  match is_int grid_id with 
  | Some n -> Illegal "Please enter a valid property name"
  | None ->  
    let prop_list = List.map (fun (x,y) -> x) Board.al_info in
    if not (List.mem grid_id prop_list) 
    then Illegal "Please enter a valid property name"
    else
      let (curr_playert,pos) = (current_player t) in
      let curr_p_id=get_player_id curr_playert in
      if get_ownership t.board grid_id = curr_p_id * -1 
      then
        match t.players with 
        | [] -> failwith "impossible no players raised in unmortgage_state"
        | hd::tl ->
          let debt = (int_of_float 
                        (-1.1 *. float_of_int 
                            (List.nth (List.assoc grid_id Board.al_info) 7))) in
          if (get_balance (fst hd)) - debt < 0 
          then Illegal "You do not have enough money to unmortgage"
          else
            Legal { 
              board = change_ownership t.board grid_id curr_p_id;
              players = ((add_money curr_playert debt),  pos)::tl;
              doubles = t.doubles;}
      else Illegal "You don't own this property or it is not mortgaged."

(** [resign_owner grid_id_l board player_id] switches all ownership of all 
 *  grid_id's in [grid_id_l] to corresponding ones of [player_id], returns final 
 *  board using this method. *)
let rec resign_owner grid_id_l board player_id money=
  match grid_id_l with 
  | [] -> (board, player_id, money) 
  | hd::tl -> 
    let num_props =
      match get_building board hd with 
      | [] -> 0
      | [House] -> 1 
      | [House; House] -> 2
      | [House; House; House] -> 3 
      | [House; House; House; House] -> 4 
      | [Hotel] -> 5 
      | _ -> 99
    in
    let payout = (List.nth (List.assoc hd Board.al_info) 8)/2 * num_props in 
    if get_ownership board hd < 0 
    then
      resign_owner tl (change_ownership (demolish board hd) hd 
                         (player_id * -1)) player_id money
    else resign_owner tl (change_ownership (demolish board hd) hd player_id) 
        player_id (money + payout)

(** [resign_bhelp player1t t] is the board after all properties changed from 
 *  the current player to the player that was landed on (keeps the mortgage). *)
let resign_bhelp player1t t =
  match player1t with 
  | (a,b) -> 
    match get_ownership t.board b with 
    | x -> let y = (if x = (get_player_id a) then 0 else x) in
      let player_prop = player_property t.board (get_player_id a) in
      resign_owner player_prop t.board y 0
    | exception _ -> let player_prop = player_property t.board (get_player_id a) 
      in resign_owner player_prop t.board 0 0

let add_money_to_id lst money id = 
  List.map (fun (a,b) -> 
      if (Player.get_player_id a = id) then (add_money a money,b) else (a,b)
    ) 
    lst


let resign (t:t) : t =
  match t.players with
  | hd::tl -> begin
      match resign_bhelp hd t with 
      | (new_bd, plyr_id, money) -> 
        if plyr_id <> 0 then  {
          board = new_bd;
          players = add_money_to_id tl money plyr_id;
          doubles = 0;}
        else {
          board = new_bd;
          players = tl;
          doubles = 0;}
    end 
  | _ -> failwith "no players, raised in resign"

let get_curr_bid_property (t:t) : Board.grid_id = 
  match current_player t with
  | (a,b) -> b

let bid (st:t) (player_id:Player.id) (amt_bid:int) (grid_id:Board.grid_id) = 
  let t' = add_bal st player_id (-1 * amt_bid) in
  change_ownership_state t' grid_id player_id




