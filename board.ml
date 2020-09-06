open Player

type other_type =
  | Go
  | Jail
  | GoJail
  | Free
  | Chest

type color =
  | Red
  | Blue
  | Green
  | Yellow

type building =
  | House 
  | Hotel

type grid_id = string
type price = int

type grid =
  | Other of (grid_id * other_type)
  | Land of (grid_id * color * price * Player.id * building list)

type t = grid list
type infot = (grid_id * int list) list

(* board is hardcoded because it is fixed *)
let board : t =
  [
    Other ("o1", Go);
    Land ("Thurston", Red, 60, 0, []);
    Other ("o2", Chest);
    Land ("Malott", Red, 80, 0, []);
    Other ("o3", Chest);
    Land ("Mann", Red, 90, 0, []);
    Land ("PSB", Red, 120, 0, []);
    Other ("o4", Jail);
    Land ("Statler", Green, 140, 0, []);
    Land ("Olin", Green, 150, 0, []);
    Land ("Duffield", Green, 180, 0, []);
    Other ("o5", Chest);
    Other ("o6", Chest);
    Land ("Klarman", Green, 200, 0, []);
    Other ("o7", Free);
    Land ("Weill", Yellow, 220, 0, []);
    Land ("Uris", Yellow, 240, 0, []);
    Other ("o8", Chest);
    Land ("Barton", Yellow, 260, 0, []);
    Land ("Sage", Yellow, 280, 0, []);
    Other ("o9", Chest);
    Other ("o10", GoJail);
    Land ("Kennedy", Blue, 300, 0, []);
    Land ("Phillips", Blue, 330, 0, []);
    Other ("o11", Chest);
    Other ("o12", Chest);
    Land ("Upson", Blue, 360, 0, []);
    Land ("Day", Blue, 400, 0, []);
  ]

(* information is hardcoded *)
let al_info : infot =
  [
    ("Thurston", [2;4;10;30;90;160;250;30;50;50]);
    ("Malott", [4;8;20;60;180;320;450;30;50;50]);
    ("Mann", [6;12;30;90;270;400;550;50;50;50]);
    ("PSB", [8;16;40;100;300;450;600;60;50;50]);
    ("Statler", [10;20;50;150;400;625;750;70;100;100]);
    ("Olin", [12;24;60;180;500;700;900;80;100;100]);
    ("Duffield", [14;28;70;200;550;750;950;90;100;100]);
    ("Klarman", [16;32;80;220;600;800;1000;100;100;100]);
    ("Weill", [18;36;90;250;700;875;1050;110;150;150]);
    ("Uris", [20;40;100;300;750;925;1100;120;150;150]);
    ("Barton", [22;44;110;330;800;975;1150;130;150;150]);
    ("Sage", [24;48;120;360;850;1025;1200;140;150;150]);
    ("Kennedy", [26;52;130;390;900;1100;1275;150;200;200]);
    ("Phillips", [28;56;150;450;1000;1200;1400;160;200;200]);
    ("Upson", [35;70;175;500;1100;1300;1500;175;200;200]);
    ("Day", [50;100;200;600;1400;1700;2000;200;200;200]);
  ]


let rec get_grid (t:t) (grid_id:grid_id) (prev:grid list) =
  match t with 
  | [] -> failwith "Unmatched grid_id, raised in board.ml"
  | Other (id,other_type)::tl -> 
    begin
      if id = grid_id 
      then (Other (id, other_type), List.rev(prev), tl)
      else get_grid tl grid_id  (Other (id, other_type)::prev)
    end
  | Land (id,color,price,owner,building)::tl -> 
    begin
      if id = grid_id 
      then (Land (id,color,price,owner,building),List.rev(prev),tl) 
      else get_grid tl grid_id 
          (Land (id,color,price,owner,building)::prev)
    end


let get_grid_only (t:t) (grid_id:grid_id) =
  let (id,_,_) = (get_grid t grid_id []) in id


let get_color t grid_id =
  match (get_grid_only t grid_id) with
  | Land (_,color,_,_,_) -> color
  | Other (_,_) -> failwith "No color for this grid, raised in board.ml"


let get_price t grid_id =
  match (get_grid_only t grid_id) with
  | Land (_,_, price,_,_) -> price
  | Other (_,_) -> failwith "No price for this grid, raised in board.ml"


let get_ownership t grid_id =
  match (get_grid_only t grid_id) with
  | Land (_,_,_,owner,_) -> owner
  | Other (_,_) -> failwith "No owner for this grid, raised in board.ml"  


let get_building t grid_id =
  match (get_grid_only t grid_id) with
  | Land (_,_,_,_,buildings) -> buildings
  | Other (_,_) -> failwith "No building for this grid, raised in board.ml"


let get_forward (t:t) (grid_id:grid_id) n_steps = 
  let curr_grid = get_grid t grid_id [] in 
  match curr_grid with
  | (Other (_,_),prev,tl) -> 
    let all_tiles_moved = List.length(prev) + n_steps in 
    let board_length = List.length t in 
    let pass_go = all_tiles_moved >= board_length in
    let steps = all_tiles_moved mod board_length in
    let next_grid = List.nth t steps in 
    begin
      match next_grid with 
      | Other (i,ot) -> (i,pass_go)
      | Land (i,c,p,o,b) -> (i,pass_go)
    end
  | (Land (_,_,_,_,_), prev, tl) -> 
    let all_tiles_moved = List.length(prev) + n_steps in 
    let board_length = List.length t in 
    let pass_go = all_tiles_moved >= board_length in
    let steps = all_tiles_moved mod board_length in
    let next_grid = List.nth t steps in 
    begin
      match next_grid with 
      | Other (i,ot) -> (i,pass_go)
      | Land (i,c,p,o,b) -> (i,pass_go)
    end


let change_ownership (t:t) (grid_id:grid_id) player_id =
  let curr_grid = get_grid t grid_id [] in 
  match curr_grid with 
  | (Other (_,_),_,_) -> failwith "Not ownable, raised in board.ml"
  | (Land (i,c,p,o,b),prev,tl) -> 
    prev @ ((Land (i,c,p,player_id,b))::tl)


let rec own_color t color player_id =
  match t with 
  | [] -> true 
  | hd::tl -> 
    begin
      match hd with 
      | Other (_,_) -> own_color tl color player_id 
      | Land (_,c,_,o,_) -> 
        if color = c && Int.abs o <> player_id 
        then false 
        else own_color tl color player_id 
    end

let rec color_not_mort t color player_id =
  match t with 
  | [] -> true 
  | hd::tl -> 
    begin
      match hd with 
      | Other (_,_) -> color_not_mort tl color player_id 
      | Land (_,c,_,o,_) -> 
        if color = c && o <> player_id 
        then false 
        else color_not_mort tl color player_id 
    end


let rec color_buildings t color adj_list =
  match t with 
  | [] -> adj_list
  | hd::tl -> 
    match hd with
    | Other (_,_) -> color_buildings tl color adj_list
    | Land (id, c,_,_,p) -> 
      if color = c then color_buildings tl color [(id,p)]@adj_list
      else color_buildings tl color adj_list

(** [check_built_helper nlist rtn] checks if the list of numbers contains
 *  any number that is not 0. *)
let rec check_built_helper nlist rtn =
  match nlist with 
  | [] -> rtn
  | h::t -> if h <> 0 then true else check_built_helper t rtn

let check_built t color adj_list =
  let color_list = color_buildings t color adj_list in 
  let num_props_list =List.map (fun (_,p)-> List.length p) color_list in 
  check_built_helper num_props_list false

let rec player_property t id =
  match t with 
  | [] -> []
  | Other (_,_)::tl -> (player_property tl id)
  | Land (a,_,_,o,_)::tl -> 
    if (Int.abs o) = id then a::(player_property tl id) 
    else (player_property tl id)

(** [can_build buildings count] returns (true,false) if you can build a 
 *  house, (false,true) if you can build a hotel, (false,false) otherwise. *)
let rec can_build buildings count =
  match buildings with
  | [] -> if count < 4 then (true,false) 
    else if count = 4 then (false,true)
    else (false, false)
  | House::t -> can_build t (count+1)
  | Hotel::t -> (false,false)


let build_property (t:t) (grid_id:grid_id) =
  let buildings = (get_building t grid_id) in
  match (get_grid t grid_id []) with
  | (Other (_,_),_,_) ->  failwith "Not buildable, raised in board.ml"
  | (Land (i,c,p,o,b),prev,tl) -> 
    let build_next = can_build buildings 0 in
    if fst build_next
    then prev @ ((Land (i,c,p,o,House::buildings))::tl)
    else begin 
      if snd build_next
      then prev @ ((Land (i,c,p,o,[Hotel]))::tl)
      else failwith "Cannot build further"
    end

(** [remove_property_helper buildings] is a building after removing. *)
let remove_property_helper lst =
  match lst with 
  | [] -> failwith "No buildings"
  | Hotel::t -> [House; House; House; House]
  | House::t -> t

let remove_property (t:t) (grid_id:grid_id) =
  let buildings = (get_building t grid_id) in
  let new_buildings = remove_property_helper buildings in  
  match (get_grid t grid_id []) with 
  | (Other (_,_),_,_) ->  failwith "Not buildable"
  | (Land (i,c,p,o,b),prev,tl)
    -> prev @ ((Land (i,c,p,o,new_buildings))::tl)

let demolish (t:t) (grid_id:grid_id) = 
  match (get_grid t grid_id []) with 
  | (Other (_,_),_,_) ->  failwith "Not removable"
  | (Land (i,c,p,o,b),prev,tl)
    -> prev @ ((Land (i,c,p,o,[]))::tl)