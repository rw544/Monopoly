open Graphics
open Board
open State
open Player


(* values of colors *)
let black = Graphics.rgb 0 0 0
let yellow_grid = Graphics.rgb 255 255 153
let blue_grid = Graphics.rgb 77 148 255
let green_grid = Graphics.rgb 102 255 153
let red_grid = Graphics.rgb 255 153 153
let grey_grid = Graphics.rgb 184 184 148
let orange_grid = Graphics.rgb 255 214 51
let purple_grid = Graphics.rgb 255 102 255
let light_grey_grid = Graphics.rgb 235 235 224
let pink_grid = Graphics.rgb 204 102 255

let brick_red_house = Graphics.rgb 128 0 0
let orange_hotel = Graphics.rgb 255 128 0

let light_blue_player = Graphics.rgb 173 216 230

let leaderboard_color = Graphics.rgb 255 204 153


(* a list of ordered pairs representing grid locations *)
let map_location grid_id = 
  let ass_list = [
    ("o1", (630,0));
    ("Thurston", (540,0));
    ("o2", (450,0));
    ("Malott", (360,0));
    ("o3", (270,0));
    ("Mann", (180,0));
    ("PSB", (90,0));
    ("o4", (0,0));
    ("Statler", (0,90));
    ("Olin", (0,180));
    ("Duffield", (0,270));
    ("o5", (0,360));
    ("o6", (0,450));
    ("Klarman", (0,540));
    ("o7", (0,630));
    ("Weill", (90,630));
    ("Uris", (180,630));
    ("o8", (270,630));
    ("Barton", (360,630));
    ("Sage", (450,630));
    ("o9", (540,630));
    ("o10", (630,630));
    ("Kennedy", (630,540));
    ("Phillips", (630,450));
    ("o11", (630,360));
    ("o12", (630,270));
    ("Upson", (630,180));
    ("Day", (630,90));
  ]
  in List.assoc grid_id ass_list

(* [player_location i] is player [i]'s pixel location in a grid so that no 
 * player overlaps *)
let player_location i = 
  let ass_list = [ 
    (1, (10,65));
    (2, (35,65));
    (3, (60,65));
    (4, (10,40));
    (5, (35,40));
    (6, (60,40));
  ]
  in List.assoc i ass_list

(** [loop_arr color m n] makes an array of arrays of [color] of m length of n 
 *  length with [black] near the edges. In graphics, this is a box with color of
 *  [color] and [m] by [n] pixels. *)
let loop_arr (c:Graphics.color) m n = 
  Array.append (
    Array.append (Array.make 1 (Array.make m black))
      (Array.make (n-2) 
         (Array.append 
            (Array.append (Array.make 1 black) (Array.make (m-2) c)) 
            (Array.make 1 black)))) (Array.make 1 (Array.make m black))


(** [opg_board ()] opens a window displaying the board with no players and no
 *  buildings and no names. *)
let opg_board () = 
  open_graph " 720x720";
  (* "o1" Start *)
  draw_image (make_image (loop_arr orange_grid 90 90)) 630 0;
  (* "Thurson" *)
  draw_image (make_image (loop_arr red_grid 90 90)) 540 0;
  (* "o2" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 450 0;
  (* "Malott" *)
  draw_image (make_image (loop_arr red_grid 90 90)) 360 0;
  (* "o3" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 270 0;
  (* "Mann" *)
  draw_image (make_image (loop_arr red_grid 90 90)) 180 0;
  (* "PSB" *)
  draw_image (make_image (loop_arr red_grid 90 90)) 90 0;
  (* "o4" *)
  draw_image (make_image (loop_arr grey_grid 90 90)) 0 0;

  (* "Statler" *)
  draw_image (make_image (loop_arr green_grid 90 90)) 0 90;
  (* "Olin" *)
  draw_image (make_image (loop_arr green_grid 90 90)) 0 180;
  (* "Duffield" *)
  draw_image (make_image (loop_arr green_grid 90 90)) 0 270;
  (* "o5" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 0 360;
  (* "o6" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 0 450;
  (* "Klarman" *)
  draw_image (make_image (loop_arr green_grid 90 90)) 0 540;
  (* "o7" *)
  draw_image (make_image (loop_arr purple_grid 90 90)) 0 630;

  (* "Weill" *)
  draw_image (make_image (loop_arr yellow_grid 90 90)) 90 630;
  (* "Uris" *)
  draw_image (make_image (loop_arr yellow_grid 90 90)) 180 630;
  (* "o8" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 270 630;
  (* "Barton" *)
  draw_image (make_image (loop_arr yellow_grid 90 90)) 360 630;
  (* "Sage" *)
  draw_image (make_image (loop_arr yellow_grid 90 90)) 450 630;
  (* "o9" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 540 630;
  (* "o10" *)
  draw_image (make_image (loop_arr grey_grid 90 90)) 630 630;

  (* "Kennedy" *)
  draw_image (make_image (loop_arr blue_grid 90 90)) 630 540;
  (* "Phillips" *)
  draw_image (make_image (loop_arr blue_grid 90 90)) 630 450;
  (* "o11" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 630 360;
  (* "o12" *)
  draw_image (make_image (loop_arr light_grey_grid 90 90)) 630 270;
  (* "Upson" *)
  draw_image (make_image (loop_arr blue_grid 90 90)) 630 180;
  (* "Day" *)
  draw_image (make_image (loop_arr blue_grid 90 90)) 630 90;

  moveto 260 350; set_color red; set_text_size 100; 
  draw_string "MONOPOLY (CORNELL THEMED EDITION)";
  moveto 660 35; draw_string "Start";
  moveto 640 15; draw_string "Collect $200";
  moveto 30 35; draw_string "Jail";
  moveto 15 665; draw_string "Free Space";
  moveto 648 665; draw_string "Go to Jail"


(** [draw_players p_lst] draws each player in their respective location on 
 *  the board. *)
let rec draw_players p_lst = 
  match p_lst with
  | [] -> ()
  | (p,l)::t -> 
    let (a,b) = map_location l in 
    let (c,d) = player_location (Player.get_player_id p) in
    draw_image (make_image (loop_arr light_blue_player 20 20)) (a+c) (b+d);
    moveto (a+c+7) (b+d+3);  
    draw_string (string_of_int (Player.get_player_id p));
    draw_players t

(** [draw_id x y id p o] draws [grid_id]'s text on the grid. *)
let draw_id x y id p o = 
  moveto (x+3) (y+3);
  if o = 0 then 
    draw_string (id ^ ": $" ^ (string_of_int p))
  else 
    draw_string (id ^ ": P" ^ (string_of_int o))

(** [draw_buildings x y bl] draws [buildings] at [x y] location.*)
let rec draw_buildings x y bl = 
  match bl with 
  | [] -> ()
  | h::t -> begin 
      if h = House then 
        begin 
          draw_image (make_image (loop_arr brick_red_house 10 10)) x y;
          draw_buildings (x+15) y t
        end
      else
        draw_image (make_image (loop_arr orange_hotel 15 15)) x y
    end

(** [draw_property_helper board] draws all the grids in the board with 
 *  information about price and buildings of [board]. *)
let rec draw_property_helper (board:grid list) = 
  match board with 
  | Other (_,_)::t -> draw_property_helper t
  | Land (id,_,price,owner,buildings)::t -> begin
      let (x,y) = map_location id in 
      begin 
        draw_id x y id price owner
      end;
      begin 
        match buildings with 
        | h::t -> draw_buildings (x+5)(y+25) buildings
        | [] -> (); 
      end;
      draw_property_helper t
    end
  | [] -> ()

(** [draw_property state] draws all the grids in the board with 
 *  information about price and buildings of [st]. *)
let draw_property st =
  draw_property_helper (State.get_board st)


(** [insert elt] inserts element [elt] into a list passed in as a parameter. *)
let rec insert elt = function
  | [] -> [elt]
  | h::t -> if Player.get_balance (fst elt) < Player.get_balance (fst h)
    then h::insert elt t
    else elt::h::t

(** [order_player_benefit] sorts an inputted list using insertion sort. *)
let rec order_player_benefit = function
  | [] -> []
  | h::t -> insert h (order_player_benefit t)

(** [draw_text lst a] displays the money of the first player in [lst] at 
 *  location [460,a]. *)
let rec draw_text (lst) (a:int) =
  match lst with
  | [] -> ()
  | h::t -> (); 
    begin 
      moveto 460 a; 
      draw_string 
        ("Player " ^ (string_of_int (Player.get_player_id (fst h)))^ ": $" ^
         (string_of_int (Player.get_balance (fst h)))); 
      draw_text t (a-30)
    end

(** [draw_leaderboard st] draws the leaderboard of [st], displaying money of all
 *  the players in order from greatest (top) to least (bottom). *)
let draw_leaderboard (st:State.t) =
  let num = (List.length (State.get_all_players st)) in 
  let length = (50 + (num*30)) in 
  draw_image 
    (make_image (loop_arr leaderboard_color 150 length)) 450 (400+(200-length));
  draw_text (order_player_benefit (State.get_all_players st)) 540;
  moveto 475 570;
  draw_string "Money Leaderboard"



let show (t:State.t) = 
  opg_board ();
  draw_players (get_all_players t);
  draw_leaderboard t;
  draw_property t;