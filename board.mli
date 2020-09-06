open Player

(** [other_type] is the type of grid that is not a property. 
 *  [Go] is the starting grid
 *  [Jail] is the jail grid
 *  [GoJail] forces the player to go to Jail
 *  [Free] is the free space. *)
type other_type = 
  | Go
  | Jail
  | GoJail
  | Free
  | Chest

(** [color] is the color attribute of a property. *)
type color = 
  | Red
  | Blue
  | Green
  | Yellow

(** [building] is the building types possible in a property. *)
type building =
  | House 
  | Hotel

(** [grid_id] is the id of grid. *)
type grid_id = string

(** [price] is the price of a property. *)
type price = int 

(** [grid] is a type of grid. It is either an other type, or a property with
 *  color, price, owner, and a list of buildings on the property. *)
type grid = 
  | Other of (grid_id * other_type)
  | Land of (grid_id * color * price * Player.id * building list)


type t = grid list

(** [infot] is the informatin about the board. It is used to get mortgage,
 *  housing, and other informations about a certain grid. *)
type infot = (grid_id * int list) list

(** [board] returns [t] in its original state. *)
val board : t

(** [al_info] is an association list of grid id's - [Rent, Rent w/ Color
 *  Set, 1 House, 2 House, 3 House, 4 House, Hotel, Mortgage, House Cost, Hotel 
 *  Cost]. *)
val al_info : infot

(** [get grid grid_id t prev] is the tuple consisting of 
 *   1) the grid corresponding to the grid_id
 *   2) a list of all grids before this grid
 *   3) a list of all the grids after this grid. *)
val get_grid :  t -> grid_id -> grid list -> grid * grid list * grid list

(** [get_grid_only t grid_id] is the grid corresponding to the [grid_id]. *)
val get_grid_only : t -> grid_id -> grid

(** [get_color t grid_id] is the color of the grid corresponding to [grid_id].*)
val get_color : t -> grid_id -> color

(** [get_price t grid_id] is the price of the grid corresponding to [grid_id].*)
val get_price : t -> grid_id -> price

(** [get_ownership t grid_id] is the owner of the grid corresponding to 
 *  [grid_id]. *)
val get_ownership : t -> grid_id -> Player.id 

(** [get_building t grid_id] is the list of all buildings of the grid 
 *  corresponding to [grid_id], is empty list if there are no buildings. *)
val get_building : t -> grid_id -> building list

(** [forward t id n] is the grid_id of the grid [n] steps ahead of [grid_id] 
 *  and true if passed GO, false if did not pass GO. Landing on GO counts as 
 *  passing *)
val get_forward : t -> grid_id -> int -> grid_id * bool

(** [change_ownership t grid_id player_id] changes the grid of [grid_id] to have 
 *  ownership of [player_id]. *)
val change_ownership : t -> grid_id -> Player.id  -> t

(** [own_color t c owner] is true if all property of [c] is owned by [owner] 
 *  disregarding mortgages, and false otherwise. *)
val own_color : t -> color -> Player.id  -> bool

(** [color_not_mort t c owner] is true if all property of [c] is owned by 
 *  [owner] and is not mortgaged, and false otherwise. *)
val color_not_mort : t -> color -> Player.id  -> bool

(** [color_buildings t color adj_list] is an adjacency list of [grid_id] with
 *  [color] to property list. *)
val color_buildings : t -> color -> (grid_id * building list) list ->
  (grid_id * building list) list

(** [check_built t color adj_list] checks if any properties are built on the 
 *  properties of [color].  *)
val check_built : t -> color -> (grid_id * building list) list -> bool

(** [player_property t id] is a grid list of all properties owned by [id]. *)
val player_property : t -> Player.id -> grid_id list

(** [build_property t grid_id] builds a property on [grid_id]. *)
val build_property : t -> grid_id -> t

(** [remove_property t grid_id] removes a property on [grid_id]. *)
val remove_property : t -> grid_id -> t

(** [demolish t grid_id] removes all the buildings on [grid_id]. *)
val demolish : t -> grid_id -> t

