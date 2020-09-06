(** [money] represents the balance each player will hold. *)
type money = int

(** [player_id] represents the id of a particular player. *)
type id = int

(** [jail_turn] represents the number of turns spent in jail. *)
type jail_turn = int

(** [status] represents the status of the player in the game. A player may be in
 *  [Queue], which means it is not their turn yet. The player in their turn is 
 *  in either [Phase_One] or [Phase_Two], one represents player in a phase 
 *  where they can roll the die and move, the other one represents the player 
 *  finished rolling and doing other actions. *)
type status = 
  | Pre
  | Post

type t 

(** [player id] is a [Player.t] where the player id is [player_id]. *)
val player : id -> t

(** [players num] is a [Player.t] list with [num] players with id from 1 to 
 *  [num]. *)
val players : int -> t list

(** [get_player_id t] is the player id of [t]. *)
val get_player_id : t -> id

(** [get_balance t] is a the balance of [t]. *)
val get_balance : t -> money

(** [get_jail_turn t] is the number of turn a player has been in jail for. *)
val get_jail_turn : t -> jail_turn

(** [get_status t] is a the status of [t]. *)
val get_status : t -> status

(** [change_balance t] changes [money] to [t.money]. *)
val change_balance : t -> money -> t

(** [change_balance t] adds [money] to [t.money]. *)
val add_money : t -> money -> t

(** [change_jail_turn t j_t] changes [jail_turn] of [t] to [j_t]. *)
val change_jail_turn : t -> jail_turn -> t

(** [cycle_status t] cycles [status] of [t]. *)
val cycle_status : t -> t