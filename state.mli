open Board
open Player
open Random

(** [doubles] is the number of doubles rolled. *)
type doubles = int 

(** [t] is a [State.t] type corresponding to the current state of the game. *)
type t

(** [result] is the result of the game. *)
type result = Legal of t | Illegal of string

(** [state i] is the initial state of the game with [i] players. *)
val state : int -> t

(** [add_bal t id n] adds [n] to the player corresponding to [id]. *)
val add_bal : t -> Player.id -> Player.money -> t

(** [exchange_balance t p1 p2 m] is the state with [p1] having [m] less money 
 *  than [t] and [p2] having [m] more money. *)
val exchange_balance : t -> Player.id -> Player.id -> Player.money -> t

(** [roll t] is a state reflecting the dice roll. *)
val roll : t -> result

(** [post_bail t] is a [result] associated with the current player. Is [Legal] 
 *  if the balance of the player is >= 50 and if [get_jail_turn] is not zero, 
 *  and [Illegal] otherwise. *)
val post_bail : t -> result

(** [next_turn t] is the next state of the game. *)
val next_turn : t -> result

(** [check_pchase t] at state [t] is [Some true] if the current property is owned
 *  by the bank, [Some false] if owned by a player, and [None] if an 
 *  exception is thrown when grid is not ownable. *)
val check_pchase : t -> bool option

(** [current_player t] is the current player at state [t] in the game. *)
val current_player : t -> Player.t * Board.grid_id

(** [get_all_players t] is a list of all players and their respective [grid_id] 
 *  positions at a particular state [t]. *)
val get_all_players : t -> (Player.t * Board.grid_id) list

(** [get_all_playerst t] maps [t.players] to a list of only [Player.t] 
 *  elements. *)
val get_all_playerst : t -> Player.t list

(** [get_board t] is a [Board.t] type. *)
val get_board : t -> Board.t

(** [buy_property t] updates the balance for players. *)
val buy_property : t -> result

(** [change_ownsership t grid_id player_id] changes the ownership of [grid_id] 
 *  to the player corresponding to [Player.id]. *)
val change_ownership_state : t -> Board.grid_id -> Player.id -> t

(** [build_property_state t grid_id] builds a building on [grid_id] if it is
 *  legal. *)
val build_property_state : t -> Board.grid_id -> result


(** [mortgage_state grid_id t] mortgages [grid_id] if legal for the current
    player. *)
val mortgage_state : Board.grid_id -> t -> result

(** [unmortgage_state grid_id t] unmortgages [grid_id] if it is legal for the 
 *   current player to do so. *)
val unmortgage_state : Board.grid_id -> t -> result

(** [tradet] is a [result] type for trading between players. *)
type tradet

(** [trade_result] is the [result] after trading. *)
type trade_result = Legalt of tradet | Illegalt of string


(** [form_tradet] forms a [trade_result] type from user input and the current 
 *  state of the game. *)
val form_tradet : string -> t -> trade_result

(** [add_trade] adds the trade given user input, the current state, and a
 *  [tradet] type. *)
val add_trade : string -> tradet -> t -> trade_result

(** [remove_trade] adds the trade given user input and a [tradet] type. *)
val remove_trade : string -> tradet -> trade_result

(** [switch tradet] switches the player by adding or removing. *)
val switch : tradet -> trade_result

(** [confirm_trade tradet t] confirms the transaction of the trade between 
 *  players. *)
val confirm_trade : tradet -> t -> t


(** [info t grid_id] prints information the user might need regarding the 
 *  property, the rent, etc., given a state [t] and grid id [grid_id]. *)
val info : t -> grid_id -> unit

(** [handle_landing st] is the new state after a player rolls and lands on a 
 * grid, if legal. Illegal if not enough money to pay the rent. *)
val handle_landing : t -> result

(** [sell_property state t grid_id] is the new state after selling a building
 *  or the entire property of [grid_id]. *)
val sell_property_state : t -> grid_id -> result

(** [bid (st:t) (player_id:Player.id) (amt_bid:int) (grid_id:Board.grid_id)] 
 *  rewards property to the winner of the bid. *)
val bid : t -> Player.id -> int -> Board.grid_id -> t

(** [print_current_player_property (st:t)] prints all the properties for the 
 *  current player. *)
val print_current_player_property : t -> unit

(** [get_curr_bid_property (st:t) : Board.grid_id] is the current grid id that 
 *  is about to be bid on. *)
val get_curr_bid_property : t -> Board.grid_id

(** [resign t] is the state after the current player resigns. *) 
val resign : t -> t

(** [tradet_current_player tradet] is the current player adding/removing. *)
val tradet_current_player : tradet -> Player.id

(** [price_match st grid] is a list index for purpose of getting rent for 
 *  landing on [grid]*)
val price_match : t -> Board.grid_id -> int