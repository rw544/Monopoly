type money = int
type id = int
type jail_turn = int
type status = 
  | Pre
  | Post

type t = {
  id : id;
  balance : money;
  jail_turn : jail_turn;
  status : status;
}

let player (player_id : int) = 
  {id = player_id;
   balance = 0;
   jail_turn = 0;
   status = Pre;}

let get_player_id t =
  t.id 

let get_balance t = 
  t.balance

let get_jail_turn t = 
  t.jail_turn

let get_status t = 
  t.status

let change_balance t money= 
  {id = t.id;
   balance = money;
   jail_turn = t.jail_turn;
   status = t.status;}

let add_money t money = 
  {id = t.id;
   balance = t.balance + money;
   jail_turn = t.jail_turn;
   status = t.status;}

let change_jail_turn t jail_turn = 
  {id = t.id;
   balance = t.balance;
   jail_turn = jail_turn;
   status = t.status;}

let cycle_status t = 
  match get_status t with 
  | Pre -> 
    {id = t.id;
     balance = t.balance;
     jail_turn = t.jail_turn;
     status = Post;}
  | Post ->
    {id = t.id;
     balance = t.balance;
     jail_turn = t.jail_turn;
     status = Pre;}

(** [players_helper numm] is a list of [t] with [id]'s from 1 to num, each with
 *  a balance of 0. *)
let rec players_helper num = 
  if num = 0 then [] 
  else player num::players_helper (num-1) 

let players num = 
  let p = List.rev (players_helper num) in 
  List.map (fun x -> change_balance x (1440/num)) p