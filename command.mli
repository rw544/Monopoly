(** [object_phase] is a list of strings that represent the user input. *)
type object_phrase = string list

(** [command] is the type of commands the user can input. *)
type command = 
  | Quit
  | Resign
  | Roll
  | Bail
  | End

  | Property 
  | Info of object_phrase

  | Build of object_phrase
  | Unbuild of object_phrase

  | Bid of object_phrase
  | Forfeit

  | Mortgage of object_phrase
  | Unmortgage of object_phrase

  | Trade of object_phrase
  | Add of object_phrase
  | Remove of object_phrase
  | Switch
  | Confirm

  | Yes
  | No

  | Back

exception Empty
exception Malformed

(** [parse str] parses the inputting string by the user into a command that is 
 *  acceptable by the program. *)
val parse : string -> command