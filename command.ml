type object_phrase = string list

type command = 
  | Quit
  | Resign
  | Roll
  | Bail
  | End

  | Property
  | Info of object_phrase (* grid_id *)

  | Build of object_phrase (* grid_id *)
  | Unbuild of object_phrase (* grid_id *)

  | Bid of object_phrase (* money *)
  | Forfeit

  | Mortgage of object_phrase (* grid_id *)
  | Unmortgage of object_phrase (* grid_id *)

  | Trade of object_phrase (* grid_id *)
  | Add of object_phrase (* money or grid_id *)
  | Remove of object_phrase (* money or grid_id *)
  | Switch
  | Confirm

  | Yes
  | No

  | Back

exception Empty
exception Malformed


(** [grid_ids] is the [grid_id]'s of the board. *)
let grid_ids = [
  "Thurston";
  "Malott";
  "Mann";
  "PSB";
  "Statler";
  "Olin";
  "Duffield";
  "Klarman";"Weill";
  "Uris";
  "Barton";
  "Sage";
  "Kennedy";
  "Phillips";
  "Upson";
  "Day";
]

(** [is_int str] is [true] if [str] is an integer, otherwise [false]. *)
let is_int str =
  match int_of_string str with
  | a -> true
  | exception _ -> false

(** [remove_empty remove_from after_removal] is the list of strings in 
 *  [remove_from] that is not the empty string. *)
let rec remove_empty remove_from (after_removal:string list) = 
  match remove_from with
  | [] -> after_removal
  | h::t -> 
    if h = "" then remove_empty t after_removal
    else h::remove_empty t after_removal 

let parse str =
  let s = remove_empty (String.split_on_char ' ' str) [] in 
  match s with
  | [] -> raise Empty 
  | h::t -> let h = String.lowercase_ascii h in 
    if h = "roll" then (
      if List.length t = 0 then Roll else raise Malformed
    ) else if h = "back" then (
      if List.length t = 0 then Back else raise Malformed
    ) else if h = "end" 
    then (
      if List.length t = 0 then End else raise Malformed
    ) else if h = "quit" 
    then (
      if List.length t = 0 then Quit else raise Malformed
    ) else if h = "bail" 
    then (
      if List.length t = 0 then Bail else raise Malformed
    ) else if h = "resign" 
    then (
      if List.length t = 0 then Resign else raise Malformed
    ) else if h = "property" 
    then (
      if List.length t = 0 then Property else raise Malformed
    ) else if h = "switch" 
    then (
      if List.length t = 0 then Switch else raise Malformed
    ) else if h = "confirm" 
    then (
      if List.length t = 0 then Confirm else raise Malformed
    ) else if h = "yes" 
    then (
      if List.length t = 0 then Yes else raise Malformed
    ) else if h = "no" 
    then (
      if List.length t = 0 then No else raise Malformed
    ) else if h = "forfeit" 
    then (
      if List.length t = 0 then Forfeit else raise Malformed
    ) else if h = "build" 
    then (
      if List.length t = 1 && List.mem (List.nth t 0) grid_ids 
      then Build t 
      else raise Malformed
    ) else if h = "unbuild" 
    then (
      if List.length t = 1 && List.mem (List.nth t 0) grid_ids 
      then Unbuild t 
      else raise Malformed
    ) else if h = "bid" 
    then (
      if List.length t = 1 && is_int (List.nth t 0) 
      then Bid t 
      else raise Malformed
    ) else if h = "mortgage" 
    then (
      if List.length t = 1 && List.mem (List.nth t 0) grid_ids 
      then Mortgage t 
      else raise Malformed
    ) else if h = "unmortgage" 
    then (
      if List.length t = 1 && List.mem (List.nth t 0) grid_ids 
      then Unmortgage t 
      else raise Malformed
    ) else if h = "info" 
    then (
      if List.length t = 1 && List.mem (List.nth t 0) grid_ids 
      then Info t 
      else raise Malformed 
    ) else if h = "trade" 
    then (
      if List.length t = 1 && is_int (List.nth t 0) then Trade t 
      else raise Malformed
    ) else if h = "add" 
    then (
      if List.length t = 1 then begin
        if is_int (List.nth t 0) || List.mem (List.nth t 0) grid_ids 
        then Add t else raise Malformed
      end
      else raise Malformed 
    ) else if h = "remove" 
    then (
      if List.length t = 1 then begin
        if is_int (List.nth t 0) || List.mem (List.nth t 0) grid_ids 
        then Remove t else raise Malformed
      end
      else raise Malformed 
    ) else raise Malformed

