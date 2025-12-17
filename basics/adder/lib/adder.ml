(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist l : int = 
  let rec aux list = match list with
    [] -> 0
    | x::xs -> x + aux xs 
  in aux l;;
