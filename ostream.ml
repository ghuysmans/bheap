class type ['a] obj = object
  method next : 'a
  method close : unit
end

let of_channel ch f = object
  method next = f ch
  method close = close_in ch
end

let of_array arr = object
  val mutable i = 0
  method next =
    if i = Array.length arr then
      raise End_of_file
    else (
      let r = arr.(i) in
      i <- i + 1;
      r
    )
  method close = i <- -1 (* FIXME? *)
end

let of_list l = object
  val mutable l = l
  method next =
    match l with
    | [] -> raise End_of_file
    | h :: t -> l <- t; h
  method close = l <- []
end

let of_function f = object
  method next = f ()
  method close = ()
end

class type ['a] cached_obj = object
  method current : 'a
  method next : 'a
  method close : unit
end

let of_fold f init = object
  val mutable acc = init
  method current = acc
  method next = acc <- f acc; acc
  method close = ()
end
(*
let cache o = of_fold (fun () -> o#next) ()
(* FIXME this would break close! *)
*)

let cache o = object
  val mutable cur = o#next
  method current = cur
  method next = cur <- o#next; cur
  method close : unit = o#close
end

(* let's avoid a circular dependency *)
module type Ordered = sig
  type t
  val compare : t -> t -> int
end

exception Unsorted_input

let check_ordered (type t) (module C : Ordered with type t = t) o = object
  method current = o#current
  method next =
    let prev = o#current in
    let cur = o#next in
    if C.compare prev cur > 0 then
      raise Unsorted_input
    else
      cur
  method close = o#close
end
