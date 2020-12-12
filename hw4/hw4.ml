module type INTSET = sig
 type t                          (* abstract type for sets of integer *)
 val empty : t                   (* the empty set *)
 val contains : int -> t -> bool (* checks if an element is in the set *)
 val insert : int -> t -> t      (* returns a new set with the given element added *)
 val remove : int -> t -> t      (* returns a new set with the given element removed *)
 val fold : ('a -> int -> 'a) -> (* "combine" all the elements via the given func. *)
            'a -> t -> 'a        (*   note the order of combination is unspecified *)
 val to_string : t -> string     (* return a string in the format {x1, ..., xn}. *)
                                 (*   in this string, the elements should be in *)
                                 (*   sorted order. *)
end

(* You don't need to understand these next few lines. They make it so Client
   can work with any implementation of INTSET. In the body of Client,
   there is a module I in scope. The only thing the body of Client knows
   about I is that it implements the module type INTSET. *)
module Client (I : INTSET) : sig
  val sum : I.t -> int
  val to_list : I.t -> int list
end = struct
  (* we don't know what I is, but it satisfies the signature INTSET *)
  let sum (s : I.t) : int = failwith "sum: not implemented"
  let to_list (s : I.t) : int list = failwith "to_list: not implemented"
end

module IntSet1 : INTSET = struct
  type t = int list

  let empty = []
  let contains = List.mem
  let insert = List.cons

  let remove = fun _ -> failwith "remove: not implemented"
  let fold = fun _ -> failwith "fold: not implemented"
  let to_string = fun _ -> failwith "to_string: not implemented"
end

module IntSet2 : INTSET = struct
  type t = int list

  let empty = []
  let contains = List.mem

  let insert = fun _ -> failwith "insert: not implemented"
  let remove = fun _ -> failwith "remove: not implemented"
  let fold = fun _ -> failwith "fold: not implemented"
  let to_string = fun _ -> failwith "to_string: not implemented"
end

(* You don't need to understand these two lines. They "instantiate" your Client module
   with the two different implementations of INTSET. *)
module C1 = Client(IntSet1)
module C2 = Client(IntSet2)

(* you can test your client by calling C1.sum and so on *)
