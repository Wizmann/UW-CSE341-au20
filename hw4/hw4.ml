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

  (* Answer for Problem 2:
   * Because plus operator is commutative, it means we can arbitrary change the order of
   * the elements in INTSET without changing the result of function `sum`
  *)
  let sum (s : I.t) : int = (I.fold (+) 0 s)

  (* Answer for Problem 4:
   * Because we can't make any assumption on the order of function `fold`, so we can to do the extra
   * sorting after we get the elements of the list
  *)
  let to_list (s : I.t) : int list = (I.fold (fun lst num -> (num :: lst)) [] s)
end

module IntSet1 : INTSET = struct
  type t = int list

  let empty = []
  let contains = List.mem
  let insert = List.cons

  let remove (to_remove : int) (lst : int list) =
      lst |> (List.filter (fun x -> to_remove != x))
  let fold = List.fold_left 
  let to_string (intset : t) =
      let rec to_string_helper (lst : int list) : string list =
          match lst with
          | [] -> []
          | hd :: tl -> (Printf.sprintf "%d" hd) :: (to_string_helper tl)
      in
      "{" ^ ((fold (fun lst num -> (num :: lst)) [] intset) |> List.sort_uniq compare |> to_string_helper |> String.concat ", ") ^ "}"
end

module IntSet2 : INTSET = struct
  type t = int list

  let empty = []
  let contains = List.mem

  let insert (to_insert : int) (lst : int list) : int list = 
      if (contains to_insert lst) then 
          lst
      else
          let (left, right) = (List.partition (fun x -> x > to_insert) lst) in
          left @ [to_insert] @ right

  let remove (to_remove : int) (lst : int list) =
      lst |> (List.filter (fun x -> to_remove != x))

  let fold = List.fold_left
  let to_string (intset : t) =
      let rec to_string_helper (lst : int list) : string list =
          match lst with
          | [] -> []
          | hd :: tl -> (Printf.sprintf "%d" hd) :: (to_string_helper tl)
      in
      "{" ^ ((fold (fun lst num -> (num :: lst)) [] intset) |> to_string_helper |> String.concat ", ") ^ "}"
end

(* Challenge Problem 9 *)

type bstree = 
    | TreeNode of bstree * bstree * int
    | TreeLeaf

module IntSet3 : INTSET = struct
  type t = bstree

  let empty = TreeLeaf
  let rec contains (x : int) (tree : bstree) : bool = 
      match tree with
      | TreeLeaf -> false
      | TreeNode (left, right, value) ->
              if x = value then
                  true
              else if x < value then
                  (contains x left)
              else
                  (contains x right)

  let rec insert (to_insert : int) (tree : bstree) : bstree = 
      match tree with
      | TreeLeaf -> TreeNode (TreeLeaf, TreeLeaf, to_insert)
      | TreeNode (left, right, value) ->
              if value = to_insert then
                  tree
              else if to_insert < value then
                  TreeNode ((insert to_insert left), right, value)
              else
                  TreeNode (left, (insert to_insert right), value)

  let rec insert_subtree_to_right_most (to_insert : bstree) (tree : bstree) : bstree = 
      match tree with
      | TreeLeaf -> to_insert
      | TreeNode (left, right, value) ->
              TreeNode (left, (insert_subtree_to_right_most to_insert right), value)

  let rec remove (to_remove : int) (tree : bstree) : bstree =
      match tree with
      | TreeLeaf -> TreeLeaf
      | TreeNode (left, right, value) ->
              if value = to_remove then
                  (insert_subtree_to_right_most right left)
              else if value > to_remove then
                  TreeNode ((remove to_remove left), right, value)
              else
                  TreeNode (left, (remove to_remove right), value)

  let rec fold (func : 'a -> int -> 'a) (init : 'a) (tree : bstree) : 'a =
      match tree with
      | TreeLeaf -> init
      | TreeNode (left, right, value) ->
              let vright = (fold func init right) in
              let vmid = (func vright value) in
              let vleft = (fold func vmid left) in

              vleft

  let to_string (intset : t) =
      let rec to_string_helper (lst : int list) : string list =
          match lst with
          | [] -> []
          | hd :: tl -> (Printf.sprintf "%d" hd) :: (to_string_helper tl)
      in
      "{" ^ ((fold (fun lst num -> (num :: lst)) [] intset) |> to_string_helper |> String.concat ", ") ^ "}"
end

(* You don't need to understand these two lines. They "instantiate" your Client module
   with the two different implementations of INTSET. *)
module C1 = Client(IntSet1)
module C2 = Client(IntSet2)
module C3 = Client(IntSet3)

(* you can test your client by calling C1.sum and so on *)

(* Problem 8
 * This as a written question. Write your answers in a comment at the bottom of your hw4.ml file. 
 * Consider the following OCaml code (that does not type check)
 *   let f g = (g "hello", g 0)
 *   let p = f (fun x -> x)
 * (a) Ignore the fact that it does not type check, and suppose evaluate p anyway. Does anything
 * bad happen? If so, what and why? If not, what value do we get as a result?
 * (b) Explain (precisely but relatively briefly) how OCaml's type inference concludes that
 * this code does not type check. Aim for at most a short paragraph.
*)

(* Answer for Problem 8
 * (a) : We can't inference the return type of `fun x -> x`, it means that `p` is not polymorphic by the rule 
 *       of value restriction.
 * (b) : `g "hello"` means that `g` is a function which take a string as the parameter. Meanwhile, `g 0` takes an
 *       integer as the parameter. It leads to a contradiction that for type inference of `g`, and that's the reason
 *       why the code for function `let f g` doesn't type check.
*)


(* Problem 10 Challenge
 * Can you think of any way to modify OCaml's type system to type check the code from problem 8?
 * What consequences does your idea have for type inference?
*)

(* Answer for Problem 10
 * We can add runtime polymorphism to solve the problem on Problem8. But it will lead to some extra overhead during
 * the runtime, and also weaken the compile time type-checking which is important for the correctness and safety of
 * a program.
*)
