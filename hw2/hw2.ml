(* CSE 341, Autumn 2020, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
type json =
    Num of float
  | String of string
  | False
  | True
  | Null
  | Array of json list
  | Object of (string * json) list

(* some examples with values with type json *)
let json_pi    = Num 3.14159
let json_hello = String "hello"
let json_false = False
let json_array = Array [Num 1.0; String "world"; Null]
let json_obj   = Object [("foo", json_pi); ("bar", json_array); ("ok", True)]

(* helper function that deduplicate the list *)
let dedup ls = List.sort_uniq compare ls

(* helper function that sort a given list *)
let sort ls = List.sort compare ls

(* We now load 3 files with bus position data represented as values with type json.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively.

   However, the medium and complete files are commented out for now because takes
   a few seconds to load, which is too long while you are debugging
   earlier problems.
*)

;;
#print_depth 3;;
#print_length 3;;

#use "parsed_small_bus.ml";;
(* #use "parsed_medium_bus.ml";; *)
(* #use "parsed_complete_bus.ml";; *)

;;
#print_depth 10;;
#print_length 1000;;

let rec range (i: int) (j: int) : int list =
    if i > j then
        []
    else
        i :: (range (i + 1) j)
;;

let rec reverse items : 'a list =
    match items with
    | [] -> []
    | hd :: tl -> reverse(tl) @ [ hd ]
;;

let option_get (nullable : 'a option) : 'a =
    match nullable with
    | Some (item) -> item
    | _ -> assert false
;;

let option_is_none(nullable : 'a option) : bool =
    match nullable with
    | None -> true
    | _ -> false

let rec map (lambda : ('a -> 'b)) (items : 'a list) : ('b list) =
    match items with
    | [] -> []
    | (hd :: tl) -> (lambda hd) :: (map lambda tl)
;;

let rec flatten (ll : 'a list list) : ('a list) =
    match ll with
    | [] -> []
    | (hd :: tl) -> hd @ (flatten tl)
;;

(* Part 0: Warm-up *)

(* Problem 1
 * Write a function make_silly_json that takes an int i and returns a json.
 * The result should represent a JSON array of JSON objects where every object
 * in the array has two fields, "n" and "b". Every object's "b" field should hold
 * true (i.e., True). The first object in the array should have a "n" field holding
 * the JSON number i.0 (in other words, the integer i converted to a floating point
 * JSON number), the next object should have an "n" field holding (i - 1).0 and so on
 * where the last object in the array has an "n" field holding 1.0.
 * Sample solution is less than 10 lines. Hints: There's a function in OCaml's
 * standard library called float_of_int that converts an integer to a float.
 * You'll want a helper function that does most of the work.
*)

let make_silly_json(n: int) : json = 
    Array (range 1 n |> reverse |> map (fun i -> Object [("n", Num (float_of_int i)); ("b", True)]))
;;

(* Part 1: Printing JSON values *)

(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job. *)
let json_string_of_float (f : float) : string =
  Printf.sprintf "%g" f

(**** PUT PROBLEMS 2-4 HERE ****)

(* Problem 2
 * Write a function concat_with that takes a separator string and a list of strings,
 * and returns the string that consists of all the strings in the list concatenated
 * together, separated by the separator. The separator should be only between elements,
 * not at the beginning or end. Use OCaml's ^ operator for concatenation (e.g.,
 * "hello" ^ "world" evaluates to "helloworld"). Sample solution is 5 lines.
*)

let rec concat_with (delimeter: string) (strs: string list) : string =
    match strs with
    | [] -> ""
    | hd :: [] -> hd
    | hd :: tl -> hd ^ delimeter ^ (concat_with delimeter tl)
;;

(* Problem 3
 * Write a function quote_string that takes a string and returns a string that is the
 * same except there is an additional '"' character (a single quote character) at the
 * beginning and end.
 * Sample solution is 1 line.
*)

let quote_string(str: string) : string =
    "\"" ^ str ^ "\""
;;

(* Problem 4
 * Write a function string_of_json that converts a json into the proper string encoding in
 * terms of the syntax described on the first page of this assignment. The two previous
 * problems are both helpful, but you will also want local helper functions for processing
 * arrays and objects (hint: in both cases, create a string list that you then pass to concat_with).
 * In the Num case, use the provided json_string_of_float function.
 * Sample solution is 25 lines.
*)

let rec string_of_json(obj: json) : string =
    let rec make_key_value (kv: string * json) : string =
        match kv with
        | (key, value) -> (quote_string key) ^ " : " ^ (string_of_json value)
    in

    match obj with
    | Num (num) -> (json_string_of_float num)
    | String (str) -> (quote_string str)
    | False -> "false"
    | True -> "true"
    | Null -> "null"
    | Array (sub_objs) -> "[" ^ (concat_with ", " (map string_of_json sub_objs)) ^ "]"
    | Object (sub_objs) -> "{" ^ (concat_with ", " (map make_key_value sub_objs)) ^ "}"

(* Part 2: Processing JSON values *)

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(**** PUT PROBLEMS 5-20 HERE ****)

(* Don't forget to write a comment for problems 7 and 20. *) 

(* Problem 5
 * Write a function take of type int * 'a list -> 'a list that takes an int called n and
 * a list called l and returns the first n elements of l in the same order as l. You may
 * assume that n is non-negative and does not exceed the length of l.
 * Sample solution is about 5 lines.
*)

let rec take (n: int) (l: 'a list) : 'a list =
    match l with
    | [] -> ignore(assert(n = 0)); []
    | hd :: tl -> 
            if n = 0 then [] else hd :: (take (n - 1)  tl)
;;

(* Problem 6
 * Write a function firsts of type ('a * 'b) list -> 'a list that takes a list of pairs
 * and returns a list of all the first components of the pairs in the same order as the
 * argument list.
 * Sample solution is about 4 lines.
*)
let rec firsts(pairs : ('a * 'b) list) : 'a list =
    match pairs with
    | [] -> []
    | (item1, item2) :: tl -> item1 :: firsts(tl)

(* Problem 7
 * Write a comment in your file after your definition of firsts answering the following
 * questions. Suppose l has type (int * int) list, and let n be an integer between 0 and
 * the length of l (inclusive), and consider the expressions firsts (take (n, l)) and
 * take (n, firsts l). Either (1) write one sentence explaining in informal but precise
 * English why these two expressions always evaluate to the same value; or (2) give example
 * values of l and n such that the two expressions evaluate to different values.
 * Regardless of whether you decide option (1) or option (2) is correct, also write one
 * sentence explaining which of the two expressions above might be faster to evaluate and why.
*)

(* Answer for Problem 7:
 *
 * Assuming we have a list `l` which `length(l) >= n + 1`,
 * and a function `nth` to retrieve the nth element of the list.

 * It's easy for us to know that:
 * ```
 * f(n) = (firsts (take n l)) = (firsts (take n l)) @ (firsts (nth (n + 1) l))
 * g(n) = (take n (firsts l)) = (take n (firsts l)) @ (firsts (nth (n + 1) l))
 * ```

 * It means for a specific length `n`, if f(n) = g(n), we can prove f(n + 1) = g(n + 1).
 * And it's trival that f(0) = g(0). We can use mathmatical induction to prove that
 * for every n, there will be f(n) = g(n)
*)

(* Problem 8
 * Write a function assoc of type 'a * ('a * 'b) list -> 'b option that takes two arguments
 * k and xs. It should return Some v1 if (k1,v1) is the pair in the list closest to the beginning
 * of the list for which k and k1 are equal. If there is no such pair, assoc returns None.
 * Sample solution is a few lines. (Note: There's a function with similar functionality in the
 * OCaml standard library, but calling it requires features we haven't covered yet.
 * Do not use that function or you will not receive credit.)
*)

let rec assoc (key : 'a) (xs : ('a * 'b) list) : 'b option =
    match xs with
    | [] -> None
    | (k, v) :: tl -> if key = k then Some v else (assoc key tl)
;;

(* Problem 9
 * Write a function dot that takes a json (call it j) and a string (call it f) and returns a
 * json option. 
 * If j is an object that has a field named f, then return Some v where v is the contents of
 * that field.
 * If j is not an object or does not contain a field f, then return None.
 * Sample solution is 4 short lines thanks to an earlier problem.
*)

let rec dot (j : json) (f : string) : json option =
    match j with
    | Object (hd :: tl) -> if fst(hd) = f then (Some (snd hd))
                           else (dot (Object tl) f)
    | _ -> None

(* Problem 10
 * Write a function dots that takes a json called j and a string list called fs that represents
 * an access path, or in other words, a list of field names. The function dots returns a json option
 * by recursively accessing the fields in fs, starting at the beginning of the list. If any of the
 * field accesses occur on non-objects, or to fields that do not exist, return None.
 * Otherwise, return Some v where v is the value of the field "pointed to" by the access path.
 * (Hint: Use recursion on fs plus your solution to the previous problem.)
 * Sample solution is about 7 lines.
*)

let rec dots (j : json) (fs : string list) : json option =
    match fs with
    | [] -> assert false
    | hd :: [] -> (dot j hd)
    | hd :: tl ->
            let sub_j = (dot j hd) in
            if sub_j = None then None
            else (dots (option_get sub_j) tl)

(* Problem 11
 * Write a function one_fields that takes a json and returns a string list. If the argument is an
 * object, then return a list holding all of its field names (not field contents). Else return the
 * empty list.
 * Use a tail-recursive, locally defined helper function. The list you return can be in any order,
 * but it is probably easiest to have the results in reverse order from how they appear in the
 * object, and this reverse order is fine/expected. Sample solution is about 10 lines.
*)

let one_fields (j : json) : string list =
    let rec one_fields_helper (kv : (string * json) list) (keys : string list) : string list =
        match kv with
        | [] -> keys
        | (hd :: tl) -> (one_fields_helper tl (fst(hd)::keys))
    in

    match j with
    | Object (obj) -> (one_fields_helper obj [])
    | _ -> []
;;

(* Problem 12
 * Write a function no_repeats that takes a string list and returns a bool that is true if and
 * only if no string appears more than once in the input. Do not (!) use any explicit recursion.
 * Rather, use provided helper function dedup (which returns its argument without duplicates)
 * together with standard library function List.length to complete this problem in one line.
*)

let no_repeats (strs : string list) : bool =
    (List.length (dedup strs)) = (List.length strs)
;;

(* Problem 13
 * Write a function recursive_no_field_repeats that takes a json and returns a bool that is
 * true if and only if no object anywhere "inside" (arbitrarily nested) the json argument has
 * repeated field names. (Notice the proper answer for a json value like False is true. Also
 * note that it is not relevant that different objects may have field names in common.)
 * In addition to using some of your previous functions, you will want two locally defined
 * helper functions for processing the elements of a JSON array and the contents of a JSON
 * object's fields. By defining these helper functions locally, rather than at the top level,
 * they can call recursive_no_field_repeats in addition to calling themselves recursively.
 * Sample solution is about 15 lines.
*)
let rec get_all_keys (obj : json) : string list = 
    match obj with
    | Object [] -> []
    | Object ((key, value) :: tl) -> [ key ] @ (get_all_keys value) @ (get_all_keys (Object tl))
    | Array (sub_objs) -> (flatten (map get_all_keys sub_objs))
    | _ -> []

let recursive_no_field_repeats (obj : json) : bool =
    ( no_repeats (get_all_keys obj) )

(* Problem 14
 * Write a function count_occurrences of type string list * exn -> (string * int) list.
 * If the string list argument is sorted (using OCaml's built-in comparison operator, <),
 * then the function should return a list where each string is paired with the number of times
 * it occurs. (The order in the output list does not matter.) If the list is not sorted, then
 * raise the exn argument. Your implementation should make a single pass over the string list
 * argument, primarily using a tail-recursive helper function.
 * You will want the helper function to take a few arguments, including the "current" string
 * and its "current" count.
 * Sample solution is about 12 lines.
*)

let rec count_occurrences_helper (current : (string * int)) (strlist : string list) (result : (string * int) list) (error : exn) : ((string * int) list) =
        match strlist with
        | [] -> if (snd current) > 0 then (result @ [ current ]) else result
        | hd :: tl ->
                if hd < (fst current) then
                    (raise error)
                else if hd = (fst current) then
                    (count_occurrences_helper ((fst current), ((snd current) + 1)) tl result error)
                else if (snd current) > 0 then
                    (count_occurrences_helper (hd, 1) tl (result @ [ current ]) error)
                else
                    (count_occurrences_helper (hd, 1) tl result error)

let count_occurrences (strlist : string list) (error : exn) : (string * int) list =
    (count_occurrences_helper ("", 0) strlist [] error)

(* Problem 15
 * Write a function string_values_for_access_path of type
 *    (string list) * (json list) -> string list
 * (the parentheses in this type are optional, so the REPL won't print them).
 * For any object in the json list that has a field available via the given "access path"
 * (string list), and has contents that are a JSON string (e.g., String "hi") put the contents
 * string (e.g., "hi") in the output list (order does not matter; the output should have
 * duplicates when appropriate).
 * Sample solution is 6 lines thanks to dots.
*)
let rec string_values_for_access_path (path : string list) (jsonlist : json list) : string list =
    let get_string_list_from_value (j : json option) : string list =
        match j with
        | Some String (s) -> [ s ]
        | _ -> []
    in
    match jsonlist with
    | [] -> []
    | hd :: tl -> (get_string_list_from_value (dots hd path)) @ (string_values_for_access_path path tl)
;;

(* Problem 16
 * Write a function filter_access_path_value of type
 *     string list * string * json list -> json list
 * The output should be a subset of the third argument, containing exactly those elements of
 * the input list that have a field available via the given access path, and that field's
 * contents are a JSON string equal to the second argument.
 * Sample solution uses dots and is less than 10 lines.
*)

let rec filter_access_path_value (path : string list) (value : string) (jsonlist : json list) : json list =
    match jsonlist with
    | [] -> []
    | hd :: tl -> (if (dots hd path) = (Some (String value)) then [ hd ] else [ ]) @ (filter_access_path_value path value tl)

(* Problem 17
 * Some of the bus data uses latitude and longitude positions to describe the location of vehicles in
* real time. To narrow our focus onto a particular geographical area, we will use the record types rect
* and point, which represent a rectangle and a point, respectively. The types are defined in the starter
* code, but copied here for completeness.
*     type rect = { min_latitude: float; max_latitude: float; min_longitude: float; max_longitude: float }
*     type point = { latitude: float; longitude: float }
* Write a function in_rect of type rect * point -> bool that determines whether a given point falls
* inside a given rectangle (inclusive).
* Solution is two lines and uses a lot of conjunction (&&).
*)

let in_rect (area : rect) (position : point) : bool =
    (area.min_latitude <= position.latitude && position.latitude <= area.max_latitude &&
     area.min_longitude <= position.longitude && position.longitude <= area.max_longitude)

(* Problem 18
 * Write a function point_of_json of type json -> point option. If the argument is a json object that
 * contains fields named "latitude" and "longitude", both of which are json numbers, then point_of_json
 * returns `Some p` where p is the point represented by these coordinates. Otherwise, it returns `None`.
 * Solution is 5 lines and uses dot and nested patterns.
*)

let point_of_json (j : json) : point option =
    let extract_num_from_json key =
        match (dot j key) with
        | Some Num (num) -> (Some num)
        | _ -> None
    in
    let lati = (extract_num_from_json "latitude") in
    let long = (extract_num_from_json "longitude") in
    if not (option_is_none lati) && not (option_is_none long) then
        Some { latitude = (option_get lati); longitude = (option_get long) }
    else
        None

(* Problem 19
 * Write a function filter_access_path_in_rect of type
 *     string list * rect * json list -> json list
 * The output should be a subset of the third argument, containing exactly those elements of the input list
 * that (1) have a field available via the given access path, (2) that field's contents are a JSON object that
 * can be converted to a point via point_of_json, and (3) the resulting point is within the rectangle
 * specified by the second argument.
 * Sample solution is less than 15 lines.
*)
let rec filter_access_path_in_rect (path : string list) (area : rect) (jsonlist : json list) : json list =
    match jsonlist with
    | [] -> []
    | hd :: tl ->
            match (dots hd path) with
            | Some (obj) ->
                    let p = (point_of_json obj) in
                    if (option_is_none p) then []
                    else if (in_rect area (option_get p)) then [ hd ]
                    else []
            | _ -> []
            @ (filter_access_path_in_rect path area tl)

(* Problem 20
 * After your definition of filter_access_path_in_rect, write a comment containing 1-3 sentences describing
 * the similarities with filter_access_path_value. Can you think of any way to refactor these two function
 * to use a common, more general function? (Do not actually do this refactoring.)
 * On a scale from 1 to "run-time error", how annoyed are you about having to write both of these functions
 * on the same homework?
*)

(* histogram and histogram_for_access_path are provided, but they use your
   count_occurrences and string_values_for_access_path, so uncomment them
   after doing earlier problems *)
exception SortIsBroken

(* 
let histogram (xs : string list) : (string * int) list =
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences (sorted_xs,SortIsBroken) in
  let compare_counts ((s1 : string), (n1 : int)) ((s2 : string), (n2 : int)) : int =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs, js) =
  histogram (string_values_for_access_path (fs, js))
*)

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude = 47.648637;
    min_longitude = -122.322099;
    max_latitude = 47.661176;
    max_longitude = -122.301019
  }

(* Part 3: Analyzing the data *)

;;
#print_depth 3;;
#print_length 3;;

(* uncomment these lines *and* the lines at the top of the file that load "parsed_complete_bus.ml"
   when ready to work on part 3 *)
(* 
let complete_bus_positions_list =
  match dot(complete_bus_positions, "entity") with
  | Some (Array l) -> l
  | _ -> failwith "complete_bus_positions_list"
*)
;;
#print_depth 10;;
#print_length 1000;;

(**** PUT PROBLEMS 21-26 HERE ****)

(* see hw2challenge.ml for challenge problems *)
