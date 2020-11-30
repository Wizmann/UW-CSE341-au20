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


(* Part 0: Warm-up *)
(**** PUT PROBLEM 1 HERE ****)

(* Part 1: Printing JSON values *)

(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job. *)
let json_string_of_float (f : float) : string =
  Printf.sprintf "%g" f

(**** PUT PROBLEMS 2-4 HERE ****)



(* Part 2: Processing JSON values *)

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(**** PUT PROBLEMS 5-20 HERE ****)

(* Don't forget to write a comment for problems 7 and 20. *) 


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
