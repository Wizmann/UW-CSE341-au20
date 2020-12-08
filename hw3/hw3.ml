#mod_use "hw3types.ml";;

open Hw3types

(* Problem 1
 * Write a function only_lowercase that takes a string list and returns a string list that has only
 * the strings in the argument that start with an lowercase letter. Assume all strings have at least 1
 * character. Use List.filter, Char.lowercase_ascii, and string index access (str.[pos]) to make a
 * 1-2 line solution.
*)

let only_lowercase(strs : string list) : string list =
    strs |> List.filter (fun str ->
        let first = str.[0] in
        first == Char.lowercase_ascii(first))

(* Problem 2
 * Write a function longest_string1 that takes a string list and returns the longest string in the
 * list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning
 * of the list. Use List.fold_left, String.length, and no recursion (other than the fact that the
 * implementation of List.fold_left is recursive).
*)
