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

let longest_string1(strs : string list) : string =
    List.fold_left (fun a b -> if String.length(a) >= String.length(b) then a else b) "" strs

(* Problem 3
 * Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
 * it returns the string closest to the end of the list. Your solution should be almost an exact copy of
 * longest_string1. Still use List.fold_left and String.length.
*)
let longest_string2(strs : string list) : string =
    List.fold_left (fun a b -> if String.length(a) > String.length(b) then a else b) "" strs

(* Problem 4
 * Write functions longest_string_helper, longest_string3, and longest_string4 such that:
 * * longest_string3 has the same behavior as longest_string1 and longest_string4 has the
 *   same behavior as longest_string2.
 * * longest_string_helper has type (int -> int -> bool) -> string list -> string (notice the currying).
 *   This function will look a lot like longest_string1 and longest_string2 but is more general because
 *   it takes a function as an argument. 
 * * If longest_string_helper is passed a function that behaves like > (so it returns true exactly when
 *   its first argument is stricly greater than its second), then the function returned has the same
 *   behavior as longest_string1.
 * * longest_string3 and longest_string4 are defined using "variable-style" let-bindings, whose right-hand
 *   sides are partial applications of longest_string_helper.
*)

let longest_string_helper(cmp : int -> int -> bool) (strs : string list) : string =
    List.fold_left
        (fun a b -> if (cmp (String.length a) (String.length b)) then a else b) "" (List.rev strs)

let longest_string3(strs : string list) : string =
    longest_string_helper (>) strs

let longest_string4(strs : string list) : string =
    longest_string_helper (>=) strs

(* Problem 5
 * Write a function longest_lowercase that takes a string list and returns the longest string in the
 * list that begins with an lowercase letter, or "" if there are no such strings. Assume all strings have
 * at least 1 character. Use a \variable-style" let-binding and the % operator from the starter code for
 * composing functions. Resolve ties like in problem 2.
*)

(* Problem 6
 * Write a function caps_no_X_string that takes a string and returns the string that is like the input
 * except every letter is capitalized and every "x" or "X" is removed (e.g. "aBxXXxDdx" becomes "ABDD").
 * Use a "variable-style" let-binding, the % operator, 3 library functions in the String module, and 1
 * in the Char module. Browse the module documentation to find the most useful functions.
*)
