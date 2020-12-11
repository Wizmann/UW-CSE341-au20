#mod_use "hw3types.ml";;

open Hw3types

(* helper functions *)
let option_get(item : 'a option) : 'a =
    match item with 
    | Some (v) -> v
    | _ -> assert(false)

let option_is_some(item : 'a option) : bool =
    match item with 
    | Some (v) -> true
    | _ -> false

let option_is_none(item : 'a option) : bool =
    match item with 
    | Some (v) -> false
    | _ -> true

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
 * at least 1 character. Use a "variable-style" let-binding and the % operator from the starter code for
 * composing functions. Resolve ties like in problem 2.
*)

let longest_lowercase = longest_string2 % only_lowercase

(* Problem 6
 * Write a function caps_no_X_string that takes a string and returns the string that is like the input
 * except every letter is capitalized and every "x" or "X" is removed (e.g. "aBxXXxDdx" becomes "ABDD").
 * Use a "variable-style" let-binding, the % operator, 3 library functions in the String module, and 1
 * in the Char module. Browse the module documentation to find the most useful functions.
*)

let caps_no_X_string =
    (String.concat "") % (String.split_on_char 'X') % String.uppercase_ascii

(** The next two problems involve writing functions over lists that will be useful in later problems. **)

(* Problem 7
 * Write a function first_answer of type ('a -> 'b option) -> 'a list -> 'b (notice the 2 arguments are curried).
 * The first argument should be applied to elements of the second argument in order until the first time it
 * returns Some v for some v and then v is the result of the call to first_answer.
 * If the first argument returns None for all list elements, then first_answer should raise the exception NoAnswer.
 * Hints: Sample solution is 7 lines and does nothing fancy.
*)

let first_answer(func : 'a -> 'b option) (items : 'a list) : 'b =
    match (List.map func items) |> List.filter (option_is_some) with
    | hd :: tl -> (option_get hd)
    | [] -> raise NoAnswer

(* Problem 8
 * Write a function all_answers of type ('a -> 'b list option) -> 'a list -> 'b list option
 * (notice the 2 arguments are curried). The first argument should be applied to elements of the second argument.
 * If it returns None for any element, then the result for all_answers is None. Else the calls to the first argument
 * will have produced Some lst1, Some lst2, ... Some lstn and the result of all_answers is Some lst where lst is
 * lst1, lst2, ..., lstn appended together.
 * (Your solution can return these lists appended in any order you want, but for brownie (i.e., no) points, try to
 * use @ in such a way that the total run time of your function is linear, not quadratic.)
 * Hints: The sample solution is 10 lines. It uses a helper function with an accumulator and uses @.
 * Note all_answers f [] should evaluate to Some [].
*)

let all_answers(func : 'a -> 'b list option) (items : 'a list) : 'b list option =
    List.fold_left
        (fun x y ->
            if (option_is_none y) || (option_is_none x) then
                None
            else
                (Some ((option_get x) @ (option_get y)))
        )
        (Some [])
        (List.map func items)

(* The remaining problems use these type definitions, which are inspired by the type definitions OCaml's internals
 * would use to implement pattern matching:
 *   * type pattern = WildcardP | VariableP of string | UnitP | ConstantP of int
 *                  | ConstructorP of string * pattern | TupleP of pattern list
 *   * type valu = Constant of int | Unit | Constructor of string * valu | Tuple of valu list
 * Given valu v and pattern p, either p matches v or not. If it does, the match produces a list of string * valu pairs;
 * order in the list does not matter. The rules for matching should be unsurprising:
 *   * `WildcardP` matches everything and produces the empty list of bindings.
 *   * `VariableP s` matches any value v and produces the one-element list holding (s,v).
 *   * `UnitP` matches only Unit and produces the empty list of bindings.
 *   * `ConstantP 17` matches only Constant 17 and produces the empty list of bindings (and similarly for other integers).
 *   * `ConstructorP(s1,p)` matches Constructor(s2,v) if s1 and s2 are the same string (you can compare them with =) and
 *   *                      p matches v. The list of bindings produced is the list from the nested pattern match. We
 *   *                      call the strings s1 and s2 the constructor name.
 *   * `TupleP ps` matches a value of the form Tuple vs if ps and vs have the same length and for all i, the ith element of
 *   *             ps matches the ith element of vs. The list of bindings produced is all the lists from the nested pattern
 *   *             matches appended together.
 *   * Nothing else matches.
*)

(* Problem 9
 * (This problem uses the pattern recursive variant type but is not really about pattern-matching.)
 * A function g has been provided to you in hw3types.ml.
 * (a) In an OCaml comment in your hw3.ml file, describe in a few English sentences the arguments that
 * g takes and what g computes (not how g computes it, though you will have to understand that to determine
 * what g computes).
 * Note: you write no code for this subproblem, only a comment.
 * (b) Use g to define a function count_wildcards that takes a pattern and returns how many WildcardP
 * patterns it contains.
 * (c) Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns
 * the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
 * in the variable patterns it contains. (Use String.length. We care only about variable names; the constructor
 * names are not relevant.)
 * (d) Use g to define a function count_a_var that takes a string and a pattern (curried) and returns
 * the number of times the string appears as a variable in the pattern. We care only about variable names;
 * the constructor names are not relevant.
*)

(* Answer for (a)
 * Function `g` takes a pattern `p` and two functions `f1` and `f2`, and evaluate the "sum of the value" of the pattern by
 * the element (and nested element) inside it with `f1` and `f2`.
*)

let count_wildcards (p : pattern) : int =
    g (fun () -> 1) (fun _ -> 0) p

let count_wild_and_variable_lengths (p : pattern) : int =
    g (fun () -> 1) (fun str -> (String.length str)) p

let count_a_var (var_key : string) (p : pattern) : int =
    g (fun () -> 0) (fun str -> if str = var_key then 1 else 0) p

(* Problem 10
 * Write a function check_pat that takes a pattern and returns true if and only if all the variables appearing
 * in the pattern are distinct from each other (i.e., use different strings). The constructor names are not
 * relevant.
 * Hints: The sample solution uses two helper functions. The first takes a pattern and returns a list of all the
 * strings it uses for variables. Using List.fold_left with a function that uses @ is useful in one case.
 * The second takes a list of strings and decides if it has repeats. List.exists or List.mem may be useful.
 * Sample solution is 15 lines.
 * These are hints: We are not requiring List.fold_left and List.exists/List.mem here, but they make it easier.
 *)
let check_pat (p : pattern) : bool =
    let rec get_all_tags (p : pattern) (tags : string list) : string list =
        match p with
        | ConstructorP(tag, p) -> (get_all_tags p (tag :: tags))
        | TupleP (ps) -> List.fold_left (fun a b -> (a @ (get_all_tags b []))) tags ps
        | _ -> tags
    in
    
    let no_repeats (items : 'a list) : bool =
        (List.length items) = (List.length (List.sort_uniq compare items))
    in

    let tags = (get_all_tags p []) in
    (no_repeats tags)


(* Problem 11
 * Write a function matches of type valu -> pattern -> (string * valu) list option. It should take a value and
 * a pattern, and return Some `lst` where `lst` is the list of bindings if the value matches the pattern, or None
 * otherwise. Note that if the value matches but the pattern has no variables in it (i.e., no patterns of the
 * form VariableP s), then the result is Some [].
 * Hints: Sample solution has one match expression with 7 branches. The branch for tuples uses all_answers and List.combine.
 * Sample solution is about 15 lines. Remember to look above for the rules for what patterns match what values,
 * and what bindings they produce. These are hints: We are not requiring all_answers and List.combine here,
 * but they make it easier.
*)
let rec matches (v : valu) (p : pattern) : (string * valu) list option = 
    match v, p with
    | Constructor(v_key, v_value), ConstructorP(p_key, p_value) ->
            if v_key = p_key then (matches v_value p_value) else None
    | Tuple (v_items), TupleP(p_items) ->
            if (List.length v_items) != (List.length p_items) then
                None
            else
                (all_answers (fun (v, p) -> (matches v p)) (List.combine v_items p_items))
    | Constant(v), ConstantP(p) -> if v = p then Some [] else None
    | Constant(v_value), VariableP(key) -> Some [(key, v)]
    | Unit, VariableP(key) -> Some [(key, Unit)]
    | Unit, UnitP -> Some []
    | _, WildcardP -> Some []
    | _, _ -> None
;;


(* Problem 12
 * Write a function first_match of type
 * * valu -> pattern list -> (string * valu) list option:
 * It returns None if no pattern in the list matches or Some lst where lst is the list of bindings for the first
 * pattern in the list that matches. Hints: Use first_answer and a try-with-expression.
 * Sample solution is about 3 lines.
*)
let first_match (v : valu) (ps : pattern list) : (string * valu) list option =
    try
        Some (first_answer (fun p -> (matches v p)) ps)
    with
    | NoAnswer -> None
    | _ -> assert(false)


(***** Challenge Problem *****)

(* Write a function typecheck_patterns that "type-checks" a pattern list. Types for our made-up pattern
 * language are defined by:
 * * type typ = AnythingT (* any type of value is okay *)
 *              | UnitT (* type for Unit *)
 *              | IntT (* type for integers *)
 *              | TupleT of typ list (* tuple types *)
 *              | VariantT of string (* some named variant *)
 * typecheck_patterns should have type:
 * * (string * string * typ) list -> pattern list -> typ option:
 *
 * The first argument contains elements that look like ("foo","bar",IntT), which means constructor foo
 * makes a value of type VariantT "bar" given a value of type IntT. Assume list elements all have different
 * first fields (the constructor name), but there are probably elements with the same second field (the
 * variant name). Under the assumptions this list provides, you "type-check" the pattern list to see if there
 * exists some typ (call it t) that all the patterns in the list can have. If so, return Some t, else return None.
 * You must return the "most lenient" type that all the patterns can have. For example, given patterns
 * * TupleP [VariableP "x", VariableP "y"] and TupleP [WildcardP, WildcardP];
 * you should return
 * * Some (TupleT [AnythingT, AnythingT])
 * even though they could both have type TupleT [IntT, IntT]. As another example, if the only patterns
 * are TupleP [WildcardP, WildcardP] and TupleP [WildcardP, TupleP [WildcardP, WildcardP]], you should return
 * Some (TupleT [AnythingT, TupleT[AnythingT, AnythingT]]).
*)


