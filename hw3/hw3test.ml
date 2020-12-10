#use "hw3.ml"

(* Problem 1 *)
let () = assert(
    (only_lowercase(["foo"; "bar"; "bAz"; "XXX"]) = ["foo"; "bar"; "bAz"])
)

let () = assert(
    (only_lowercase(["XXX"]) = [])
)

let () = assert(
    (only_lowercase([]) = [])
)

(* Problem 2 *)
let () = assert(
    (longest_string1(["foo"; "bar"; "baz"]) = "foo"))

let () = assert(
    (longest_string1(["foo"; "bar123"; "baz"]) = "bar123"))

let () = assert(
    (longest_string1([]) = ""))

(* Problem 3 *)
let () = assert(
    (longest_string2(["foo"; "bar"; "baz"]) = "baz"))

let () = assert(
    (longest_string2(["foo"; "bar"; "baz"; "x"]) = "baz"))

let () = assert(
    (longest_string2(["foo"; "bar123"; "baz"]) = "bar123"))

let () = assert(
    (longest_string2([]) = ""))

(* Problem 4 *)
let () = assert(
    (longest_string3(["foo"; "bar"; "baz"]) = "foo"))

let () = assert(
    (longest_string3(["foo"; "bar123"; "baz"]) = "bar123"))

let () = assert(
    (longest_string3([]) = ""))

let () = assert(
    (longest_string4(["foo"; "bar"; "baz"]) = "baz"))

let () = assert(
    (longest_string4(["foo"; "bar"; "baz"; "x"]) = "baz"))

let () = assert(
    (longest_string4(["foo"; "bar123"; "baz"]) = "bar123"))

let () = assert(
    (longest_string4([]) = ""))

(* Problem 5 *)
let () = assert(
    (longest_lowercase(["foo"; "Barrrrr"; "baz"]) = "baz"))

let () = assert(
    (longest_lowercase(["foo"; "barrrrr"; "baz"]) = "barrrrr"))

(* Problem 6 *)
let () = assert(
    (caps_no_X_string("abcdxxxABCDXXX123") = "ABCDABCD123"))

(* Problem 7 *)
let () = assert(
    (first_answer (fun x -> if (x mod 2 = 0) then Some x else None) [1;2;3;4]) = 2)

let () = assert (
    try
        (first_answer (fun x -> if (x mod 2 = 0) then Some x else None) [1;3;5]) = -1
    with
    | NoAnswer -> true
    | _ -> false
)

(* Problem 8 *)
let () = assert(
    (all_answers (fun x -> if (x mod 2 = 0) then (Some [x]) else None) [1;2;3;4]) = None)

let () = assert(
    (all_answers (fun x -> if (x mod 2 = 0) then (Some [x;x]) else None) [2;4]) = (Some [2;2;4;4]))

(* Problem 9 *)
let () = assert(
    count_wildcards(WildcardP) = 1)

let () = assert(
    count_wildcards(ConstructorP("foo", WildcardP)) = 1)

let () = assert(
    count_wildcards(ConstructorP("foo", TupleP [WildcardP; WildcardP])) = 2)

let () = assert(
    count_wildcards(ConstructorP("foo", TupleP [WildcardP; ConstantP 1])) = 1)

let () = assert(
    count_wildcards(ConstructorP(
        "foo",
        TupleP [WildcardP; VariableP "xyz"; ConstantP 1; TupleP [WildcardP]; UnitP ])) = 2)

let () = assert(
    count_wild_and_variable_lengths(WildcardP) = 1)

let () = assert(
    count_wild_and_variable_lengths(ConstructorP("foo", WildcardP)) = 1)

let () = assert(
    count_wild_and_variable_lengths(ConstructorP("foo", TupleP [WildcardP; VariableP "xyz"])) = 4)

let () = assert(
    (count_a_var "foo" (ConstructorP("foo", TupleP [WildcardP; VariableP "xyz"]))) = 0)

let () = assert(
    (count_a_var "xyz" (ConstructorP("foo", TupleP [WildcardP; VariableP "xyz"]))) = 1)

let () = print_endline("HW3 test OK")
