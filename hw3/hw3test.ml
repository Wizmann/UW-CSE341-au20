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

(* Problem 10 *)

let () = assert(
    check_pat(TupleP [ConstructorP("foo", WildcardP); ConstructorP("bar", ConstantP(1))]) = true)

let () = assert(
    check_pat(TupleP [ConstructorP("foo", WildcardP); ConstructorP("foo", ConstantP(1))]) = false)

let () = assert(
    check_pat(TupleP [
        ConstructorP("foo", WildcardP);
        ConstructorP("bar", ConstantP(1));
        TupleP [
            ConstructorP("foo", WildcardP);
            ConstructorP("bar", ConstantP(1));
        ];
    ]) = false)

(* Problem 11 *)
let () = assert(
    (matches (Constant 1) WildcardP) = Some [])

let () = assert(
    (matches (Constant 1) (ConstantP 2)) = None)

let () = assert(
    (matches (Constant 1) (VariableP "foo")) = (Some [("foo", (Constant 1))]))

let () = assert(
    (matches 
        ( Tuple [(Constant 1); Unit] )
        ( TupleP [(VariableP "foo")] ) ) = None)

let () = assert(
    (matches 
        ( Tuple [(Constant 1); Unit] )
        ( TupleP [(VariableP "foo"); UnitP] ) ) = (Some [("foo", (Constant 1))]))

(* Problem 12 *)
let () = assert(
    (first_match (Constant 1) [WildcardP]) = Some [])

let () = assert(
    (first_match (Constant 1) [UnitP; WildcardP]) = Some [])

let () = assert(
    (first_match (Constant 1) [UnitP; (ConstantP 1)]) = Some [])

let () = assert(
    (first_match (Constant 1) [UnitP; (ConstantP 2)]) = None)

(* Challenge Problem *)

let () = assert(
    (typecheck_patterns [] [TupleP [VariableP "x"; VariableP "y"]; TupleP [WildcardP; WildcardP]]) = Some(TupleT [AnythingT; AnythingT]))

let () = assert(
    (typecheck_patterns [] []) = None)

let () = assert(
    (typecheck_patterns [] [TupleP [WildcardP; TupleP [WildcardP; WildcardP]]; TupleP [WildcardP; WildcardP]]) =
        Some(TupleT [AnythingT; TupleT [AnythingT; AnythingT]]))

let () = assert(
    (typecheck_patterns [] [ConstantP 5; WildcardP; ConstantP 3; VariableP "x"]) = Some(IntT))

let () = assert(
    (typecheck_patterns [] [ConstantP 5; UnitP]) = None)

let() = assert(
    (typecheck_patterns [("c", "t", IntT)]  [ConstructorP("c", ConstantP 5); ConstantP 5]) = None)

let () = assert(
    (typecheck_patterns [] [TupleP [VariableP "x"]; TupleP [WildcardP; WildcardP]]) = None)

let () = assert(
    (typecheck_patterns [] [TupleP [WildcardP]; TupleP [WildcardP; WildcardP]]) = None)

let () = assert(
    (typecheck_patterns [] [TupleP [WildcardP; ConstantP 1]; TupleP [WildcardP; TupleP [WildcardP]]]) = None)

let () = assert(
    (typecheck_patterns [] [ConstructorP("c", VariableP "x")]) = None)

let () = assert(
    (typecheck_patterns
        [("c", "t", TupleT [IntT; AnythingT])]
        [ConstructorP("c", TupleP [ConstantP 4; VariableP "x"])]) = Some(VariantT "t"))

let () = assert(
    (typecheck_patterns
        [("c1", "t1", UnitT); ("c2", "t2", UnitT)]
        [ConstructorP("c1", UnitP); ConstructorP("c2", UnitP)]) = None)

let () = assert(
    (typecheck_patterns
        [("c1", "t1", UnitT); ("c2", "t2", UnitT)]
        [ConstructorP("c1", UnitP); ConstructorP("c1", UnitP)]) = Some(VariantT "t1"))

let () = assert(
    (typecheck_patterns
        [("c", "t", TupleT[AnythingT; AnythingT])]
        [ConstructorP("c", TupleP [ConstantP 4; VariableP "x"])]) = None) 

let () = assert(
    (typecheck_patterns
        [("c", "t", IntT)]
        [TupleP [WildcardP; TupleP [ConstantP 3; WildcardP]];
         TupleP [WildcardP; TupleP [VariableP "x"; UnitP]];
         TupleP [ConstructorP("c", ConstantP 13); WildcardP]])
    = Some(TupleT [VariantT "t"; TupleT [IntT; UnitT]]))

let () = assert(
    (typecheck_patterns
        [("c1", "t", TupleT[IntT; VariantT "t"]); ("c2", "t", UnitT)]
        [ConstructorP("c1", TupleP [ConstantP 5; ConstructorP("c2", UnitP)]); ConstructorP("c2", UnitP)])
    = Some(VariantT "t"))

let () = assert(
    (typecheck_patterns
        [("foo1", "bar1", VariantT "bar2"); ("foo2", "bar2", UnitT)]
        [ConstructorP("foo1", VariableP "x")])
    = Some (VariantT "bar1"))

let () = print_endline("HW3 test OK")
