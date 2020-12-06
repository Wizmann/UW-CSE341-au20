(* CSE 341, Homework 2 Tests *)

#use "hw2.ml";;

(* You will surely want to add more! *)

(* Problem 1 *)
let () = 
  assert( (make_silly_json 2) = 
      Array [Object [("n", Num 2.); ("b", True)]; Object [("n", Num 1.); ("b", True)]]
  )

let () = 
  assert( (make_silly_json 1) = 
      Array [Object [("n", Num 1.); ("b", True)]; ]
  )

let () = 
  assert( (make_silly_json 0) = Array [] )

(* Problem 2 *)
let () = assert( (concat_with ";" ["1"; "2"]) = "1;2" )
let () = assert( (concat_with ";" ["xyz"]) = "xyz" )
let () = assert( (concat_with ";" []) = "" )

(* Problem 3 *)
let () = assert( (quote_string "hello") = "\"hello\"")
let () = assert( (quote_string "") = "\"\"")

(* Problem 4 *)
let () = assert( (string_of_json json_pi) = "3.14159" )
let () = assert( (string_of_json json_hello) = "\"hello\"" )
let () = assert( (string_of_json json_false) = "false" )
let () = assert( (string_of_json json_array) = "[1, \"world\", null]" )
let () = assert(
    (string_of_json json_obj) = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}")

let obj1 = Object [("foo", json_pi)]
let () = assert( (string_of_json obj1) = "{\"foo\" : 3.14159}" )

let obj2 = Object [("foo", json_hello); ("bar", Object [("baz", String "bazbaz"); ("array", json_array)])]
let () = assert(
    (string_of_json obj2) = "{\"foo\" : \"hello\", \"bar\" : {\"baz\" : \"bazbaz\", \"array\" : [1, \"world\", null]}}")

(* Problem 5 *)
let () = assert( (take 0 [4; 5; 6; 7]) = [] )
let () = assert( (take 1 [4; 5; 6; 7]) = [4] )
let () = assert( (take 2 [4; 5; 6; 7]) = [4; 5] )
let () = assert( (take 3 [4; 5; 6; 7]) = [4; 5; 6] )
let () = assert( (take 4 [4; 5; 6; 7]) = [4; 5; 6; 7] )
let () = assert( (take 2 ["No.1"; "No.2"; "No.3"; "No.4"]) = ["No.1"; "No.2"] )

(* Problem 6 *)
let () = assert( (firsts [(1,2); (3,4)]) = [1; 3])

(* Problem 7 *)
let () =
    let l = [(1, 2); (3, 4); (5, 6)] in
    assert( (firsts (take 2 l)) = (take 2 (firsts l)))

(* Problem 8 *)

let () = assert( (assoc "foo" [("bar", 17);("foo", 19)]) = Some 19)
let () = assert( (assoc 12 [(12, 17);(13, 19)]) = Some 17)
let () = assert( (assoc 999 [(12, 17);(13, 19)]) = None)

(* Problem 9 *)
let () = assert( (dot (json_obj, "ok")) = Some True)
let () = assert( (dot (json_obj, "bar")) = Some json_array)
let () = assert( (dot (json_obj, "foo")) = Some json_pi)
let () = assert( (dot (json_pi, "foo")) = None)
let () = assert( (dot (json_hello, "hello")) = None)

(* Problem 10 *)
let () = assert( (dots (Object [("f", Object [("g", String "gotcha")])]) ["f"; "g"]) = Some (String "gotcha"))
let () = assert( (dots (Object [("f", Object [("g", String "gotcha")])]) ["f"; "gw"]) = None)
let () = assert( (dots (Object [("f", Object [("g", String "gotcha")])]) ["a"; "b"]) = None)

(* Problem 11 *)
let () = assert( (one_fields json_pi) = [])
let () = assert( (one_fields json_obj) = List.rev ["foo";"bar";"ok"])

(* Problem 12 *)
let () = assert((no_repeats []) = true)
let () = assert((no_repeats ["foo";"bar";"foo"]) = false)
let () = assert((no_repeats ["foo";"bar";"baz"]) = true)

(* Problem 13 *)
let nest1 = Array [Object [];
                   Object[("a",True);
                          ("b",Object[("foo",True);
                                      ("foo",True)]);
                          ("c",True)];
                   Object []]
let () = assert((recursive_no_field_repeats nest1) = false)

let nest2 = Array [Object [];
                   Object[("a",True);
                          ("b",Object[("foo",True);
                                      ("bar",True)]);
                          ("c",True)];
                   Object []]
let () = assert((recursive_no_field_repeats nest2) = true)

let nest3 = Array [Object [];
                   Object[("a",True);
                          ("b",Object[("foo",True);
                                      ("bar",True)]);
                          ("foo",True)];
                   Object []]
let () = assert((recursive_no_field_repeats nest3) = false)

let nest4 = Array [Object []; Object []]
let () = assert((recursive_no_field_repeats nest4) = true)

let nest5 = json_pi
let () = assert((recursive_no_field_repeats nest5) = true)

(* Problem 14 *)
(* Any order is allowed by the specification, so it's ok to fail this test because of a different order. 
   You can edit this test to match your implementation's order. *)
let () = assert(
    (count_occurrences (["a"; "a"; "b"], (Failure ""))) = [("a", 2);("b", 1)])
let () = assert(
    (count_occurrences ([], (Failure ""))) = [])

(* test to see that an exception is thrown when the input list is not sorted *)
let () = assert(
    try (count_occurrences (["b"; "a"; "b"], (Failure ""))) = [("dummy", 123)]
    with Failure _ -> true
)

(* Problem 15 *)
let () = assert(
    (string_values_for_access_path
        (["x"; "y"],
        [Object [("a", True);("x", Object [("y", String "foo")])]; Object [("x", Object [("y", String "bar")]); ("b", True)]]))
    = ["foo";"bar"] )

let () = assert(
    (string_values_for_access_path (["x"; "y"], [ json_array ])) = [])

let () = assert(
    (string_values_for_access_path
        (["x"; "y"],
        [Object [("a", True);("x", Object [("y", String "foo")])]; Object [("x", Object [("y", False)]); ("b", True)]]))
    = ["foo"] )

(* Problem 16 *)
let () = assert(
    filter_access_path_value(["x"; "y"],
                             "foo",
                             [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
                              Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
                              Object [("x", String "a")];
                              Object []])
             = 
             [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
              Object [("x", Object [("y", String "foo")]); ("z", String "baz")]] )

let () = assert(
    filter_access_path_value(["x"],
                              "b",
                              [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
                               Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
                               Object [("x", String "a")];
                               Object []])
             = [] )

(* Problem 17 *)
let pgascse =
  { latitude = 47.653221;
    longitude = -122.305708 }

let () =
    let pgascse1 = { latitude = 47.653221; longitude = -122.305708 } in
    assert( (in_rect u_district pgascse1) = true)

let () =
    let pgascse2 = { latitude = 40.653221; longitude = -122.305708 } in
    assert( (in_rect u_district pgascse2) = false)

let () =
    let pgascse3 = { latitude = 47.653221; longitude = 122.305708 } in
    assert( (in_rect u_district pgascse3) = false)

(* Problem 18 *)
let json_pgascse = Object [("latitude", Num 47.653221); ("longitude", Num (-122.305708))]

let () = 
    let json_pgascse1 =
        Object [("latitude", Num 47.653221); ("longitude", Num (-122.305708))] in
    let pgascse1 = { latitude = 47.653221; longitude = -122.305708 } in
    assert( (point_of_json json_pgascse1) = (Some pgascse1) )

let () = 
    let json_pgascse2 =
        Object [("hello", Num 47.653221); ("world", Num (-122.305708))] in
    assert( (point_of_json json_pgascse2) = None )

let () = 
    let json_pgascse3 =
        Object [("latitude", String "hahaha"); ("longitude", Num (-122.305708))] in
    assert( (point_of_json json_pgascse3) = None )

let () = 
    let json_pgascse4 = Object [] in
    assert( (point_of_json json_pgascse4) = None )

(* Problem 19 *)

let () = assert(
    filter_access_path_in_rect(["x"; "y"], u_district, [Object [("x", Object [("y", json_pgascse)])]; Object []])
             = [Object [("x", Object [("y", json_pgascse)])]] )

let () = assert(
    filter_access_path_in_rect(["x"; "z"], u_district, [Object [("x", Object [("y", json_pgascse)])]; Object []])
             = [])

let () = assert(
    let json_pgascse1 = Object [("latitude", Num 47.653221); ("longitude", Num (122.305708))] in
    filter_access_path_in_rect(["x"; "z"], u_district, [Object [("x", Object [("y", json_pgascse1)])]; Object []])
             = [])

(* Problem 20 *)
(* None *)

(* Problem 21 *)
 let () = assert(
    route_histogram =
        [("E Line", 12); ("C Line", 11); ("7", 10); ("62", 10); ("A Line", 9); ("40", 9); ("150", 9); ("F Line", 8); ("41", 8); ("120", 8); ("106", 8); ("D Line", 7); ("550", 7); ("45", 7); ("B Line", 6); ("49", 6); ("48", 6); ("44", 6); ("36", 6); ("128", 6); ("8", 5); ("70", 5); ("65", 5); ("522", 5); ("5", 5); ("255", 5); ("250", 5); ("245", 5); ("165", 5); ("160", 5); ("First Hill Streetcar", 4); ("67", 4); ("60", 4); ("545", 4); ("50", 4); ("271", 4); ("26", 4); ("221", 4); ("21", 4); ("2", 4); ("181", 4); ("132", 4); ("131", 4); ("101", 4); ("75", 3); ("554", 3); ("348", 3); ("346", 3); ("32", 3); ("28", 3); ("27", 3); ("240", 3); ("24", 3); ("239", 3); ("225", 3); ("183", 3); ("168", 3); ("13", 3); ("124", 3); ("107", 3); ("73", 2); ("542", 2); ("4", 2); ("372", 2); ("347", 2); ("345", 2); ("331", 2); ("33", 2); ("3", 2); ("231", 2); ("161", 2); ("14", 2); ("12", 2); ("11", 2); ("105", 2); ("10", 2); ("1", 2); ("241", 1); ("230", 1); ("226", 1); ("187", 1); ("184", 1); ("182", 1); ("156", 1); ("148", 1); ("125", 1); ("118", 1)])



(* Problem 22 *)
let () = assert(
    top_three_routes = ["E Line"; "C Line"; "7"])

(* Problem 23 *)
let () = assert(
    ud_route_histogram = [("49", 2); ("48", 2); ("44", 2); ("70", 1); ("67", 1); ("65", 1); ("45", 1); ("255", 1)])

(* Problem 24 *)
let () = assert(
    top_three_ud_routes = ["49"; "48"; "44"])

(* Problem 25 *)
let () = assert( (List.length all_fourty_fours) = 6 )

(* ---------Challenge Problems------------- *)

(* Problem C1 *)

let () = assert(
    consume_string_literal (char_list_of_string "\"foo\" : true") = ("foo", [' '; ':'; ' '; 't'; 'r'; 'u'; 'e']))

(*

(* Commented out tests for challenge problems *)


let testC2 = consume_keyword (char_list_of_string "false foo") = (FalseTok, [' '; 'f'; 'o'; 'o'])

(* Test for tokenize_char_list. You'll want more. *)
let testC3 = tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [true, false] }")
             = [LBrace; StringLit "foo"; Colon; NumLit "3.14"; Comma; StringLit "bar";
                Colon; LBracket; TrueTok; Comma; FalseTok; RBracket; RBrace]

let testC5 = parse_string ([StringLit "foo"; FalseTok]) = ("foo", [FalseTok])

let testC6 = expect (Colon, [Colon; FalseTok]) = [FalseTok]

(* Test for parse_json. You'll want more. *)
let testC10 = parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")
              = (Object [("foo", Null); ("bar", Array [True; False])], [])
*)

let () = print_endline("HW2 test OK")
