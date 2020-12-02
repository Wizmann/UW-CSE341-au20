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
let () = assert( (dot json_obj "ok") = Some True)
let () = assert( (dot json_obj "bar") = Some json_array)
let () = assert( (dot json_obj "foo") = Some json_pi)
let () = assert( (dot json_pi "foo") = None)
let () = assert( (dot json_hello "hello") = None)

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

(*

(* Any order is allowed by the specification, so it's ok to fail this test because of a different order. 
   You can edit this test to match your implementation's order. *)
let test14a = count_occurrences (["a"; "a"; "b"], Failure "") = [("b",1);("a",2)]

(* test to see that an exception is thrown when the input list is not sorted *)
let test14b = try count_occurrences (["b"; "a"; "b"], Failure "") = []
              with Failure _ -> true

let test15 = string_values_for_access_path (["x"; "y"], [Object [("a", True);("x", Object [("y", String "foo")])];
                                                         Object [("x", Object [("y", String "bar")]); ("b", True)]])
            = ["foo";"bar"]

let test16 = filter_access_path_value (["x"; "y"], "foo",
                                       [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
                                        Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
                                        Object [("x", String "a")];
                                        Object []]) 
             = 
             [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
              Object [("x", Object [("y", String "foo")]); ("z", String "baz")]]


let pgascse =
  { latitude = 47.653221;
    longitude = -122.305708 }

let test17 = in_rect (u_district, pgascse)

let json_pgascse = Object [("latitude", Num 47.653221); ("longitude", Num (-122.305708))]

let test18 = point_of_json json_pgascse = Some pgascse

let test19 = filter_access_path_in_rect (["x"; "y"], u_district, [Object [("x", Object [("y", json_pgascse)])]; Object []])
             = [Object [("x", Object [("y", json_pgascse)])]]


(* Commented out tests for challenge problems *)

let testC1 = consume_string_literal (char_list_of_string "\"foo\" : true") = ("foo", [' '; ':'; ' '; 't'; 'r'; 'u'; 'e'])

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
