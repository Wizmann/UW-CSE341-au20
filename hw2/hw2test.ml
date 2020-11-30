(* CSE 341, Homework 2 Tests *)

#use "hw2.ml";;

(* You will surely want to add more! *)

let test1 = 
  make_silly_json 2 
  = 
  Array
    [Object [("n", Num 2.); ("b", True)]; 
     Object [("n", Num 1.); ("b", True)]]

let test2 = concat_with (";", ["1"; "2"]) = "1;2"

let test3 = quote_string "hello" = "\"hello\""

let test4 = string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}"

let test5 = take (2, [4; 5; 6; 7]) = [4; 5]

let test6 = firsts [(1,2); (3,4)] = [1; 3]

let test7 = String.length "don't forget to write a comment!" > 0

let test8 = assoc ("foo", [("bar",17);("foo",19)]) = Some 19

let test9 = dot (json_obj, "ok") = Some True

let test10 = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"]) = Some (String "gotcha")

let test11 = one_fields json_obj = List.rev ["foo";"bar";"ok"]

let test12 = not (no_repeats ["foo";"bar";"foo"])

let nest = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True);
                                     ("foo",True)]);
                         ("c",True)];
                  Object []]

let test13 = not (recursive_no_field_repeats nest)

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
