#use "hw2challenge.ml";;

(* Problem C1 *)

let () = assert(
    consume_string_literal (char_list_of_string "\"foo\" : true") = ("foo", [' '; ':'; ' '; 't'; 'r'; 'u'; 'e']))

let () = assert(
    consume_string_literal (char_list_of_string "\"123\"") = ("123", []))

let () = 
    try
        assert(consume_string_literal (char_list_of_string "123") = ("always false", []))
    with 
    | LexicalError(msg) ->
        assert(msg = "Lexical error: Expecting string literal, no string literal at the beginning of the given character list.")
    | _ -> assert(false)

let () = 
    try
        assert(consume_string_literal (char_list_of_string "\"123") = ("always false", []))
    with 
    | LexicalError(msg) ->
        assert(msg = "Lexical error: Expecting string literal, the list has no closing double quote.")
    | _ -> assert(false)

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

let () = print_endline("HW2 challenge test OK")

