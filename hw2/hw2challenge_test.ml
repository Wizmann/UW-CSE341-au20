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

(* Problem C2 *)
let () = assert(
    consume_keyword (char_list_of_string "false foo") = (FalseTok, [' '; 'f'; 'o'; 'o']))

let () = assert(
    consume_keyword (char_list_of_string "false,foo") = (FalseTok, [','; 'f'; 'o'; 'o']))

let () = assert(
    consume_keyword (char_list_of_string "false ,foo") = (FalseTok, [' '; ','; 'f'; 'o'; 'o']))

let () = assert(
    consume_keyword (char_list_of_string "null foo") = (NullTok, [' '; 'f'; 'o'; 'o']))

let () =
    try
        assert(consume_keyword (char_list_of_string "hello foo") = (NullTok, [' '; 'f'; 'o'; 'o']))
    with
    | LexicalError(msg) ->
        assert(msg = "Lexical error: Expecting keyword, character list does not start with a keyword.")
    | _ -> assert(false)

(* Problem C3 *)
let () = assert(
    tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [true, false] }")
             = [LBrace; StringLit "foo"; Colon; NumLit "3.14"; Comma; StringLit "bar";
                Colon; LBracket; TrueTok; Comma; FalseTok; RBracket; RBrace])

let () = assert(
    tokenize_char_list (char_list_of_string "") = [])

let () = assert(
    tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [\"txxx\", false] }")
             = [LBrace; StringLit "foo"; Colon; NumLit "3.14"; Comma; StringLit "bar";
                Colon; LBracket; StringLit "txxx"; Comma; FalseTok; RBracket; RBrace])

let () = assert(
    tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [\"txxx\", \"hello world\", \"123\"] }")
             = [LBrace; StringLit "foo"; Colon; NumLit "3.14"; Comma; StringLit "bar";
                Colon; LBracket; StringLit "txxx"; Comma; StringLit "hello world"; Comma; StringLit "123"; RBracket; RBrace])

let () =
    try
        assert(tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [txxx, false] }") = [])
    with
    | LexicalError(msg) -> assert(true)
    | _ -> assert(false)

let () =
    try
        assert(tokenize_char_list (char_list_of_string "hello foo") = [])
    with
    | LexicalError(msg) -> assert(true)
    | _ -> assert(false)

(* Problem C5 *)
let () = assert(parse_string ([StringLit "foo"; FalseTok]) = ("foo", [FalseTok]))

let () =
    try
        assert(parse_string ([TrueTok; FalseTok]) = ("foo", [FalseTok]))
    with
    | SyntaxError(msg) -> assert(true)
    | _ -> assert(false)

let () =
    try
        assert(parse_string ([]) = ("foo", [FalseTok]))
    with
    | SyntaxError(msg) -> assert(true)
    | _ -> assert(false)

(* Problem C6 *)
let () = assert(expect (Colon, [Colon; FalseTok]) = [FalseTok])

(* Problem C10 *)
let () = assert( parse_json (tokenize "{ \"foo\" : null, \"bar\" : { \"t\": true, \"f\"   : false   } }")
              = (Object [("foo", Null); ("bar", Object [("t", True); ("f", False)])], []))

let () = assert( parse_json (tokenize "{ \"foo\" : null }")
              = (Object [("foo", Null)], []))

let () = assert( parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")
              = (Object [("foo", Null); ("bar", Array [True; False])], []))

(* Problem C11 : TODO *)
(* Problem C12 : TODO *)

(* Test suite : https://github.com/nst/JSONTestSuite *)

let () = print_endline("HW2 challenge test OK")

