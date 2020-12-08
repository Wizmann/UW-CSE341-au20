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


let () = print_endline("HW3 test OK")
