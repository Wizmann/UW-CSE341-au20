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


let () = print_endline("HW3 test OK")
