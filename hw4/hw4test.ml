#mod_use "hw4.ml";;
open Hw4

let fail_if_not_equal name a b =
  if a <> b
  then failwith ("test failed: " ^ name)

let () = fail_if_not_equal "test1" 6 (C1.sum (IntSet1.insert 1 (IntSet1.insert 2 (IntSet1.insert 3 IntSet1.empty))))
let () = fail_if_not_equal "test2" [1] (C1.to_list (IntSet1.insert 1 IntSet1.empty))
let big_set =
  IntSet1.empty |> IntSet1.insert 4
                |> IntSet1.insert 2
                |> IntSet1.insert 3
                |> IntSet1.insert 1
                |> IntSet1.remove 4
                |> IntSet1.insert 3
let () = fail_if_not_equal "test3" "{1, 2, 3}" (IntSet1.to_string big_set)
