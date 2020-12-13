#mod_use "hw4.ml";;
open Hw4

let fail_if_equal name a b =
  if a = b
  then failwith ("test failed: " ^ name)

let fail_if_not_equal name a b =
  if a <> b
  then failwith ("test failed: " ^ name)

let () = fail_if_not_equal "test_problem_1" 6 (C1.sum (IntSet1.insert 1 (IntSet1.insert 2 (IntSet1.insert 3 IntSet1.empty))))
let () = fail_if_not_equal "test_problem_3_1" [1] (C1.to_list (IntSet1.insert 1 IntSet1.empty))
let () = fail_if_not_equal "test_problem_3_2" [1; 3; 2; 2; 0]
    (IntSet1.empty |> IntSet1.insert 1
                   |> IntSet1.insert 3
                   |> IntSet1.insert 2
                   |> IntSet1.insert 2
                   |> IntSet1.insert 0
                   |> C1.to_list)
let big_set =
  IntSet1.empty |> IntSet1.insert 4
                |> IntSet1.insert 2
                |> IntSet1.insert 3
                |> IntSet1.insert 1
                |> IntSet1.remove 4
                |> IntSet1.insert 3
let () = fail_if_not_equal "test_problem5_1" "{1, 2, 3}" (IntSet1.to_string big_set)

let () = fail_if_not_equal "test_problem6_1" [1; 2; 3]
    (IntSet2.empty |> IntSet2.insert 1
                   |> IntSet2.insert 3
                   |> IntSet2.insert 2
                   |> C2.to_list)

let () = fail_if_not_equal "test_problem6_2" [1; 2; 3]
    (IntSet2.empty |> IntSet2.insert 1
                   |> IntSet2.insert 4
                   |> IntSet2.insert 3
                   |> IntSet2.insert 2
                   |> IntSet2.remove 4
                   |> C2.to_list)

let () = fail_if_not_equal "test_problem6_3" "{1, 2, 3}"
    (IntSet2.empty |> IntSet2.insert 1
                   |> IntSet2.insert 3
                   |> IntSet2.insert 2
                   |> IntSet2.to_string)


let result_set1 = (IntSet1.empty |> IntSet1.insert 1
                                 |> IntSet1.insert 4
                                 |> IntSet1.insert 3
                                 |> IntSet1.insert 2
                                 |> IntSet1.remove 4
                                 |> IntSet1.fold (fun a b -> a * 10 + b) 0)

let result_set2 = (IntSet2.empty |> IntSet2.insert 1
                                 |> IntSet2.insert 4
                                 |> IntSet2.insert 3
                                 |> IntSet2.insert 2
                                 |> IntSet2.remove 4
                                 |> IntSet2.fold (fun a b -> a * 10 + b) 0)

let () = fail_if_equal "test_problem7_1" result_set1 result_set2

let () = fail_if_not_equal "test_problem9_1" [1; 2; 3]
    (IntSet3.empty |> IntSet3.insert 1
                   |> IntSet3.insert 3
                   |> IntSet3.insert 2
                   |> C3.to_list)

let () = fail_if_not_equal "test_problem9_2" [1; 2; 3; 4]
    (IntSet3.empty |> IntSet3.insert 1
                   |> IntSet3.insert 4
                   |> IntSet3.insert 3
                   |> IntSet3.insert 2
                   |> IntSet3.insert 4
                   |> C3.to_list)

let () = fail_if_not_equal "test_problem9_3" [1; 2; 3]
    (IntSet3.empty |> IntSet3.insert 1
                   |> IntSet3.insert 4
                   |> IntSet3.insert 3
                   |> IntSet3.insert 2
                   |> IntSet3.remove 4
                   |> C3.to_list)

let () = fail_if_not_equal "test_problem9_4" "{1, 2, 3}"
    (IntSet3.empty |> IntSet3.insert 1
                   |> IntSet3.insert 3
                   |> IntSet3.insert 2
                   |> IntSet3.to_string)


let () = print_endline("HW4 test OK")
