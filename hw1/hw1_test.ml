#use "hw1.ml"

(* Problem1 *)
let () = assert(is_older((1999, (1, 1)), (1998, (12, 31))) = true)
let () = assert(is_older((1999, (1, 2)), (1999, (1, 1))) = true)
let () = assert(is_older((1999, (2, 1)), (1999, (1, 12))) = true)
let () = assert(is_older((1999, (1, 2)), (1999, (1, 12))) = false)
let () = assert(is_older((1999, (3, 2)), (1999, (3, 3))) = false)
let () = assert(is_older((-2, (1, 2)), (-1, (1, 12))) = false)

(* Problem2 *)
let () = assert(number_in_month(
    [(1999, (1, 1)); (2000, (2, 1)); (2001, (1, 31))], 1) = 2)
let () = assert(number_in_month(
    [(1234, (12, 23)); (2000, (112, 1)); (2001, (12, 31))], 12) = 2)

(* Problem3 *)
let () = assert(number_in_months(
    [(1234, (12, 23)); (2000, (112, 1)); (2001, (12, 31))], [12]) = 2)
let () = assert(number_in_months(
        [(1234, (12, 23));
         (2000, (112, 1));
         (2001, (12, 31));
         (2022, (1, 23))], [12; 1]) = 3)
let () = assert(number_in_months([], [12; 1]) = 0)

(* Problem4 *)
let () = assert(dates_in_month(
        [(1234, (12, 23));
         (2000, (112, 1));
         (2001, (12, 31));
         (2022, (1, 23))], 13) = [])

let () = assert(dates_in_month(
        [(1234, (12, 23));
         (2000, (112, 1));
         (2001, (12, 31));
         (2022, (1, 23))], 12) = [(1234, (12, 23)); (2001, (12, 31))])

(* Problem5 *)
let () = assert(dates_in_months(
        [(1234, (12, 23));
         (2000, (112, 1));
         (2001, (12, 31));
         (2022, (1, 23))], [13]) = [])
let () = assert(dates_in_months(
        [(1234, (12, 23));
         (2000, (112, 1));
         (2001, (11, 31));
         (2022, (1, 23))], [12; 11]) = [(1234, (12, 23)); (2001, (11, 31))])

(* Problem6 *)
let () = assert(get_nth(["foo"], 1) = "foo")
let () = assert(get_nth(["foo"; "bar"; "baz"], 1) = "foo")
let () = assert(get_nth(["foo"; "bar"; "baz"], 2) = "bar")
let () = assert(get_nth(["foo"; "bar"; "baz"], 3) = "baz")

(* Problem7 *)
let () = assert(string_of_date((2015, (9, 10))) = "September-10-2015")

(* Problem8 *)
let () = assert(number_before_reaching_sum(1, [2; 3; 4]) = 0)
let () = assert(number_before_reaching_sum(2, [2; 3; 4]) = 0)
let () = assert(number_before_reaching_sum(3, [2; 3; 4]) = 1)
let () = assert(number_before_reaching_sum(9, [2; 3; 4]) = 2)
let () = assert(number_before_reaching_sum(1, [2]) = 0)

(* Problem9 *)
let () = assert(what_month(1) = 1)
let () = assert(what_month(32) = 2)
let () = assert(what_month(365) = 12)

(* Problem10 *)
let () = assert(month_range(100, 2) = [])
let () = assert(month_range(1, 2) = [1; 1])
let () = assert(month_range(364, 365) = [12; 12])
let () = assert(month_range(31, 32) = [1; 2])

(* Problem11 *)
let () = assert(oldest([(1999, (1, 1))]) = Some (1999, (1, 1)))
let () = assert(oldest([(1999, (1, 1)); (0, (2, 3)); (0, (1, 1))]) = Some (1999, (1, 1)))
let () = assert(oldest([]) = None)

(* Problem12 *)
let () = assert(cumulative_sum([1; 2; 3]) = [1; 3; 6])
let () = assert(cumulative_sum([]) = [])
let () = assert(cumulative_sum([2]) = [2])

(* Problem13 *)
let () = assert(number_in_months_challenge(
    [(1234, (12, 23)); (2000, (112, 1)); (2001, (12, 31))], [12; 12]) = 2)
let () = assert(number_in_months_challenge(
        [(1234, (12, 23));
         (2000, (112, 1));
         (2001, (12, 31));
         (2022, (1, 23))], [12; 1; 1]) = 3)
let () = assert(number_in_months_challenge([], [12; 1]) = 0)

(* Problem14 *)
let () = assert(reasonable_date((-1, (1, 1))) = false)
let () = assert(reasonable_date((1, (-1, 1))) = false)
let () = assert(reasonable_date((1, (1, -1))) = false)
let () = assert(reasonable_date((1, (1, 0))) = false)
let () = assert(reasonable_date((1, (0, 1))) = false)
let () = assert(reasonable_date((0, (1, 1))) = false)
let () = assert(reasonable_date((1, (1, 1))) = true)
let () = assert(reasonable_date((1, (1, 32))) = false)
let () = assert(reasonable_date((1, (2, 29))) = false)
let () = assert(reasonable_date((1, (13, 29))) = false)
let () = assert(reasonable_date((4, (2, 29))) = true)
let () = assert(reasonable_date((100, (2, 29))) = false)
let () = assert(reasonable_date((400, (2, 29))) = true)
let () = assert(reasonable_date((2000, (2, 29))) = true)
let () = assert(reasonable_date((2000, (12, 29))) = true)
let () = assert(reasonable_date((2000, (12, 31))) = true)
let () = assert(reasonable_date((2000, (12, 33))) = false)

let () = print_endline("HW1 test OK")
