open Printf

(* 
 * Problem1
 * Write a function is_older that takes two dates and evaluates to true or false. 
 * It evaluates to true if the first argument is a date that comes before the second argument.
 * (If the two dates are the same, the result is false.) 
*)

let is_older((d1: int * (int * int)), (d2: int * (int * int))) =
    if fst(d1) > fst(d2) then
        true
    else if fst(snd(d1)) > fst(snd(d2)) then
        true
    else if snd(snd(d1)) > snd(snd(d2)) then
        true
    else
        false
;;


(*
 * Problem2
 * Write a function number_in_month that takes a list of dates and a month
 * (i.e., an int) and returns how many dates in the list are in the given month.
*)

let rec number_in_month((ds: ((int * (int * int)) list)), (month: int)) =
    match ds with
    | [] -> 0
    | (yy, (mm, dd)) :: tl -> (if mm = month then 1 else 0) + number_in_month(tl, month)
;;


(*
 * Problem3
 * Write a function number_in_months that takes a list of dates
 * and a list of months (i.e., an int list) and returns the number
 * of dates in the list of dates that are in any of the months in
 * the list of months. 
 * Assume the list of months has no number repeated.
 * Hint: Use your answer to the previous problem.
*)

let rec number_in_months((ds: ((int * (int * int)) list)),
                         (months: int list)) =
    match months with
    | [] -> 0
    | (mm) :: tl -> number_in_month(ds, mm) + number_in_months(ds, tl)
;;

(*
 * Problem4
 * Write a function dates_in_month that takes a list of dates
 * and a month (i.e., an int) and returns a list holding the dates
 * from the argument list of dates that are in the month.
 * The returned list should contain dates in the order they were
 * originally given.
*)

let rec dates_in_month((ds: ((int * (int * int)) list)), (month: int)) =
    match ds with
    | [] -> []
    | (yy, (mm, dd)) :: tl -> 
            (if mm == month then [(yy, (mm, dd))] else [])
            @ dates_in_month(tl, month)
;;

(*
 * Problem5
 * Write a function dates_in_months that takes a list of dates and
 * a list of months (i.e., an int list) and returns a list holding
 * the dates from the argument list of dates that are in any of the
 * months in the list of months. Assume the list of months has no number
 * repeated.
 * Hint: Use your answer to the previous problem and OCaml's list-append operator (@).
*)
let rec dates_in_months((ds: ((int * (int * int)) list)), (months: int list)) =
    match months with
    | [] -> []
    | mm :: tl -> dates_in_month(ds, mm) @ dates_in_months(ds, tl)
;;

(*
 * Problem6
 * Write a function get_nth that takes a list of strings and a positive int n
 * and returns the nth element of the list where the head of the list is 1st.
 * Do not worry about the case where the list has too few elements:
 * your function may apply List.hd or List.tl to the empty list in this case,
 * which is okay.
*)
let rec get_nth(items, n) =
    match items with
    | [] -> assert false
    | hd :: tl -> if n == 1 then hd
                  else if n > 1 then get_nth(tl, n - 1)
                  else assert false
;;

(*
 * Problem7
 * Write a function string_of_date that takes a date and returns a string of the
 * form September-10-2015 (for example). Use the operator ^ for concatenating
 * strings and the library function string_of_int for converting an int
 * to a string.  
 * For producing the month part, do not use a bunch of conditionals. Instead,
 * use a list holding 12 strings and your answer to the previous problem.
 * For consistency, use hyphens exactly as in the example and use English month
 * names: January, February, March, April, May, June, July, August, September,
 * October, November, December.
*)

let string_of_date(date: int * (int * int)) =
    let months_str = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"] in
    match date with
    | (yy, (mm, dd)) -> get_nth(months_str, mm) ^ "-" ^ Stdlib.string_of_int(dd) ^ "-" ^ Stdlib.string_of_int(yy)
;;

(*
 * Problem8
 * Write a function number_before_reaching_sum that takes an int called sum,
 * which you can assume is positive, and an int list, which you can assume
 * contains all positive numbers, and returns an int. 
 * You should return an int n such that the first n elements of the list add
 * to less than sum, but the first n+1 elements of the list add to sum or more.
 * Assume the entire list sums to more than the passed in value;
 * it is okay for an exception to occur if this is not the case.
*)

let rec number_before_reaching_sum((sum: int), (nums: int list)) = 
    match nums with
    | [] -> assert false
    | num :: tl -> 
            if (sum > num) then
                1 + number_before_reaching_sum(sum - num, tl)
            else
                0
;;

(*
 * Problem9
 * Write a function what_month that takes a day of year (i.e., an int between 1 and 365)
 * and returns what month that day is in (1 for January, 2 for February, etc.).
 * Use a list holding 12 integers and your answer to the previous problem.
*)

let what_month(day: int) =
    let months = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
    number_before_reaching_sum(day, months) + 1
;;

(*
 * Problem10
 * Write a function month_range that takes two days of the year day1 and day2 and
 * returns an int list [m1;m2;...;mn] where m1 is the month of day1, m2 is the
 * month of day1+1, . . . , and mn is the month of day day2. Note the result will
 * have length day2 - day1 + 1 or length 0 if day1>day2.
*)
let rec month_range((day1: int), (day2: int)) =
    if day1 > day2 then
        []
    else
        [ what_month(day1) ] @ month_range(day1 + 1, day2)
;;

(*
 * Problem11
 * Write a function oldest that takes a list of dates and evaluates to an
 * (int*(int*int)) option. It evaluates to None if the list has no dates else
 * Some d where the date d is the oldest date in the list.
*)
let rec oldest(ds: (int * (int * int)) list) = 
    match ds with
    | [] -> None
    | hd :: tl -> let rem = oldest(tl) in
                  match rem with
                  | Some rem -> if is_older(hd, rem) then Some hd else Some rem
                  | None -> Some hd
;;

(*
 * Problem12
 * Write a function cumulative_sum that takes a list of numbers and returns a
 * list of the partial sums of these numbers. For example, cumulative_sum
 * [12;27;13] = [12;39;52]. Hint: Use a helper function that takes two arguments.
*)
let cumulative_sum(nums: int list) =
    let rec cumulative_sum_helper((sum: int), (rems: int list)) =
        match rems with
        | [] -> []
        | rem :: tl -> [ sum + rem ] @ cumulative_sum_helper(sum + rem, tl)
    in

    cumulative_sum_helper(0, nums)
;;

(*
 * Problem13
 * Challenge Problem: Write functions number_in_months_challenge and
 * dates_in_months_challenge that are like your solutions to problems 3 and 5
 * except having a month in the second argument multiple times has no more effect
 * than having it once. (Hint: Remove duplicates, then use previous work.)
*)

module SI = Set.Make(Int)

let dedup(nums: int list) =
    let rec dedup_helper((nums: int list), (set: SI.t)) =
        match nums with
        | [] -> []
        | hd :: tl -> (if SI.mem hd set then [] else [ hd ]) @ dedup_helper(tl, SI.add hd set)
    in

    dedup_helper(nums, SI.empty)
;;

let number_in_months_challenge((ds: ((int * (int * int)) list)), (months: int list)) =
    let deduped_months = dedup(months) in
    number_in_months(ds, deduped_months)
;;

let rec dates_in_months_challenge((ds: ((int * (int * int)) list)), (months: int list)) =
    let deduped_months = dedup(months) in
    dates_in_months(ds, deduped_months)
;;

(*
 * Problem14
 * Challenge Problem: Write a function reasonable_date that takes a date and
 * determines if it describes a real date in the common era. A "real date" has
 * a positive year (year 0 did not exist), a month between 1 and 12, and a day
 * appropriate for the month. Solutions should properly handle leap years.
 * Leap years are years that are either divisible by 400 or divisible by 4 but
 * not divisible by 100.
 * (Do not worry about days possibly lost in the conversion to the Gregorian
 * calendar in the Late 1500s.)
*)

let reasonable_date(date: int * (int * int)) =
    let is_leap(year: int) =
        (year mod 4 = 0) && (year mod 100 != 0) || (year mod 400 = 0)
    in

    let months = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in

    match date with
    | (yy, (mm, dd)) ->
            if yy <= 0 then false
            else if mm <= 0 || mm > 12 then false
            else
                let day_of_month = get_nth(months, mm) + (if mm == 2 && is_leap(yy) then 1 else 0) in
                1 <= dd && dd <= day_of_month
;;
