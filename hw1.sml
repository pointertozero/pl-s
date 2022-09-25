(* checks if d1 is before d2, return true if yes otherwise false *)
fun is_older (d1 : (int * int * int), d2 : (int * int * int)) =
    (#1 d1) < (#1 d2)
    orelse ( (#1 d1) = (#1 d2)
	     andalso (#2 d1) < (#2 d2) )
    orelse ( (#1 d1) = (#1 d2)
	     andalso (#2 d1) = (#2 d2)
	     andalso (#3 d1) < (#3 d2) )

(* returns how many dates are in the given month *)
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
	if (#2 (hd dates)) = month
	then 1 + number_in_month(tl dates, month)
	else     number_in_month(tl dates, month)

(* returns how many dates are in the any month in the given months list
ASSUMES: no duplicates in the list months *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* returns a list of dates which are in the given month *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	if (#2 (hd dates)) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else               dates_in_month(tl dates, month)

(* returns a list of dates which are in any month in the months list
ASSUMES: no duplicates in the months *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* returns the nth element from given string list
ASSUMES: number of elements in the list >= n
n >= 1
*)
fun get_nth (strings : string list, n : int) =
    if n = 1
    then
	hd strings
    else
	get_nth(tl strings, n - 1)



fun date_to_string (date : int * int * int) =
    let
	val months = ["January", "February", "March", "April",
		      "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date)
	^ " "
	^ Int.toString (#3 date)
	^ ", "
	^ Int.toString (#1 date)
    end

	
(*
ASSUMES: sum > 0
nums - only positive numbers
entire list nums sums to more than sum

RETURNS: int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more *)
	
fun number_before_reaching_sum (sum : int, nums : int list) =
    if sum - hd nums <= 0
    then 0
    else
	1 + number_before_reaching_sum(sum - hd nums, tl nums)

(* day: integer from [1, 365]
IGNORES LEAP YEAR
*)
fun what_month (day : int) =
    let
	val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, month_days)
    end


(* d1, d2 int from [1, 365] *)
fun month_range (d1 : int, d2 : int) =
    if d1 > d2
    then []
    else
	what_month(d1) :: month_range(d1 + 1, d2)

				     
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let
	fun oldest_nonempty (dates : (int * int * int) list) =
	    if null (tl dates)
	    then hd dates
	    else let
		val oldest_tl = oldest_nonempty(tl dates)
	    in
		if is_older(hd dates, oldest_tl)
		then hd dates
		else oldest_tl
	    end
    in
	SOME (oldest_nonempty dates)
    end



(* ========================================
    CHALLENGE
*)

	     
(*
int * int list -> Boolean
checks if el is in given list, true if yes otherwise false *)
fun member (el : int, lst : int list) =
    if null lst
    then false
    else (el = hd lst) orelse member(el, tl lst)

	
(* int list -> int list
removes duplicates from list *)
fun remove_duplicates (months : int list) =
    if null months
    then []
    else if member(hd months, tl months)
    then remove_duplicates(tl months)
    else (hd months) :: remove_duplicates(tl months)


fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))


fun dates_in_months_challenge (dates: (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))



		   

(* same as get_nth but int list instead of string list *)
fun get_nth_int (ints : int list, n : int) =
    if n = 1
    then
	hd ints
    else
	get_nth_int(tl ints, n - 1)

	       
(* checks if given year is leap, true if yes otherwise false *)
fun is_leap_year(year : int) =
    year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)

(* Checks if given day is correct; true if yes otherwise false *)
fun good_day (year : int, month : int, day : int) =
    let
	val month_days = [31, if is_leap_year(year) then 29 else 28,
			  31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	0 < day andalso day <= get_nth_int(month_days, month)
    end

fun reasonable_date (date : int * int * int) =
    (#1 date) > 0
    andalso (#2 date) <= 12 andalso 1 <= (#2 date)
    andalso good_day(#1 date, #2 date, #3 date)
	       
