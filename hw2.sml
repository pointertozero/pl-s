(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* string * string list -> string list option
removes given element from the list and returns list as SOME option
if element not found then returns NONE *)
fun all_except_option (s, slist) =
    case slist of
	[] => NONE
      | x :: xs => if same_string(x, s)
		   then SOME xs
		   else case all_except_option(s, xs) of
			    NONE => NONE
			  | SOME ans_tl => SOME (x :: ans_tl)


fun get_substitutions1 (sll, s) =
    case sll of
	[] => []
      | x :: xs => (case all_except_option(s, x) of
			NONE => []
		      | SOME sl => sl)
		   @ get_substitutions1(xs, s)


(* string list list * string -> string list *)
fun get_substitutions2 (sll, s) =
    let fun aux (sll, acc) =
	    case sll of
		[] => acc
	      | x :: xs => aux(xs, acc @ (case all_except_option(s, x) of
					      NONE => []
					    | SOME sl => sl))
    in
	aux(sll, [])
    end


fun similar_names (subs, {first=f, middle=m, last=l}) =
    let fun helper (sl) =
	    case sl of
		[] => []
	      | x :: xs => {first=x, middle=m, last=l} :: helper(xs)
    in
	helper(f :: get_substitutions2(subs, f))
    end


	
(* put your solutions for problem 2 here *)


fun card_color c =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red 


fun card_value c =
    case c of
	(_, Num n) => n
      | (_, Ace) => 11
      | _ => 10

(* Card list * Card * exn -> Card list
throws passed exception if card not in the list *)
fun remove_card (cs, c, ex) =
    case cs of
	[] => raise ex
      | x :: xs => if x = c
		   then xs
		   else x :: remove_card(xs, c, ex)



					
fun all_same_color cs =
    case cs of
	[] => true
      | _ :: [] => true
      | a :: b :: c => (card_color a = card_color b) andalso all_same_color(b :: c)


fun sum_cards cs =
    let fun aux (cs, acc) =
	    case cs of
		[] => acc
	      | x :: xs => aux(xs, acc + card_value x)
    in
	aux(cs, 0)
    end

(* card list * int -> int *)
fun score (held_cards, goal) =
    let
	val sum = sum_cards(held_cards)
	val prel_score = if sum > goal then 3 * (sum - goal)
			 else (goal - sum)
    in
	if all_same_color(held_cards)
	then prel_score div 2
	else prel_score
    end


fun officiate (card_list, move_list, goal) =
    (* hand is Card list *)
    let fun run (card_list, move_list, hand) =
	    case (card_list, move_list) of
		(_, []) => score(hand, goal)
	      | (_, (Discard c) :: xs) => run(card_list,
					      xs,
					      remove_card(hand, c, IllegalMove))
	      | ([], Draw :: xs) => score(hand, goal)
	      | (c :: cs, Draw :: xs) => if sum_cards(c :: hand) > goal
					 then score(c :: hand, goal)
					 else run(cs, xs, c :: hand)
    in
	run(card_list, move_list, [])
    end




(* CHALLENGE
===========================	 *)

fun aces_count cards =
    case cards of
	[] => 0
      | x :: xs => (if card_value x = 11 then 1 else 0)
		   + aces_count xs

fun lowest_sum_cards cards = sum_cards cards - 10 * aces_count cards

				
fun score_challenge (held_cards, goal) =
    let
	(* Initially we set all aces value to 1, we calculate the score
	   then we set one ace to 11, rest to 1, we calculate the score, ...
	   finally we set value of all aces to 11, calculate the score and get the minimum of these scores *)
	fun helper (number_of_aces_reduced) =
	    if number_of_aces_reduced = 0
	    then score(held_cards, goal)
	    else let
		val sum = sum_cards(held_cards) - 10 * number_of_aces_reduced
		val prel_score = if sum > goal then 3 * (sum - goal)
				 else (goal - sum)
	    in
		Int.min(if all_same_color(held_cards)
			then prel_score div 2
			else prel_score,
			helper(number_of_aces_reduced - 1))
	    end
    in
	helper(aces_count(held_cards))
    end
		
		


(* score changed to score_challenge
sum_cards changed to lowest_sum_cards *)
fun officiate_challenge (card_list, move_list, goal) =
    (* hand is Card list *)
    let fun run (card_list, move_list, hand) =
	    case (card_list, move_list) of
		(_, []) => score_challenge(hand, goal)
	      | (_, (Discard c) :: xs) => run(card_list,
					      xs,
					      remove_card(hand, c, IllegalMove))
	      | ([], Draw :: xs) => score_challenge(hand, goal)
	      | (c :: cs, Draw :: xs) => if lowest_sum_cards(c :: hand) > goal
					 then score_challenge(c :: hand, goal)
					 else run(cs, xs, c :: hand)
    in
	run(card_list, move_list, [])
    end






fun careful_player (card_list, goal) =
    let
	(* card list * card -> card option
	 returns SOME card that can be discarded to draw c, value of card is smallest out of all possible
	 if there is no such card to be found returns NONE *)
	fun can_discard_draw (cs0, c) =
	    let
		val sum = sum_cards cs0
				    
		fun card_min (c1, c2) =
		    if card_value c1 < card_value c2
		    then c1
		    else c2
			     
		fun helper cs =
		    case cs of
			[] => NONE
		      | x :: xs => if sum + card_value c - card_value x <= goal                  (* we could return immediately if equality, we don't bother *)
				   then case helper xs of
					    NONE => SOME x
					  | SOME x2 => SOME (card_min (x, x2))
				   else helper xs
					       
	    in
		helper cs0
	    end
				       
	fun generate_movelist (card_list, hand_list) =
	    let val sum = sum_cards(hand_list)
	    in
		if score(hand_list, goal) = 0
		then []
		else if goal - sum > 10
		then case card_list of
			 [] => [Draw] 
		       | x :: xs => Draw :: generate_movelist(xs, x :: hand_list)
		else case card_list of           (* goal - sum <= 10 *)
			                         (* we don't have to draw,
			                            the only thing we need to check is whether we can discard a card and then draw a card to reach score = 0 *)
			 [] => []
		       | x :: xs => case can_discard_draw(hand_list, x) of
					NONE => []
				      | SOME c => (Discard c) :: Draw :: generate_movelist(xs, x :: remove_card(hand_list, c, IllegalMove))

	    end
    in
	generate_movelist(card_list, [])
    end
