(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)



val only_capitals =
    List.filter (fn i => Char.isUpper(String.sub(i, 0)))

val longest_string1 =
    foldl (fn (i, init) => if String.size i > String.size init
			   then i
			   else init)
	  ""

 
val longest_string2 =
    foldl (fn (i, init) => if String.size i >= String.size init
			   then i
			   else init)
	  ""


fun longest_string_helper f =
    foldl (fn (i, init) => if f(String.size i, String.size init)
			   then i
			   else init)
	  ""



val longest_string3 = longest_string_helper (fn (i, j) => i > j)
val longest_string4 = longest_string_helper (fn (i, j) => i >= j)


val longest_capitalized = longest_string3 o only_capitals

val rev_string = implode o rev o explode

(* (’a -> ’b option) -> ’a list -> ’b  *)
fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | x :: xs => case f x of
		       NONE => first_answer f xs
		     | SOME v => v

(* (’a -> ’b list option) -> ’a list -> ’b list option *)
fun all_answers f lst0 =
    let fun helper (lst, acc) =
	    case lst of
		[] => SOME acc
	      | x :: xs => case f x of
			       NONE => NONE
			     | SOME l => helper(xs, acc @ l)
    in
	helper(lst0, [])
    end


val count_wildcards =
    g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths =
    g (fn () => 1) (fn x => String.size x)

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

      
(* checks if all variable names inside the pattern are unique *)
fun check_pat p =
    let
	fun variable_names p =
	    case p of
		Variable x => [x]
	      | TupleP (p :: ps) => (variable_names p) @ (variable_names (TupleP ps))
	      | ConstructorP(_, p) => variable_names p
	      | _ => []
			 
	fun are_all_unique sl =
	    case sl of
		[] => true
	      | x :: xs => (not o List.exists (fn y => y = x)) xs
			   andalso are_all_unique xs

    in
	are_all_unique (variable_names p)
    end


(* valu * pattern -> (string * valu) list option *)
fun match vp =
    case vp of
	(_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const n, ConstP k) => if n = k then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps andalso check_pat(TupleP ps)
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE                                                 
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match (v, p) else NONE
      | _ => NONE


fun first_match v pl =
    SOME (first_answer match
		       (map (fn p => (v, p)) pl))
    handle NoAnswer => NONE





			   
			   
(*         ===================== 
                 CHALLENGE
           =====================         *)
fun typecheck_patterns (definitions, patterns) =
    let								       
	fun more_lenient (t1, t2) =
	    let
		fun handle_tuples (ts1, ts2) =
		    if length ts1 <> length ts2
		    then raise NoAnswer
		    else
			List.map more_lenient (ListPair.zip(ts1, ts2))
	    in
		case (t1, t2) of
		    (_, Anything)              => t1
		  | (Anything, _)              => t2
		  | (UnitT, UnitT)             => UnitT
		  | (IntT, IntT)               => IntT
		  | (TupleT ts1, TupleT ts2)   => TupleT (handle_tuples(ts1, ts2))
		  | (Datatype s1, Datatype s2) => if s1 = s2 then t1 else raise NoAnswer
		  | _                          => raise NoAnswer
	    end    
			       
	fun pat_to_type p =
	    case p of
		Wildcard             => Anything
	      | Variable _           => Anything
	      | UnitP                => UnitT
	      | ConstP _             => IntT
	      | TupleP ps            => TupleT (List.map pat_to_type ps)
	      | ConstructorP (s, p2) => case List.find (fn (c, _, t) => (c = s) andalso (more_lenient(pat_to_type p2, t); true))
						       definitions of
					    SOME (_, data_name, _) => Datatype data_name
					  | NONE                   => raise NoAnswer

    in
	SOME (foldl (fn (p, acc) => more_lenient(pat_to_type(p), acc))
		    Anything
		    patterns)
	handle NoAnswer => NONE (* handle: no type exists *)
    end
