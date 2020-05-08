(* SML comments appear like this *)
(* Jacob Hill *)

(* #1 - pow DONE *)
(* Returns a^b *)
(* Initially sets parameter x = 0 in which case we return 1,
otherwise we recursivelly call pow and multiply n*n  x number of times *)
 
fun pow (n, 0) = 1  
| pow (n, x) = n * pow(n, x-1);

					
(* #2 - sumTo DONE *)
(* Computes sum of first "n" reciprocals, sumTo(3) should return (1 + 1/2 + 1/3) = 1.83333333.... The function should return 0.0 if n is 0.  *)
(* Converts int to sum under either condition, if not = 0, then divide 1 by the int and the recursion call of the number -1 *)
fun sumTo n = 
	if n = 0 then 0.0 else 
	1.0 / real(n) + sumTo(n-1);



               
(* #3 - repeat DONE *)
(* We set x = 1 for the purpose of not repeating a given string one more time than intended *)
(* We print the string and call the function constanly until we've reached our desired number of string repititions *)
fun repeat (s:string, x:int) = 
	if x = 1 then s else 
	s ^ repeat(s,x-1);


(* #4 - binary *)
(* Initially convert a zero value to a string, else we get the recursion call of out input div 2, we then concat that with our string value of the remainder+48*) 

fun binary x = 
	if x = 0 then "0" else
	binary(x div 2)^str(chr((x mod 2) +48));

(* #5 - countNegative DONE *)
(* If the current head of our list is greater than zero, we recursively call on our tail *)
(* if our number is negative we up our counter by one and recursively call the tail again *)

fun countNegative x = 
	if null x then 0 else 
	if hd x >= 0 then countNegative(tl(x)) else 
	1 + countNegative(tl x);



(* #6 - absList DONE *)
fun tuplehandler (n,r) = (abs(n), abs(r));

(* tuplehandler gets the absolute value of a pair of tuples *)
(* If we dont have an empty list we call upon the head of our tuple and concat that to the tail of our list *)

fun absList x = 
	if null x then [] else tuplehandler(hd x)::absList(tl x);

(* #7 - split DONE *)

fun inthandler x = 
	if x mod 2 = 0 then (x div 2, x div 2) else (x div 2, (x div 2) + 1);

(* inthandler assures that we can cleanly pass our input value into the function and any remainder to the second tuple value*)
(*split populates an empty list with tuples from the previous function *)

fun split n = if null n then [] else inthandler(hd n)::split(tl n);

(* #8 - isSorted *)
(* Accounts for an empty list or 1 input to be sorted & true*)
(* if the head of our list is less than the front of the tail that is also true, otherwise false. *)
fun isSorted n = 
	if null n then true else 
	if length(n) = 1 then true 
	else if hd n <= hd(tl n) then (true; isSorted(tl n)) else false;

 
(* #9 - collapse *) 
(* Considering we have an empty set *) 
(* we attempt to combine a pair of numbers and replace those values with their sum *)
fun collapse (lin::nil) = [lin] 
| collapse(x::n::rem) = 
	if length(rem) > 0 then
	(x+n) :: collapse(rem) else [x+n];

(* #10 - insert DONE*)        
(* We recurse through the insert function and for each iteration, check to see if our number *)
(* is less than our current head, if so, it is inserted into the front of the node we are currently on *)
fun insert(init,nil) = [init]
 | insert (init,y::ys) = 
	if init<y then 
	init::y::ys else 
	y :: insert (init,ys);
