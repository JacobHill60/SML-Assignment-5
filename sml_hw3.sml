
(* Jacob Hill *)

(* #1 - duplist *)
(* Takes the list and returns two of each value in the list *)
(* Accomplishes this using an anonymous function within foldr *) 
(* fn takes 'a' and appends the respected element to itself, then adds them to an empty list *)

fun duplist x = foldr(fn (a, b) => a::a::b) [] x;

(* #2 - mylength *)
(* Function that returns the length of a list *)
(* Handles empty list (returns 0) & populated list *)
(* Anonymous function adds 1 for each index accounted for in the list *)

fun mylength x = foldr(fn (_, y) => 1 + y) 0 x;

(* #3 - il2absrl *)
(* Function that takes a list of integers and returns a list of the absolute values of each integer as a real number. *)
(* Uses two predefined functions, "real" to convert to reals and "abs" to convert to absolute value. *)
(* Anonymous function converts the current value of the list into an absolute value, then real, then populates an empty list *)

fun il2absrl x = foldr(fn (a,b) => real(abs(a))::b) [] x;

(* #4 - myimplode *)
(* function myimplode that works just like the predefined implode. *)
(* Takes a list of characters and converts the list to a string *)
(* Uses foldr with op keyword and "^" operator to add our value to an empty string *)
(* Anonymous function uses String.str to convert char -> str before adding it to an empty list *)

fun myimplode x = foldr (op ^) "" (foldr (fn (a,b) => String.str(a)::b) [] x);

(* #5 - lconcat *)
(* Function lconcat takes a list of lists, returns the list formed by appending the input lists together in order. *)
(* Pretty straight forward with the implementation, similar to the previous problems with using foldr *)
(* The anonymous function concatenates the lists and populates an empty list *)

fun lconcat x = foldr (fn (a, b) => a @ b) [] x;


(* #6 - convert *)
(* Function convert converts a list of pairs into a pair of lists, preserving the order of the elements. *)
(* The anonymous function handles elements in pairs within a list in such a way that .. *)
(* it matches the first elements of pairs as well as the last elements of pairs and populates them into their respective lists *)

fun convert x = foldr (fn ((a, b), (c, d)) => (a::c, b::d)) ([], []) x;

(* #7 - mymap *)
(* Function mymap represents the functionality of map without using map *)
(* map function itself is curried *)
(* Uses anonymous function to apply a function to each element of a list *)

fun mymap f x = foldr (fn (a, b) => f a::b) [] x;


(* #8 - myfoldl *)
(* Represents the functionality of foldl without using foldl *)
(* foldl is curried *)
(* foldl should be implemented similar to foldr except it goes from left to right associativity *)
(* given a head and tail of a list, we're basically just saying that the implementation is equal to *)
(* the evaluation of a list starting with the head and continuing with the evaluation through the tail of the list *)

fun myfoldl _ x [] = x
|   myfoldl f x (head::tail) = myfoldl f (f (head, x)) tail;


