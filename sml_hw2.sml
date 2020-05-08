
(* Jacob Hill *)

(* #1 - quicksort *)
(* If input list = [] returns the empty lists *)
(* else we make a recursive call with pivot and rest *)
(* and assign those values to below and above. If the first element of the list is less than the pivot, *)
(* we insert it into the int list below. Otherwise, we insert it into the int list above. *)

fun partition(nil, _) = (nil,nil)
| partition(first :: rest, pivot) = 
let 
val(below,above) = partition(rest,pivot)
in 
if first < pivot then (first :: below, above) else (below, first :: above)
end;


(* Uses partition as a helper *)
(* If the list is empty, returns an empty list. Otherwise, we call the partition with the first and the rest of the list *)
(* and assign those values to below and above. The function recursively calls itself with the int list *)
(* below is concatenated with the first which is concatenated to the recursive call with the int list above. *)


fun quicksort nil = nil
| quicksort(first::rest) = 
let 
val(below,above) = partition(rest,first)
in 
quicksort(below) @ [first] @ quicksort(above)
end;

(* #2 - member *)
(* Searches a list and returns a boolean value false if the item is not in tbe list or empty *)


fun member (_,nil) = false
| member (inList,first::rest) = inList = first orelse member(inList,rest);
               
             	   
(* #3 - returns the union of sets (lists) s1 and s2*)
(* If any list is empty it will return the other list *)
(* Uses member() function as a helper to see if the first element of the first set = an element of the second set.*)
(* If it doesn't contain the element, it inserts the first element of the first set to the *)
(* recursive call with the rest of the first set and all of the second set. *)
fun union(s1, nil) = s1
| union(nil, s2) = s2
| union(s1init::s1Rest, s2) = 
	if member(s1init, s2) then union(s1Rest, s2)
	else s1init::union(s1Rest,s2);


(* #4 - returns the intersection of sets (lists) s1 and s2 *)
(* If any list is empty it will return an empty list *)
(* Uses member() as a helper function to see if the first element of the first set = an element of the second set. *)
(* If it is an element, insert the first element of the first list to the recursive call with the rest of the first list and all of the second list.*)
(* if its not an element recursively call this function with the rest of the first list and all of the second list. *)


fun intersection(_,nil) = nil
| intersection(nil, _) = nil
| intersection(s1init::s1Rest, s2) = 
	if member(s1init, s2) then s1init::intersection(s1Rest,s2)
	else intersection(s1Rest, s2);

(* #5 - Return list of integers from start (inclusive) to stop (exclusive) by step *)
(* If starting number > stopping number and the step size is ~, or if starting number < stopping number and the step size is positive, *)
(* we insert starting number into the recursive call with the function with the new starting number increased or decreased by the step size. *)
(* Otherwise we return an empty list. *)


fun range(start, stop, step) =
	if start > stop andalso step < 0
	orelse start < stop andalso step > 0 
	then start::range(start+step, stop, step)
	else nil;

(* #6 - Return a slice of a list between indices start, and stop. Assume first element of list is at index 0*)
(* slicetrack acts as a helper function *)
(* We use a counter to determine if the count of the recursive call is between start and stop indices *)
(* if so we insert the first element to the rest of the list, then recursively call the function with the rest of the list.*)
(* then the start, stop, and counter indices increase by 1 *)
(* if the counter is not between the start index and stop index *)
(* we recursively call the function with the rest of the list, start, stop, and counter are increased by 1 *)

fun slicetrack(nil,start,stop,counter) = nil
| slicetrack(first::rest, start,stop,counter) =
	if counter >= start andalso counter < stop
	then first::slicetrack(rest,start,stop,counter+1)
	else slicetrack(rest,start,stop,counter+1);

(* Calls slicetrack with the list, start, stop, and counter *)


fun slice(aList, start, stop) = slicetrack(aList,start,stop,0);

