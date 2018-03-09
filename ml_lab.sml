(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Joel Lechman
* joel1500@bresnan.net
*
***************************************************************)

(* Define your data type and functions here *)

(*Warmup funtion*)
 fun f [] = [] (* a *)
   | f (x::xs) = (x + 1) :: (f xs) (* b *);


(* Datatype set *)
(*used to represent sets, two types: Set & Empty where set is of type  'element * 'element set*)
datatype 'element set =
  Empty | Set of 'element * 'element set;

(* a function that determines if an element e is part of the set, set.
 This function will return true if e is a member of the set, set, and false otherwise.*)
fun isMember (e, Set(element, otherSet)) =
  if e = element then true                  (*if the element is a part of the set8*)
  else if otherSet = Empty then false       (*if there is nothing else to check element with*)
  else isMember(e, otherSet);               (*recurse with the 'tail' of the set. ie continue comparing e. *)

(* a function that takes a list and transforms it into the created datatype 'set' using head and tail, & recursion to process each element one at a time *)
fun list2Set [] = Empty
| list2Set (hd::tl) = Set(hd, list2Set(tl));  (*add the 'head' of the list to the set datatype and recurse with the tail of the list *)

(*function that returns the union of 2 sets (NOT LISTS)*)
fun union (Set(element, otherSet), set_2) =
  if isMember(element, set_2) (*if the element is in set_2, skip it and move on *)
  then union(otherSet, set_2)
  else if otherSet = Empty  (*if the element is not in set_2, check if there is more in the first set, if there isnt ie its empty, you have reached the end, return the result set (the unionized set)*)
  then Set(element, set_2)
  else Set(element, union(otherSet, set_2)); (* if otherset isn't empty i.e. not the end of the first set, add element to the final set, and recursively call union again to keep moving*)



(*function that returns the intersection of 2 sets (NOT LISTS)*)
fun intersect (Set(element, otherSet), set_2) =
  if otherSet = Empty andalso isMember(element, set_2)
  then Set(element, Empty) (*Checks if there is more in the first set/ if otherSet is empty and if element (the head of the set) is apart of set_2. If it is, finish the final set off and add element to it.*)
  else if otherSet = Empty
  then Empty (*If the program is at the end and element is not in set_2, return empty and finish the final set.*)(*no intersect at the end*)
  else if isMember(element, set_2)
  then Set(element, intersect(otherSet, set_2))(*If element is also in set_2 add it to the final set and call intersect recursively with otherSet and set_2 to continue building the final set*)
  else intersect(otherSet, set_2);



(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];


(* Question 5 *)
val quest5 = isMember("one", (list2Set(["1", "2", "3", "4"])));
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");


(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
val a = list2Set["green", "eggs", "and"];
val b = list2Set["ham"];
print "\nQuestion 9: ";
union(a,b);
(*
Original print out for question nine. (My version is reformatted but tests the same thing.):
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));
*)


(* Question 10 *)
val a = list2Set["stewed", "tomatoes", "and", "macaroni"];
val b = list2Set["macaroni", "and", "cheese"];
print("\n");
print "\nQuestion 10: ";
intersect(a,b);
(*
Original print out for quesiton 10, mine tests the same thing though:
print "\nQuestion 10: ";element
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
*)


print("\n End of program");
