(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Joel Lechman
* joel1500@bresnan.net
*
***************************************************************)

(* Define your data type and functions here *)
 fun f [] = [] (* a *)
   | f (x::xs) = (x + 1) :: (f xs) (* b *);


(* Datatype set *)
datatype 'a set =
  Empty | Set of 'a * 'a set;

(* a function that determines if an element e is part of the set, set.
 This function will return true if e is a member of the set, set, and false otherwise.*)
fun isMember (e, Set(element, otherSet)) =
  if e = element then true
  else if otherSet = Empty then false
  else isMember(e, otherSet);


fun list2Set [] = Empty
| list2Set (hd::tl) = Set(hd, list2Set(tl));

(*function that returns the union of 2 sets*)
fun union (Set(element, otherSet), set_2) =
  if isMember(element, set_2) (*if the element is in set_2, skip it and move on *)
  then union(otherSet, set_2)
  else if otherSet = Empty  (*if the element is not in set_2, check if there is more in the first set, if there isnt ie its empty, you have reached the end, return the result set (the unionized set)*)
  then Set(element, set_2)
  else Set(element, union(otherSet, set_2)); (* if otherset isn't empty i.e. not the end of the first set, add element to the final set, and recursively call union again to keep moving*)



(*function that returns the intersection of 2 sets *)
fun intersect (Set(element, otherSet), set_2) =
  if otherSet = Empty andalso isMember(element, set_2)
  then Set(element, Empty) (*Checks if otherSet is empty (the end) and if element is apart of set_2. If it is, finish the set off and add element to it.*)
  else if otherSet = Empty
  then Empty (*If the program is at the end, but element is not in set_2, just return empty and finish the set.*)
  else if isMember(element, set_2)
  then Set(element, intersect(otherSet, set_2))(*If element is also in set_2, add it to the set and call intersect recursively with otherSet and set_2.*)
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
print "\nQuestion 9: ";
val a = list2Set["green", "eggs", "and"];
val b = list2Set["ham"];
union(a,b);
(*
Original print out for question nine. (I had to reformat, but my version tests the same thing.)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));
*)


(* Question 10 *)
print "\nQuestion 10: ";
val a = list2Set["stewed", "tomatoes", "and", "macaroni"];
val b = list2Set["macaroni", "and", "cheese"];
print("\n");
intersect(a,b);
(*
Original print out for quesiton 10, mine tests the same thing though.
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
*)


print("\n End of program");
