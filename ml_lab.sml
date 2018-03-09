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


isMember(1,Set(1,Set(2,Set(3,Empty))));


fun list2Set [] = Empty
| list2Set (hd::tl) = Set(hd, list2Set(tl));

(*returns the untion of 2 different sets*)
fun union (Set(element, otherSet), set2) =
  if isMember(element, set2) then union(otherSet, set2) (*Checks if element is in set2. If it is skip it and move on.*)
  else if otherSet = Empty then Set(element, set2) (*If element is not in set2, check if otherSet is empty. If it is, you have reached the end, return the unionized set.*)
  else Set(element,union(otherSet, set2)); (*If otherset isn't empty i.e. not the end, add element to the set, and recursively call union with otherset and set2*)



(*returns the intersection of 2 different sets *)
fun intersect (Set(element, otherSet), set2) =
  if otherSet = Empty andalso isMember(element, set2) then Set(element, Empty) (*Checks if otherSet is empty (the end) and if element is apart of set2. If it is, finish the set off and add element to it.*)
  else if otherSet = Empty then Empty (*If the program is at the end, but element is not in set2, just return empty and finish the set.*)
  else if isMember(element, set2) then Set(element, intersect(otherSet, set2))(*If element is also in set2, add it to the set and call intersect recursively with otherSet and set2.*)
  else intersect(otherSet, set2);



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
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
