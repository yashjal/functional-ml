(* helper functions length and sum *)
fun length [] = 0              
    | length (x::xs) = 1 + length(xs);
fun sum [] = 0.0
    | sum (x::xs) :real = x + sum(xs);

exception EmptyList;

(* if x = [], avg_list throws an exception instead of diving by 0 *)
fun avg_list [] = raise EmptyList
    | avg_list x = (sum x)/real(length x);

exception NoItem;
fun get_index [] _ = raise NoItem
    | get_index (x::xs) 0 = x
    | get_index (x::xs) index = get_index xs (index-1);

fun get_odd_midpoint x = get_index x ((length x) div 2);

fun get_even_midpoint x = 
     let val index = (length x) div 2 
     in
        ((get_index x index) + (get_index x (index-1)))/2.0
     end;

fun get_median x =               
     if ((length x) mod 2) = 0 
     then get_even_midpoint x
     else get_odd_midpoint x;

(* helper function sum_int *)
fun sum_int [] = 0
    | sum_int (x::xs) = x + sum_int(xs); 

fun listsum x n = (sum_int x) = n;

fun isten x = listsum x 10;

exception Mismatch;
fun zip ([],[]) = []
 (* if exclusively 1 list is empty then length of lists are different *)
    | zip ([], y) = raise Mismatch
    | zip (x, []) = raise Mismatch
    | zip (x::xs, y::ys) = (x,y)::(zip (xs,ys)); 

fun unzip [] = ([],[])
    | unzip ((x,y)::xy) = 
 (* we need the value of (unzip xy) explicitly to be able to use :: on each of the lists in the tuple *)
      let val (l1,l2) = unzip xy
      in (x::l1,y::l2)
      end; 

(* accumulator is calculated during preorder processing of the recursion *)
(* using :: the accumulator is then added to the return list recursively *) 
fun scan_left F y [] = y::[] 
    | scan_left F y (x::xs) = y::(scan_left F (F y x) xs);

(* helper function countup creates a list 1,…,n *)
fun countup n =   
    let fun countup1 0 l = l
          | countup1 i l = countup1 (i-1) (i::l)
    in countup1 n []
    end;

fun fact_list n = tl (scan_left (fn x => fn y => x*y) 1 (countup n));
