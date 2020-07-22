open Array
exception CellEmpty
exception UnequalRanges

(* This is the data type I have defined it is either nothing or equal to floating point numbers*)


type cell =
    | Nothing
    | Float of float


(*This matches the value to the floating point value*)

let value (a:cell):float = match a with
	Float x -> x
	| Nothing -> raise CellEmpty;;

(*This is the datatype sheet it is an array of array of cells*)
type sheet = cell array array;; 


(*Since I am returning a string in the case of indices and ranges, these functions eneble me to get the integer values from them*)

let remove_paren s = String.sub s 1 ((String.length s)-2);;

let remove_last_paren s = String.sub s 0 ((String.length s)-1);;

let remove_first_paren s = String.sub s 1 ((String.length s)-1);;

let get_list s = Str.split (Str.regexp "[':' ',']") s;;

let get_first_index s = (List.hd (get_list (remove_paren s)));;

let get_second_index s = (List.hd (List.tl (get_list (remove_paren s))));;

let get_first_row_range s = (List.hd (get_list (remove_paren (remove_paren s))));;

let get_first_col_range s = remove_last_paren (List.hd (List.tl (get_list (remove_paren (remove_paren s)))));;

let get_second_row_range s = remove_first_paren (List.hd (List.tl (List.tl (get_list (remove_paren (remove_paren s))))));;

let get_second_col_range s = List.hd (List.tl (List.tl (List.tl (get_list (remove_paren (remove_paren s))))));;


(*This function prints the array and all of its elements*)

let print_array (a:sheet) = match a with
	_-> for i = 0 to (Array.length a)-1 do
			for j = 0 to (Array.length a.(i))-1 do
				if (a.(i).(j) != Nothing) then 
					Printf.printf "%f %s" (value a.(i).(j)) " "
				else
					Printf.printf "%s" "Empty "
			done;
			Printf.printf "\n"
		done;;


(*These are helper functions, they are recursive functions that get the value of the col or the row*)

let rec sum_of_col (x: sheet) (r1:int) (r2:int) (c:int) :float = match (r1,r2) with
	(r1,r2) -> if (r1 > r2) then 0. else (value x.(r1).(c)) +. (sum_of_col x (r1 + 1) r2 c);;

let rec sum_of_row (x: sheet) (c1:int) (c2:int) (r:int) :float = match (c1,c2) with
	(c1,c2) -> if (c1 > c2) then 0. else (value x.(r).(c1)) +. (sum_of_row x (c1 + 1) c2 r);;

(*These are functions that calculate the sum of a subset of the spreadsheet provided*)

let rec full_sum_rec (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int):float = match (c1,c2) with
	(c1,c2) -> if (c1 > c2) then 0. else (sum_of_col x r1 r2 c1) +. (full_sum_rec x r1 r2 (c1+1) c2);; 

let full_sum (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = match (cell1,cell2) with
	(_,_) -> (x.(cell1).(cell2) <- Float (full_sum_rec x r1 r2 c1 c2));;


(*These are functions that calculate the max of a subset of the spreadsheet provided*)

let rec max_of_col (x:sheet) (r1:int) (r2:int) (c:int) :float = match (r1,r2) with
	(r1,r2) -> if (r1 == r2) then (value x.(r1).(c)) else (max (value x.(r1).(c)) (max_of_col x (r1 + 1) r2 c));;

let rec max_of_row (x:sheet) (c1:int) (c2:int) (r:int) :float = match (c1,c2) with
	(c1,c2) -> if (c1 == c2) then (value x.(r).(c1)) else (max (value x.(r).(c1)) (max_of_row x (c1 + 1) c2 r));;

let rec full_max_rec (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) :float = match (c1,c2) with
	(c1,c2) -> if (c1 > c2) then min_float else (max (max_of_col x r1 r2 c1) (full_max_rec x r1 r2 (c1+1) c2) );;

let full_max (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = (x.(cell1).(cell2) <- Float (full_max_rec x r1 r2 c1 c2));;

let col_max (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for i = c1 to c2 do
		x.(cell1).(i-c1+cell2) <- Float (max_of_col x r1 r2 i)
	done;;

let row_max (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for i = r1 to r2 do
		x.(i-r1+cell1).(cell2) <- Float (max_of_row x c1 c2 i)
	done;;

(*These are functions that calculate the min of a subset of the spreadsheet provided*)

let rec min_of_col (x:sheet) (r1:int) (r2:int) (c:int) : float = match (r1,r2) with
	(r1,r2) -> if (r1 == r2) then (value x.(r1).(c)) else (min (value x.(r1).(c)) (min_of_col x (r1 + 1) r2 c));;

let rec min_of_row (x:sheet) (c1:int) (c2:int) (r:int) : float = match (c1,c2) with
	(c1,c2) -> if (c1 == c2) then (value x.(r).(c1)) else (min (value x.(r).(c1)) (min_of_row x (c1 + 1) c2 r));;

let rec full_min_rec (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) : float = match (c1,c2) with
	(c1,c2) -> if (c1 > c2) then max_float else (min (min_of_col x r1 r2 c1) (full_min_rec x r1 r2 (c1+1) c2) );;

let full_min (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = (x.(cell1).(cell2) <- Float (full_min_rec x r1 r2 c1 c2));;

let col_min (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for i = c1 to c2 do
		x.(cell1).(i-c1+cell2) <- Float (min_of_col x r1 r2 i)
	done;;

let row_min (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for i = r1 to r2 do
		x.(i-r1+cell1).(cell2) <- Float (min_of_row x c1 c2 i)
	done;;


(*These are functions that calculate the value after operating each cell of the subset of the spreadsheet with the constant*)


let add_const (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) (const:float) = match const with
	_ -> for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float (const +. (value x.(i).(j)))
			done;
		done;;

let sub_const (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) (const:float) = match const with
	_ -> for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float ((value x.(i).(j)) -. const)
			done;
		done;;

let div_const (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) (const:float) = match const with
	_ -> for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float ((value x.(i).(j)) /. const)
			done;
		done;;

let mult_const (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) (const:float) = match const with
	_ -> for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float ((value x.(i).(j)) *. const)
			done;
		done;;

(*These are functions that calculate the sum of each cell of the subset of the spreadsheet*)


let sum_row_help (x:sheet) (r1:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	x.(cell1).(cell2) <- Float (sum_of_row x r1 c1 c2);;

let row_sum (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for j = cell1 to cell1+r2-r1 do
		x.(j).(cell2) <- Float (sum_of_row x c1 c2 (j-cell1+r1))
	done;;

let col_sum (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for j = cell2 to cell2+c2-c1 do
		x.(cell1).(j) <- Float (sum_of_col x r1 r2 (j-cell2+c1))
	done;;

(*These are functions that calculate the avg of each cell of the subset of the spreadsheet*)

let full_avg  (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = x.(cell1).(cell2) <- Float ((full_sum_rec x r1 r2 c1 c2) /. float_of_int((r2-r1+1)*(c2-c1+1)) );;

let row_avg (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for j = cell1 to cell1+r2-r1 do
		x.(j).(cell2) <- Float ((sum_of_row x c1 c2 (j-cell1+r1)) /. float_of_int(c2-c1+1))
	done;;

let col_avg (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for j = cell2 to cell2+c2-c1 do
		x.(cell1).(j) <- Float ((sum_of_col x r1 r2 (j-cell2+c1)) /. float_of_int(r2-r1+1))
	done;;

(*These are functions that calculate the operations on ranges of cells of the subset of the spreadsheet*)

let add_range (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (r1p:int) (r2p:int) (c1p:int) (c2p:int) (cell1:int) (cell2:int) = 
	let w1 = r2 -r1 in
	let w2 = r2p - r1p in
	let l1 = c2 - c1 in
	let l2 = c2p - c1p in
	if (w1 != w2 || l1 != l2) 
		then raise UnequalRanges
	else
		for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float ((value x.(i).(j)) +. (value x.(i-r1+r1p).(j-c1+c1p)))
			done;
		done;;

let subt_range (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (r1p:int) (r2p:int) (c1p:int) (c2p:int) (cell1:int) (cell2:int) = 
	let w1 = r2 -r1 in
	let w2 = r2p - r1p in
	let l1 = c2 - c1 in
	let l2 = c2p - c1p in
	if (w1 != w2 || l1 != l2) 
		then raise UnequalRanges
	else
		for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float ((value x.(i).(j)) -. (value x.(i-r1+r1p).(j-c1+c1p)))
			done;
		done;;

let mult_range (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (r1p:int) (r2p:int) (c1p:int) (c2p:int) (cell1:int) (cell2:int) = 
	let w1 = r2 -r1 in
	let w2 = r2p - r1p in
	let l1 = c2 - c1 in
	let l2 = c2p - c1p in
	if (w1 != w2 || l1 != l2) 
		then raise UnequalRanges
	else
		for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float ((value x.(i).(j)) *. (value x.(i-r1+r1p).(j-c1+c1p)))
			done;
		done;;

let div_range (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (r1p:int) (r2p:int) (c1p:int) (c2p:int) (cell1:int) (cell2:int) = 
	let w1 = r2 -r1 in
	let w2 = r2p - r1p in
	let l1 = c2 - c1 in
	let l2 = c2p - c1p in
	if (w1 != w2 || l1 != l2) 
		then raise UnequalRanges
	else
		for i = r1 to r2 do
			for j = c1 to c2 do
				x.(i-r1+cell1).(j-c1+cell2) <- Float ((value x.(i).(j)) /. (value x.(i-r1+r1p).(j-c1+c1p)))
			done;
		done;;

let rec count_of_row (x:sheet) (r:int) (c1:int) (c2:int):int = match (c1,c2) with
	(c1,c2) -> if(c1 > c2) then 0
				else
					if (x.(r).(c1) == Nothing) then (count_of_row x r (c1+1) c2) else 1 + (count_of_row x r (c1+1) c2);;

let row_count (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for i = r1 to r2 do
		x.(i-r1+cell1).(cell2) <- Float (float_of_int (count_of_row x i c1 c2))
	done;;

let rec count_of_col (x:sheet) (c:int) (r1:int) (r2:int):int = match (r1,r2) with
	(r1,r2) -> if(r1 > r2) then 0
				else
					if (x.(r1).(c) == Nothing) then (count_of_col x c (r1+1) r2) else 1 + (count_of_row x c (r1+1) r2);;

let col_count (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) = 
	for i = c1 to c2 do
		x.(cell1).(i-c1+cell2) <- Float (float_of_int (count_of_col x i r1 r2))
	done;;

let rec full_count_rec (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) :int = match (r1,r2) with
	(r1,r2) -> if(r1 > r2) then 0 else ((count_of_row x r1 c1 c2) + (full_count_rec x (r1+1) r2 c1 c2)) ;;

let full_count (x:sheet) (r1:int) (r2:int) (c1:int) (c2:int) (cell1:int) (cell2:int) =
	x.(cell1).(cell2) <- Float ( float_of_int (full_count_rec x r1 r2 c1 c2));;

(*

	This is for testing, uncomment to test
*)


(*
let x = [|[|Float 1.;Float 2.;Float 3.|];[|Float 8.;Float 0.;Float 9.|];[|Float 10.;Float 7.;Float 3.6|];[|Float 4.9;Float 7.7;Float 8.2|];[|Float 6.3;Float 3.4;Float 8.8|];[|Float 6.3;Float 3.4;Float 8.8|]|];;

print_array x;;

(full_count x 0 1 0 1 2 2);;

Printf.printf "\n";;

print_array x;;

*)