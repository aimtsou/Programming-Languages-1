(* quicksort *)
fun quicksort [] = []|
	quicksort [x] = [x]|
	quicksort (a::bs) = 
	let
		fun partition (left,right,[]) = (quicksort left) @ (a::quicksort right)|
			partition (left, right, x::xs) = 
				if x<=a then partition (x::left, right, xs)
				else partition (left, x::right, xs)
	in partition([],[],bs)
	end;	

(*power function *)
fun (a: IntInf.int) ^ (b: IntInf.int)  =
  if b = 0 then 1
  else a * (a ^ (b-1));	
  
fun initialize n k = 
	let fun iterate 0 result = result
		| iterate 1 result = k::result
		| iterate i result = iterate(i-1) ((n-n)::result)
	in
		iterate n []
	end

fun equal  k nil = false
  | equal  k [x] = if (x=k) then true 
			     else false
  | equal  k (x::xs) = if (x <> k) then false	
                                    else equal k xs 
		
(* find the next sorted number so we can proceed *)
fun next_sort b n  nil = nil
  | next_sort b n [x]  = [(x+1) mod b]  
  | next_sort b n  (x::y::cs) = if equal x (x::y::cs) then  ((x+1) mod b)::(initialize (n-1) 0)
						  else   x::(next_sort b (n-1) (y::cs))  

(* Get the biggest sorted number given base and number of digits*)
fun create_number b n =
		if (n mod 2) = 0 then 
			let 
				fun fill base number digit = 
				if (digit < (number div 2)) then (base-1)::fill base number (digit+1)
				else if (digit = (number div 2)) then (base-2)::fill base number (digit+1)
				else if (digit = number) then [0]
				else 0::fill base number (digit+1)
			in
			(fill b n 1)
			end
		else
			let
				fun fill base number digit = 
				if (digit <= (number div 2)) then (base-1)::fill base number (digit+1)
				else if (digit = ((number div 2) +1)) then (base-2)::fill base number (digit+1)
				else if (digit = number) then [0]
				else 0::fill base number (digit+1)
			in
			(fill b n 1)
			end;
			
(* base conversion *)			
fun convert_ten [] (pow:IntInf.int) b  (sum:IntInf.int) =  sum
	|convert_ten (x::xs) pow b sum = convert_ten xs (pow div b) b (sum + pow*IntInf.fromInt x)
	
fun reverse [] = []
	| reverse (h::t) = reverse(t) @ [h];	
	
(* substract 2 lists *)	
fun my_substract [] [] c  b = []
	|my_substract (x::xs) (y::ys) c b= 	
		if ( x >= y + c) then (x - y-c )::my_substract xs ys 0  b
		else    (x+b-y-c)::my_substract xs ys 1 b
		
fun substract x y b = reverse(my_substract x y 0 b);	

(* return the first element *)	
fun  first [] 1 i = i
	|first [x] 1 i = i
	| 	first (x::y::xs) 1 i =
		if ( x=y) then first  (x::xs) 1 (i+1)
		else	  i
	| first (x::xs) start i = first xs (start-1) i	
	
(* apply kaprekar routine *)	
fun Kaprekar []  b= []
	| Kaprekar x 0 = []
	| Kaprekar x b =  
	let 
		val (sorted) = (quicksort x)
	in
		substract sorted (List.rev(sorted)) b
	end;		

(* check if the number is magic *)
fun is_it_magic x b n=
	if (first x 1 1 <> n) andalso ((Kaprekar x b) = x) then true
	else false;
	
fun zeros 0=[]|
	zeros k=(0)::zeros (k-1)	

(* function to call kaprekar procedure and check if our given number is a magic number *)
fun magic_routine number b n =
	if (number = (1::zeros(n-1))) then 0
	else
		let 
			val test = Kaprekar number b
		in 
			if  (is_it_magic test b n)  then  convert_ten test ((IntInf.fromInt b)^(IntInf.fromInt n-1)) (IntInf.fromInt b) (IntInf.fromInt 0)
			else magic_routine (next_sort b n number) b n 
		end;

(* main magic function *)
fun magic b n =
	if (b < 2) then 0
	else
	let 
		val num = (create_number b n)
	in
		magic_routine num b n
	end;