

(*-------------- 
This exception is declared to handle if the input string is not a valid numeric value
---------------*)
exception Invalid_Input_exception of string

(*----------------
	Function Name: dropzeroes 
	Input Type: int list
	Objective: To drop the prefix zeroes
	Operation: This function drop zeroes from the list
	Output type: int list
----------------------*)
fun dropzeroes([]) = [] 
| dropzeroes(l: int list) =
if hd(l)<>0 then l
else
dropzeroes(tl(l))

fun prefixzeroes(l1 : int list, 0) = l1 
|prefixzeroes(l1 : int list, count : int) =
	prefixzeroes(0::l1,count-1)

(*----------------
	Function Name: makeequal 
	Input Type: int list * int list
	Objective: To make two lists equal length 
	Operation: This function appends zeroes to the smaller list and make the lengths equal
	Output type: int list * int list
----------------------*)
fun makeequal(l1 : int list, l2: int list) =
let
val len1 = List.length(l1)
val len2 = List.length(l2)
in
if len1= len2 then (l1,l2)
else 
let 
val diff = Int.abs(len1 - len2)
in
if(len1 < len2) then makeequal(prefixzeroes(l1,diff),l2)
else makeequal(l1,prefixzeroes(l2,diff))
end
end


(*----------------
	Function Name: inttolist 
	Input Type: int * int list
	Objective: To convert an integer into int list
	Operation: This function makes the length of string multiple of 4 and then passes it to the function fromStr
	Output type: int list
----------------------*)
fun inttolist(num : int, lst : int list)= 
	if num <= 0 then []@lst
	else
	(
		let
		val m = num mod 10000 :: lst
		in
		inttolist(num div 10000,m)
		end
	)
(*----------------
	Function Name: fromStr 
	Input Type: string
	Objective: To convert string to int list
	Operation: This function is called by the function fromString, it converts the string into an int list and 
				return to the function fromString
	Output type: int list
----------------------*)
fun fromStr(numstr: string)= (*from string to int list conversion*)
	if numstr ="" then []
	else
	let
	val len = size numstr
	val sub = substring(numstr,len-4,4);
	in
	valOf(Int.fromString sub)::fromStr(substring(numstr,0,len-4))
	end;

(*----------------
	Function Name: fromString 
	Input Type: string
	Objective: To call the function fromStr
	Operation: This function makes the length of string multiple of 4 and then passes it to the function fromStr
	Output type: int list
----------------------*)
fun fromString(numstr: string)= (*from string to int list conversion*)
 let
 	val len = size numstr
 	val modval = len mod 4
 	in
 	if modval = 1 then List.rev(fromStr("000"^numstr))
	else if modval = 2 then List.rev(fromStr("00"^numstr))
	else if modval = 3 then List.rev(fromStr("0"^numstr))
	else List.rev(fromStr(numstr))
	end



(*
	Function Name: toStr 
	Input Type: int list
	Objective: Helps the caller function toString to convert an int list to string
	Operation: checks the length of each list element and if the length is not mod 4 =0 then append zeroes accordingly
	Output type: string

*)
fun toStr(oplist : int list)=
	if List.null oplist then ""
	else
	let
	val len = (if hd(oplist)>=0 andalso hd(oplist)<=9 then 1 
	else if hd(oplist)>=10 andalso hd(oplist)<=99 then 2 
	else if hd(oplist)>=100 andalso hd(oplist)<=999 then 3
	else  4)
	in 
	if len mod 4 = 1
	then "000"^Int.toString(List.nth(oplist,0))^toStr(tl oplist)  
	else if len mod 4 = 2
	then "00"^Int.toString(List.nth(oplist,0))^toStr(tl oplist)  
	else if len mod 4 = 3 
	then "0"^Int.toString(List.nth(oplist,0))^toStr(tl oplist)  
	else
	Int.toString(List.nth(oplist,0))^toStr(tl oplist)  
	end
(*----------------
	Function Name: toString 
	Input Type: int list
	Objective: To convert an int list to string
	Operation: Cut the head element and calls another helping function toStr to convert rest of the list to string
	Output type: string
----------------------*)
fun toString( oplist : int list)= (*from int list to string conversion*)
	if List.null oplist then ""
	else 
	let
	val h = hd(oplist)
	in
	Int.toString(h)^toStr(tl oplist)
	end


(* -------------
Function Name: split
Input Type: int list * int * int list * int list 
Objective: To split a list
Operation: this function helps the karastuba function to split the list into two parts.
Output: int list * int list
--------------*)
fun split(l, 0, head, tail) = (List.rev(head), tail@l) (*split list into two parts, first part contains m cells*)
|   split(l : int list, m : int, head : int list, tail : int list) = 
		split(tl(l), m-1, hd(l)::head, tail)

(* -------------
Function Name: addlist
Input Type: int list * int list * int list * int
Objective: To add two lists
Operation: It takes 4 inputs, two input lists, sum and carry. Initially sum is empty list and carry is assigned to 0. 
			it takes head elements, add them and finally append the result to the final list.
Output: int list
--------------*)

fun addlist([], [], sum, carry) = if carry=1 then 1::sum else sum
| addlist(l1: int list, l2 : int list, sum : int list, carry: int)=
	if (hd(l1) + hd(l2)+carry >= 10000) then addlist(tl l1, tl l2, ((hd(l1) + hd(l2)+carry)-10000)::sum,1)
	else
	addlist(tl(l1), tl(l2), ((hd(l1)+hd(l2) + carry)::sum),0)


(* -------------
Function Name: add
Input Type: int list * int list
Objective: To call the function add list
Operation: This function is caller function which calls the addlist function to do the actual addition. It makes two lists 
			equal, reverse them and send the lists to addlist funciton.
Output: int
--------------*)
fun add([],l2) = l2
| add(l1, []) = l1 
| add(l1: int list, l2: int list) = 
	let 
	val(list1, list2) = makeequal(l1,l2)
	in
	addlist(List.rev list1, List.rev list2,[],0)
	end

(* -------------
Function Name: subtract
Input Type: int list * int list
Objective: To compare two lists
Operation: This function compares the two input list and sets the flag 0,1 or 2 accordingly.
Output: int
--------------*)
fun complisteqlen([],[])= 0
| complisteqlen(l1 : int list, l2 : int list)=
	if hd(l1)> hd(l2) then 1
	else if hd(l2) > hd(l1) then 2
	else complisteqlen(tl(l1),tl(l2))


(* -------------
Function Name: complist
Input Type: int list * int list
Objective: To compare two lists
Operation: This function compares and return the lists in order.
Output: int list
--------------*)
fun complist(l1 : int list, l2: int list) = 
 if List.length(l1) > List.length(l2) then 1
else if List.length(l2) > List.length(l1) then 2
else complisteqlen(l1, l2)

(* -------------
Function Name: subtract
Input Type: int list * int list * int list * int
Objective: To subtract two lists
Operation: This function is called by the calling function sublist, it does the actual subtraction on two lists.
Output: int list
--------------*)

fun subtract([],[],diff,borrow) = diff 
|subtract(l1: int list, l2 : int list, diff : int list, borrow : int) =
	if borrow = 0 then
	if hd(l1)>=hd(l2) then subtract(tl(l1), tl(l2),hd(l1)-hd(l2)::diff,0)
	else
	subtract(tl(l1), tl(l2),((hd(l1)+10000)-hd(l2))::diff,1)
	else
	let
	val l1 = (hd(l1)-1)::tl(l1) 
	in
	if hd(l1)>=hd(l2) then subtract(tl(l1), tl(l2),hd(l1)-hd(l2)::diff,0)
	else
	subtract(tl(l1), tl(l2),((hd(l1)+10000)-hd(l2))::diff,1)
	end
(* -------------
Function Name: Sublist
Input Type: int list * int list
Objective: to call the helping function subtract
Operation: this function compare two lists, find the bigger one, make then equal length, reverse the lists and then send the lists 
		   to subtract function for the actual subtraction.
Output: int list
--------------*)
fun sublist(l1 , [] ) = l1 
| sublist([], l2 ) = l2 
| sublist(l1 : int list, l2: int list ) =
	let
	val flag = complist(l1,l2)
	val(l1,l2) = makeequal(l1,l2)
	in
	if flag = 0 then [0] 
	else if flag = 1 then dropzeroes(subtract(List.rev l1,List.rev l2,[],0))
    else dropzeroes(subtract(List.rev l2,List.rev l1,[],0))
	end
(* -------------
Function Name: appendzeroes
Input Type: int list
Objective: to calculate B^m
Operation: this function computes z2*B^(2m) and z1*B^m where B=10000
Output: int list
--------------*)
fun appendzeroes(l, 0) = l
| appendzeroes(l : int list , m : int) = 
  	appendzeroes(l@[0000],m-1)

(*
	Function: karastuba
	Input Type: int list -> int list
	Objective: This function is the core of the solution, it calculate the multiplication of two input numbers.
	Operation: It recursively calls it self to divide the lists into two parts each until both the lists have 1-1 element each then
				multiply them and return the multiplication.
	Output: int list
*)
fun karatsuba( l1: int list)=
	fn(l2 : int list)=>
	if List.length l1 = 1 andalso List.length l2 =1
	then 
	inttolist(hd(l1)*hd(l2),[])
	else
	let
	val (l1,l2) = makeequal(l1,l2)
	val n = Real.fromInt(List.length l1)
	val m = ceil(n/2.0)
	val m1 = floor(n/2.0)
	val (x1, x0) = split(l1, m1, [], []); (*dividng the first list into two parts*)
	val (y1, y0) = split(l2, m1, [], []); (*dividng the second list into two parts*)
	val z2 =  karatsuba x1 y1 
	val z0 = karatsuba x0 y0
	val z2z1z0 = karatsuba (add(x1,x0)) (add(y1,y0))
	val z1z0 = (sublist(z2z1z0,z2))
	val z1 = (sublist(z1z0,z0))

	val z2_0 = appendzeroes(z2,2*m)
	val z1_0 = appendzeroes(z1,m) 
	val z2_0_z1_0 = add(z2_0, z1_0)
	val xy = add(z2_0_z1_0,z0)
    in
    dropzeroes(xy)
    end
(*--------------
	Funciton: f1
	Input Type: Int list
	Objective: Helping function to help the function Factorial
	Operation: This function implement the actual factorial
	Output Type: int list
--------------*)

fun f1 (n : int, pre: int list, k : int list) =
	if n>0 then
	let
	val res = karatsuba pre k
	in
	f1(n-1, res, add(k,[1])) 
	end
	else
	toString(pre) 
(* ------------
	Function: factorial
	Input Type: string
	Objective: To compute the factorial of the given input
	Operation: this function checks if the given input is a valid number, the calls the function f1 to calculate actual factorial.
	Output Type : string

------------*)
fun factorial(instr : string)=  (*main function, where the execution starts from*)
	if List.all Char.isDigit(String.explode instr) then f1((valOf(Int.fromString instr)),[1],[1])
	else raise Invalid_Input_exception "Input is not valid"

