 type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);; 

let llist1=LCons(1,function ()->LCons(2,function ()->LCons(9,function ()->LNil)));;

(*Funkcje wspomagające *)

let rec ltake (number,llist)=match (number,llist) with
| (0,_)->[]
| (_,LNil)->[]
| (number,LCons(x,xf))->x::ltake(number-1,xf());;

let rec (@$) ll1 ll2 =
	 match ll1 with 
	| LNil -> ll2 
	| LCons(x, xf) -> LCons(x, function () -> (xf()) @$ ll2);; 

(*ZADANIE 1 *)

let rec f number x=match number with			(*Zwraca leniwą liste powtórzoną z danym elementem number razy*)
| 0->LNil
| _->LCons(x,function()->f (number-1) x);;

ltake(4,(f 4 7));;


let rec multiElements (k,llist)=match llist with
| LNil->LNil
| LCons(x,xf)->(f k x) @$ multiElements(k,xf());;

multiElements(3,llist1);;


ltake(9,multiElements(3,llist1));;


(*ZADANIE 2*)


let fibo=
	let rec fiboH ppn pn=
		LCons(ppn,function()->fiboH pn (ppn+pn))
	in fiboH 0 1;;	

ltake(10,fibo);;

(*ZADANIE 3a*)
 type 'a lBT = LEmpty  |  LNode of  'a * (unit ->'a lBT) * (unit -> 'a lBT);; 

let wszerz tree=
	let rec pomWszerz list=match list with
	| []->LNil
	| LEmpty::tail->pomWszerz tail
	| LNode(value,left,right)::tail->LCons(value,function()->pomWszerz (tail@[left();right()]))

	in pomWszerz [tree];;

let tr=LNode(2,(function()->LEmpty),(function()->LEmpty));;		(*Muszą byc nawiasy na funkcje bo inaczej nie wykrywa to jako 2 argumenty*)
let tt=LNode(2,(function()->LNode(8,(function()->LEmpty),(function()->LEmpty))),(function()->LNode(3,(function()->LEmpty),(function()->LEmpty))));;

ltake(3,(wszerz tt));;



(*ZADANIE 3b*)
let rec iTree n=match n with
| 0->LEmpty
| _->LNode(n,(function()->iTree (2*n)),(function()->iTree (2*n+1)));;