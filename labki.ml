(function x->x mod 3=0)7



let rec zip (list,list2)=match (list,list2) with
| ([],[])->[]
| (hd::tl,hd2::tl2)->hd::hd2::zip(tl,tl2)
| (hd::tl,[])->hd::zip(tl,[])
| ([],hd::tl)->hd::zip([],tl);;
	



zip([1;2;3;4;5;6],[7;8;9;10;11;12]);;

zip([],[1;2;3]);;
zip([1;2;3],[]);;
zip([1;2],[3]);;


let rec exist list elem=match list with
| []->false
| hd::tl->if hd=elem then true else exist tl elem;;


let rec delete list elem= match list with
| []->[]
| hd::tl->if exist hd elem then delete tl elem else hd::delete tl elem;;


delete [[1;2;3;4;5];[1;2;3;5];[1;2;3;5];[1;2;3;4;5]] 4;;




let rec binary list=match list with
| []->0.
| hd::tl->(2.**(float_of_int(List.length list)-.1.)*.hd)+.(binary tl);;

binary [0.;1.;1.;1.;1.];;


binary[];;
binary[0.];;
binary[1.];;
binary[0.;0.];;


(*let binary list=List.map (fun x->x*x) list;;

binary [1;2;3;4;5;6];;

let sum list=
	let rec sumT acc list=
		if list=[] then acc else sumT (acc+(List.hd list)) (List.tl list)
		
	in sumT 0 list;;

	let sumSqr list=sum (binary list);;


sumSqr [1;2;3;4;5;6];; *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let t=Node(8,Node(3,Node(1,Empty,Empty),Node(6,Node(4,Empty,Empty),Node(7,Empty,Empty))),Node(10,Empty,Node(14,Node(13,Empty,Empty),Empty)));;

		
let rec contains (liczba,tree)= match tree with
	| Empty ->false
	| Node(v,t1,t2)->if v=liczba then true else contains (liczba,t1) || contains (liczba,t2);; 
		 


contains (10,t);;


let rec sum =function
	| Empty->0
	| Node(v,t1,t2)->v+sum t1+sum t2;;

sum t;;

let rec iloczyn =function
	| Empty->1
	| Node(v,t1,t2)->v*sum t1*sum t2;;

iloczyn t;;



type dzialanie=Suma of dzialanie*dzialanie|Negacja of dzialanie|Liczba of int;;


let rec funkcja o=match o with
| Suma(x,y) ->(funkcja x) + (funkcja y)
| Negacja(x) ->0-(funkcja x)
| Liczba(x) -> x

let k1=Suma(Negacja(Liczba(3)),Liczba(4));;
let k2=Negacja(Liczba(8));; 
let k3=Suma(Suma(Negacja(Liczba(5)),Liczba(8)),Suma(Liczba(9),Negacja(Liczba(1))));;


funkcja k1;;
funkcja k2;;
funkcja k3;;

(*let (_,_,x)=(1,2,3);;
snd (1,2);;*)