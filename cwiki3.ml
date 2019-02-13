



let sumProd list=List.fold_left (fun (s,p) h->(s+h,p*h)) (0,1) list;;

sumProd [1;2;3;4;6];;



(*CWICZENIA 4 *)


 type 'a bt = Empty | Node of 'a * 'a bt * 'a bt ;;

let tt = Node(1, Node(2, Node(4, Empty, Empty ), Empty ), Node(3, Node(5, Empty, Node(6, Empty, Empty ) ), Empty ) );;

(*ZADANIE 3*)

let wszerz t=
	let rec bfs list=match list with
	| []->[]
	| Empty::tail->bfs tail
	| Node(value,left,right)::tail->value::bfs(tail@[left;right])

	in bfs [t];;

wszerz tt;;



(*ZADANIE 4*)


let countWew t=
	let rec countWewWew(t,high)=match t with
	| Empty->0
	| Node(value,left,right)-> high+countWewWew(left,high+1)+countWewWew(right,high+1)

	in countWewWew(t,0);;

countWew tt;;


let countZew t=
	let rec countZewWew(t,high)=match t with
	| Empty-> high
	| Node(value,left,right)->countZewWew(left,high+1)+countZewWew(right,high+1)

	in countZewWew(t,0);;

countZew tt;;

(*ZADANIE 5*)

type 'a graph = Graph of ('a -> 'a list);;

 let g = Graph 
(function 
	| 0 -> [3] 
	| 1 -> [0;2;4] 
	| 2 -> [1] 
	| 3 -> [] 
	| 4 -> [0;2] 
	| n -> failwith("Graph g: node "^string_of_int n^" doesn't exist") );;

let deapthSearch (Graph gr) from=
	let rec dfs visited stack=
		match stack with
		| []->List.rev visited
		| hd::tl->if (List.mem hd visited) then dfs visited tl else dfs (hd::visited) ((gr hd)@tl)

	in dfs [] [from];;


deapthSearch g 2;;	 

