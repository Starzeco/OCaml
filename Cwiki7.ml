(*ZADANIE 1*)
module type QUEUE_FUN = 
	sig   
		type 'a t   
		exception Empty of string   
		val empty: unit -> 'a t   
		val enqueue: 'a * 'a t -> 'a t   
		val dequeue: 'a t -> 'a t          
		val first: 'a t -> 'a   
		val isEmpty: 'a t -> bool 
	end;; 

(*PODPUNKT A*)
module QueueList:QUEUE_FUN=
	struct
		type 'a t='a list
		exception Empty of string
		let empty ()=[]
		
		let enqueue (element,kolejka)=kolejka@[element]
		
		let dequeue kolejka=match kolejka with
		| []->[]
		| _::tl->tl
		
		let first kolejka=match kolejka with
		| []->raise (Empty "Pusta kolejka")
		| hd::_->hd
		
		let isEmpty kolejka=kolejka=[]
		
	end;;

	
		
let q = QueueList.empty();;
let q1 = QueueList.enqueue(1, q);;
let q2 = QueueList.enqueue(2, q1);;

				
QueueList.dequeue q2;;
QueueList.dequeue q1;;
QueueList.dequeue q;;
QueueList.first q1;;		
QueueList.first q2;;	
QueueList.first q;;				
QueueList.isEmpty q;;	

(*PODPUNKT B*)						
												
module PairQueueList:QUEUE_FUN=
	struct
		type 'a t='a list *'a list
		
		exception Empty of string
		
		let empty ()=([],[])
		
		let toNormal pair=match pair with
		| ([],second)->(List.rev second,[])
		| (first,second)->(first,second)

		let enqueue (element,para)=toNormal (fst para,element::snd para)
		
		let dequeue para=match para with
		| ([],_)->([],[])
		| (hd::tl,second)->toNormal (tl,second)

		let first para=match para with
		| ([],_)->raise (Empty "Pusta kolejka")
		| (hd::tl,_)->hd

		let isEmpty para=fst para=[]
		
	end;;


let pq=PairQueueList.empty();;
let pq1=PairQueueList.enqueue(1,pq);;
let pq2=PairQueueList.enqueue(2,pq1);;
let pq3=PairQueueList.enqueue(3,pq2);;

PairQueueList.dequeue pq;;
PairQueueList.dequeue pq1;;
PairQueueList.dequeue pq2;;
PairQueueList.dequeue pq3;;

PairQueueList.first pq;;
PairQueueList.first pq1;;
PairQueueList.first pq2;;
PairQueueList.first pq3;;

PairQueueList.isEmpty pq;;
PairQueueList.isEmpty pq1;;
PairQueueList.isEmpty pq2;;
PairQueueList.isEmpty pq3;;

																																																	
																																																																																																		
(*ZADANIE 2*)
module type QUEUE_MUT = 
	sig   
		type 'a t         (* The type of queues containing elements of type ['a]. *)   
		exception Empty of string         (* Raised when [first q] is applied to an empty queue [q]. *)   
		exception Full of string         (* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)   
		val empty: int -> 'a t         (* [empty n] returns a new queue of length [n], initially empty. *)   
		val enqueue: 'a * 'a t -> unit       (* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)   
		val dequeue: 'a t -> unit         (* [dequeue q] removes the first element in queue [q] *)           
		val first: 'a t -> 'a        (* [first q] returns the first element in queue [q] without removing              it from the queue, or raises [Empty] if the queue is empty. *)    
		val isEmpty: 'a t -> bool         (* [isEmpty q] returns [true] if queue [q] is empty,             otherwise returns [false]. *)   
		val isFull: 'a t -> bool         (* [isFull q] returns [true] if queue [q] is full,             otherwise returns [false]. *) 
	end;; 
																																																																																																																																																																																																																																																																																																						
																																																																																																																																																																																																				
module CyclicQueueArray:QUEUE_MUT=
	struct
		type 'a t={tab:'a option array;mutable f:int;mutable r:int} (*Rekord posiadający tablice i dwa modyfikowalne wskazniki *)
		(*f wskazuje zawsze na pierwszy element w kolejce*)
		(*r wskazuje na nastepne puste miejsce*)
		exception Empty of string
		exception Full of string
		(*TYP Option zawiera None i Some, tablica musi być stworzona i wypełniona None'ami a potem zmienia się je na Some*)
		let empty length={tab=Array.make (length+1) None;f=0;r=0}		(*Tworzy tablica pustą NONE*)
		
		let isEmpty kolejka=kolejka.f=kolejka.r
		
		(*funkcja succ inkrementuje o jeden inta*)
		let isFull kolejka= (succ kolejka.r)mod(Array.length (kolejka.tab))=kolejka.f
		
		let enqueue (value,kolejka)=if (isFull kolejka) then raise (Full "Kolejka pełna")
																else kolejka.tab.(kolejka.r)<-Some value; kolejka.r<-(succ kolejka.r)mod Array.length kolejka.tab	
																		 
					
		let dequeue kolejka=if (isEmpty kolejka) then ()
												else kolejka.tab.(kolejka.f)<-None;
														 kolejka.f<-(succ kolejka.f)mod Array.length kolejka.tab
						
		let first kolejka=if (isEmpty kolejka) then raise (Empty "Pusta kolejka")
											else match kolejka.tab.(kolejka.f) with
											| Some value->value
											| None->failwith "Cos tam"			
		
		end;;																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																											
																																																																																																																																																																																																																																																																																																						
let q=CyclicQueueArray.empty 5;;
CyclicQueueArray.enqueue(1,q);;
CyclicQueueArray.enqueue(2,q);;		
CyclicQueueArray.enqueue(3,q);;
CyclicQueueArray.isEmpty q;;																																																																																																																																																																																																																																																																																																																																																									
																																																																																																																																																																																																																																																																																																																																																																																																								
																																																																																																																																																																																																																																																																																																																																																																																																																																																									
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																										
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																												