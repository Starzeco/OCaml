type  'a  llist  =  LKoniec  |  LElement  of  'a  *  (unit  ->  'a  llist);; 
type  'a  nlist  =  Koniec|  Element  of  'a  *  ('a  nlist);;

let rec lfrom number=LElement(number,function ()->lfrom (number+1));;

let llist1=lfrom 2;;

let rec ltake (number,llist)=match (number,llist) with
| (0,_)->[]
| (_,LKoniec)->[]
| (number,LElement(x,xf))->x::ltake(number-1,xf());;

ltake (6,llist1);;
(*DZIELI PO INDEKSIE*)

(*PODZIEL*)
let podziel lista=
	let rec helper (list,i)=match (list,i) with
	|(Koniec,_)->(Koniec,Koniec)
	| (Element(value,tail),ii)->if ii mod 2=0 then (Element(value,(fst (helper(tail,ii+1)))),(snd (helper(tail,ii+1))))
	else((fst (helper(tail,ii+1)),Element(value,(snd (helper(tail,ii+1))))))
		 

	in helper(lista,0);;

let llist3=Element(2,Koniec);;
let llist2=Element(1,Element(2,Element(3,Element(4,Element(5,Koniec)))));;


podziel llist3;;
podziel llist2;;

(*LPODZIEL*)
let lpodziel lista=
	let rec lhelper (list,i)=match (list,i) with
	| (LKoniec,_)->(LKoniec,LKoniec)
	| (LElement(value,xf),ii)->if ii mod 2=0 then (LElement(value,function()->(fst (lhelper(xf(),ii+1)))),(snd (lhelper(xf(),ii+1))))
	else (fst (lhelper(xf(),ii+1)),LElement(value,function()->(snd (lhelper(xf(),ii+1)))))
	
	in lhelper(lista,0);;


let llistInfi=LElement(4,function ()->LElement(2,function()->LElement(7,function()->LKoniec)));;

ltake (2, fst (lpodziel llistInfi));;


(*LISTA 6*)
(*let heado lista=match lista with
| Koniec->(-1)
| Element(value,_)->value;;


let powielo l=
	let rec powielH (listka,n)=match (listka,n) with
	| (Koniec,_)->Koniec
	| (Element(value,tail),k)->if k=0 then powielH (tail,(heado tail)) 
	   else Element(value,powielH(listka,(value-1)))
	
	
	in powielH (l,(heado l));; *)
	
	let powiel l =
  let rec inPowiel = function
      ([], _) -> []
    | (_::t, 0) -> if t = [] then [] else inPowiel (t, List.hd t)
    | (h::t as li, n) -> h::inPowiel (li, n-1)
  in if l = [] then [] else inPowiel(l, List.hd l);;




powiel [1;2;3];; (* [1;2;2;3;3;3] *)
powiel [0;1;2];; (* [1;2;2] *)
powiel [];;




(*Powielenie leniwe*)

(*let headL list=match list with
| LKoniec->(-1)
| LElement(value,_)->value;;

let lpowiel list=
	let rec lpowielH (l,n)=match (l,n) with
	| (LKoniec,_)->LKoniec
	| (LElement(value,xf),k)->if k=0 then lpowielH (xf(),(headL (xf()))) 
	else LElement(value,function ()->lpowielH (l,(value-1))) 

	in lpowielH (list,(headL list));; *)

let lpowiel2 = function
    LKoniec -> LKoniec
  | LElement(v, nl) as el -> 
      let rec inLPowiel = function 
          (LKoniec, _) -> LKoniec
        | (LElement(_, nl), 0) -> (
            match nl() with
                LKoniec -> LKoniec
              | LElement(v, nnl) as ll -> inLPowiel(ll, v)
          )
        | (LElement(v, nl) as ll, n) -> LElement(v, fun () -> inLPowiel (ll, n-1))
      in inLPowiel(el, v);;


ltake (13,(lpowiel2 llistInfi));; 




(*INSERT*)

let rec insert xs x =
  match xs with
      [] -> [x]
    | h::t as l -> 
        if h < x then h::insert t x
        else x::l;;

let list = [1;3;5;7];;
insert list 4;; (* [1;3;4;5;7] *)
insert list 0;; (* [0;1;3;5;7] *)
insert list 8;; (* [1;3;5;7;8] *)
insert list 3;; (* [1;3;3;5;7] *)

	
	
(*ZADANIE 3*)

type rasa=Ogien|Woda|Powietrze|Ziemia;;
type skill=Plomyk|Strumien|Podmuch|Kwiatek;;

type llazy=Combo of rasa*(unit->skill);;

let wodny=Combo(Ogien,fun()->Plomyk);;
let ognisty=Combo(Woda,fun()->Strumien);;
let powietrze=Combo(Powietrze,fun()->Podmuch);;
let ziemia=Combo(Ziemia,fun()->Kwiatek);;



(*ZADANIE 4*)

type llisto=LKoniecc |Node of int*(unit->llisto);;


let rec lazlist pred=match lazlist with
| LKoniecc->Nil
| Node(x,xf)->if pred x then x else lazylist (xf()) pred;;


