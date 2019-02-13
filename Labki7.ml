module Tree=
	struct
		type 'a bt = Leaf | Node of 'a * 'a bt * 'a bt
  	type 'a t = { mutable root: int bt }
  	exception NotFound of string
		
		let create()={root=Leaf}
		
		let push (el,sT)=
			let rec helpPush =function
			| Node(value,left,right)->if el>=value then Node(value,left,helpPush right) 
														else Node(value,helpPush left,right)
			| Leaf->Node(el,Leaf,Leaf)
		
		
		in sT.root<-helpPush sT.root
		
		let pushEl (el,sT)=if el mod 2=0 then push(el,sT) else failwith "Niepatzysty element" 
		
		let remove (el, sT) =
    	let rec inRemove iEl = function
        Node(v, lt, rt) as toRemove ->
          if iEl < v then Node(v, inRemove iEl lt, rt)
          else if iEl > v then Node(v, lt, inRemove iEl rt)
          else (match toRemove with
                 | Leaf -> failwith "Not happenin"
                 | Node(_, Leaf, Leaf) -> Leaf
                 | Node(_, lt, Leaf) -> lt
                 | Node(_, Leaf, rt) -> rt
                 | Node(_, lt, rt) -> 
                     let rec minValue = function
                         Leaf -> failwith "Not happenin"
                       | Node(min, Leaf, _) -> min
                       | Node(_, lST, _) -> minValue lST
                     in let suc = minValue rt 
                     in Node(suc, lt, inRemove suc rt)
               )
      | Leaf -> raise (NotFound "module SearchTree: remove")
    	in sT.root <- inRemove el sT.root

		let find (el, sT) =
   		 let rec inFind = function
        Node(v, lt, rt) -> 
          if el < v then inFind lt
          else if el > v then inFind rt
          else true
      	| Leaf -> false
    in inFind sT.root

  let getPreOrder sT = 
    let rec preorder = function
        Leaf -> []
      | Node(v, lt, rt) -> v::(preorder lt @ preorder rt)
    in preorder sT.root

  let getPostOrder sT = 
    let rec postorder = function
        Leaf -> []
      | Node(v, lt, rt) -> postorder lt @ postorder rt @ [v]
    in postorder sT.root

  let getInOrder sT =
    let rec inorder = function
        Leaf -> []
      | Node(v, lt, rt) -> inorder lt @ (v :: inorder rt)
    in inorder sT.root
	

		
	end;;	



let tree=Tree.create();;
Tree.pushEl (5,tree);;
Tree.pushEl (1,tree);;
Tree.pushEl (4,tree);;
Tree.pushEl (7,tree);;
Tree.pushEl (6,tree);;

Tree.getInOrder tree;;

Tree.remove (4,tree);;

Tree.getInOrder tree;;