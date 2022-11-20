type 'a gtree =
    Empty
  | Node of ('a * ('a gtree) list)

let get_1_2  g= match g with 
Empty->' '
|Node (e ,l )->e 


let rec affichage  =  function  
    []->'v'
  |e::l-> print_char e ; print_string " " ; affichage l ;;

let rec dans e a = match a with
    Empty -> false
  | Node(v,fs)->(e=v) || (List.exists(dans e)fs) ;;

let rec to_list a = match a with
  |Empty -> []
  |Node(x,[])-> [x]
  |Node(x,l) -> x ::(List.fold_left List.append [] (List.map to_list l))


let  ajout_racine e a = match a with
  | Empty -> Node(e,[])
  | Node(x,l) -> Node(x, l@[Node(e,[])]);;

let rec sous_arbre c a = match a with 
  | Empty -> Empty
  | Node(x,[]) -> Empty
  | Node(x,Node(e,l)::q)-> if (c==e) then Node(e,l) else sous_arbre c (Node(x,q))



let rec racine a = match a with
  |Empty -> []
  |Node(x,[]) -> []
  |Node(x,[Node(y,[])]) -> [y]
  |Node(x,Node(y,l)::q) -> y :: racine (Node(x,q));;

let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

let rec concat a b suf= match a with 
Node(x,Node(y,l)::q) -> if (suf.[0]== y) then (b::q) else (Node(y,l)::concat (Node(x,q)) b suf) ;;

let rec construction suf abr =
  let x=String.length suf and i= ref 0 and abr2 = ref (Empty) and index = ref 0 in
  while ((!i)<=(x-1)) do
  let c=suf.[!i] and l= racine (!abr)  in
  if (!i== 0) 
  then (if not(List.mem c l ) then (abr:= ajout_racine c (!abr);  i:=(!i)+1) 
  else ( (!abr); i:=(!i)+1 ; index:=(!index)+1 )  )
  else if (!i== 1) 
  then ( abr2:= sous_arbre (suf.[(!i)-1]) (!abr) ;   
  if not(List.mem c (racine (!abr2)))
  then (abr2:= ajout_racine c (!abr2) ;   i:=(!i)+1 ) 
  else ((!abr); i:=(!i)+1 ; index:=(!index)+1) ) 
  else (let abr3 = ref(sous_arbre suf.[(!i)-1] (!abr2)) in 
  if not(List.mem c (racine (!abr3))) 
  then (abr2:= ajout_racine c (!abr2) ; i:=(!i)+1 ;  )
  else ((!abr); i:=(!i)+1; index:=(!index)+1))
done ;
if x>=2 then (let l = concat (!abr) (!abr2) suf in 
abr:= Node(get_1_2 (!abr),l) ; (!abr)  ) else (!abr) ;;


let arbre_suffixe s  =

  let x=String.length s and abr = ref (Node('~',[])) in 
    for j=0 to x-1 do
    let suf= String.sub s j (x-j) in 
    
    abr:= construction suf abr ;
    
    done ;

(!)abr ;;


(*let x = ref (Node('~',[Node('A',[Node('B',[Node('C',[])])]) ; Node('D',[]) ; Node('E',[Node('F',[])])])) in
x := ajout_racine 'g' (!x) ;
let z = sous_arbre 'E' (!x) in
let l2 = to_list z in affichage l2 ;*)



let x= arbre_suffixe "ANANAS#" ;;
    affichage (to_list x)



(*let x = ref(Node('~',[Node('A',[Node('N',[Node('A',[Node('N',[Node('A',[Node('S',[Node('#',[])])])])])])]) ; Node('N',[Node('A',[Node('N',[Node('A',[Node('S',[Node('#',[])])])])])])])) in  x:= sous_arbre 'N' !x ; affichage (to_list !x) ;*)


