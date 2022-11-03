type 'a gtree =
    Empty
  | Node of ('a * ('a gtree) list)

let rec  arbre_vide abr =  match abr with 
  Empty -> true 
  | a-> false 
  ;;

let max a b = if a >b then a else b ;;                   
                    
let rec height arb =
  let rec  height_liste l=
    match l with 
    | []->0 
    |(q::t )->max  (height q ) (height_liste t ) in 
  match arb with 
  |Empty -> 0 
  |Node (_,[])-> 1
  |Node (_,l)->1+height_liste l 
  ;;

(* fonction qui  retourne la liste des lettre a la racine *) 

let get_1_2  g= match g with 
Empty->0
|Node (e ,l )->e 

let  liste_lettre abr  = 
    let rec liste_racine l = match l with 
    |[]->[]
    |q::t-> (get_1_2 q ):: liste_racine t  in 
   match abr with 
    |Empty->[]
    |Node(e,l ) -> liste_racine l 
    ;;

;;

let rec affichage  =  function  
[]->()
|e::l-> print_int e ; print_string " " ; affichage l ;;


let x = Node (1, [Node ( 2,[]) ; Node ( 4,[]) ]) ;;
let resultat_lettre_racine =  liste_lettre x ;;

 affichage resultat_lettre_racine ;;



let  ajout_racine e a = 
match a with
| Empty -> Node(e,[])
| Node(x,l) -> Node(x, l@[Node(e ,[]) ]);; 




let rec souschaine  s abr =  let s1= Bytes.of_string s  in  
    let rec  s_liste ss l=
        match l with 
        | []->false  
        |(q::t )->  souschaine  ss q  ||   s_liste ss t  in 
match abr  with 
 Empty -> false                                       
 |Node (e, l )-> if (Bytes.get s1 (0) == e   )  then 
                        if (String.length s == 1  && Bytes.get s1 (0)=='#' ) then  true
                        else 
                         (print_string ( String.sub s  1  ((String.length s )-1)) ; s_liste (String.sub s  1  ((String.length s )-1))  l  )  
                    else  
                       s_liste s  l 
                    ;;
      
let exemple =Node ('.', [Node ( '#',[]) ; Node ( 'A',[  Node ( 'N',[])  ; Node ( 'S',[ Node ( '#',[])])]) ; Node ( 'N',[]) ; Node ( 'S',[])  ]) ;;     


Printf.printf "%B" (souschaine "AS#" exemple );; 


