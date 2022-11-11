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
Empty->'.'
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





let  ajout_racine e a = 
match a with
| Empty -> Node(e,[])
| Node(x,l) -> Node(x, l@[Node(e ,[Node('#',[])]) ]);; 


let rec appartien c l = match l with 
|[]-> false 
|e::q-> if (e==c ) then true  
            else  appartien c q ;;


let  rec affichier arbre =  
 let rec liste_affiche liste = 
      match liste with 
      |[]->""
      |q::t->( affichier q)^( liste_affiche t ) in 
match arbre  with 
    |Empty-> "Empty"
    |Node (e, l)->"Node(" ^Char.escaped e ^", ["  ^ liste_affiche l  ^ "]  )" ;; 

(* --------------------------------------- *)

let rec construire s  abr =  let s1= Bytes.of_string s  in  
    let rec  s_liste ss l=   let s2=Bytes.of_string ss in 
        match l with 
        |[Empty]->[Empty]
        |[]->  if (String.length ss ==1 ) then  [Node (Bytes.get s2 (0) ,[Empty])]
                                           else  [construire  ( String.sub ss  1  ((String.length ss )-1))   (Node (Bytes.get s2 (0) ,[]) ) ]
        |(q::t )-> [construire   ss  q ] @  s_liste ss t in 
match abr  with 
 Empty -> Empty                     
 |Node (e, l )-> 
                  if (Bytes.get s1 (0) == e  ) then  Node ( e, s_liste ( String.sub s  1  ((String.length s )-1))  l )

                 else Node (e ,  s_liste s l  ) ;;


 (* print_string ( affichier ( construire "ANA" (Node ('.', [Node ('#' ,[Empty]) ; Node ('B' ,[Empty])  ] ) )) );;  *) 

let  sufix chaine =  let i = ref 0  and abr = ref (Node ('.', [Node ('#' ,[Empty])] ) ) in 
   while (!i)< ((String.length chaine )-1)do 
      abr:= construire ( String.sub chaine  (!i) ((String.length chaine )-1))   ((!)abr) ;
      i:= (!i )+1 ;
    done ;
    (!) abr 
;;
print_string ( affichier ( sufix "Lyn")  );;

 (*----------------------------------------------------- *)

let element l = match l with 
[]->'.'
|(q::t)->get_1_2 q 
let  rec suite s = match s with 
[]->[]
|q::t->t 

let rec  compression abr =   
 let rec  s_liste l = match l with  
 []->[]
 |q::t-> [compression q ] @ s_liste t  in 
match  abr with
|Empty ->Empty 
|Node (e , liste)-> if (List.length liste  == 1)  then   Node ((Char.escaped e) ^  Char.escaped (element liste) , s_liste (suite liste))
                
                else   Node(Char.escaped e, s_liste liste )  ;;
 
 
 

let exemple0 =Node ('.', [Node ( '#',[]) ; Node ( 'A',[  Node ( 'N',[Node ( 'S',[Node ( 'S',[Node ( 'S',[])])])  ])  ; Node ( 'S',[ Node ( '#',[])])]) ; Node ( 'N',[ Node ( 'A',[])]) ; Node ( 'S',[])  ]) ;;    



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
      
let exemple =Node ('.', [Node ( '#',[]) ; Node ( 'A',[  Node ( 'N',[])  ; Node ( 'S',[ Node ( '#',[])])]) ; Node ( 'N',[ Node ( 'A',[])]) ; Node ( 'S',[])  ]) ;;    





(* Printf.printf "%B" (souschaine "AS#" exemple );; *)





let  rec affichier_compression arbre =  
 let rec liste_affiche liste = 
      match liste with 
      |[]->""
      |q::t->( affichier_compression q)^ ( liste_affiche t ) in 
match arbre  with 
    |Empty-> " "
    |Node (e, l)->"\n\n  " ^ e ^"\n /"  ^ liste_affiche l ;; 


let exemple1 = Node ('A',[Node('A',[Node('A',[Node('A',[])])])]) ;;

print_string ( affichier_compression ( compression exemple1 ));;





let rec to_list a = match a with
  |Empty -> []
  |Node(x,[])-> [x]
  |Node(x,l) -> x ::(List.fold_left List.append [] (List.map to_list l))
