(* _______________________________________________________________________ *)
type 'a gtree =
    Empty
  | Node of ('a * ('a gtree) list)
(* ________________________________________________________________________*)
(*PRMITIVES *)
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

let get_2_2 abr = match abr with 
|Empty -> []
|Node (e,l)->l


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
[]->print_string " "
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

(* ----------------------------------------------------------------------------- *)
(* construction de l'abre des sufixes *)
let  rec suite s = match s with 
[]->[]
|q::t->t ;;


let rec construire s  abr =  let s1= Bytes.of_string s  in  
    let rec  s_liste ss l=   let s2=Bytes.of_string ss in 
        match l with 
        |[Empty]->[]
        |[]->  if (Bytes.get s2 (0) =='#'  &&  String.length ss ==1 ) then  [Node('#',[])] 
                                              else   [construire  ( String.sub ss  1  ((String.length ss )-1))   (Node (Bytes.get s2 (0) ,[]) ) ]
        |(q::t )-> [construire   ss  q ] @  s_liste ss t in 
match abr  with 
 Empty -> Empty                   
 |Node (e, l )-> 
                  if (Bytes.get s1 (0) == e &&  String.length s  >1  )   then
                  Node ( e, s_liste ( String.sub s  1  ((String.length s )-1)) ( l ))
                    
                 else  if ( e=='#' ) then Node('#',[] ) else   Node (e ,  s_liste s l)   ;;


 (* print_string ( affichier ( construire "N" (Node ('.', [Node ('#' ,[Empty]) ; Node ('B' ,[Empty])  ] ) )) );;     *)
 print_string ("\n");;


let  sufix chaine =  let  abr = ref (Node ('.', [Node ('#' ,[])] ) ) in 
   for i =0 to  (String.length chaine )-1 do 
      abr:= construire ( String.sub chaine   i ((String.length chaine ) -i))   ((!)abr) ;
     
    done ;
    (!) abr 
;;

(* ----------------------------------------------------------------------------- *)

 

let exemple0 =Node ('.', [Node ( '#',[]) ; Node ( 'A',[  Node ( 'N',[Node ( 'S',[Node ( 'S',[Node ( 'S',[])])])  ])  ; Node ( 'S',[ Node ( '#',[])])]) ; Node ( 'N',[ Node ( 'A',[])]) ; Node ( 'S',[])  ]) ;;    

(* ----------------------------------------------------------------------------------*)
(*Sous_chaine    *)
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
                         s_liste (String.sub s  1  ((String.length s )-1))  l   
                    else  
                       s_liste s  l 
                    ;;
      
let exemple =Node ('.', [Node ( '#',[]) ; Node ( 'A',[  Node ( 'N',[Node('#',[])])  ; Node ( 'S',[ Node ( '#',[])])]) ; Node ( 'N',[ Node ( 'A',[Node('#',[])])]) ; Node ( 'N',[])  ]) ;;  
let c1 ="D'abord confinée dans les monastères et limitée essentiellement au domaine religieux, la traduction s'étend au domaine profane dès le Xe  siècle. Bientôt apparaissent en langue romane des fabliaux, comédies ou romans imités d'oeuvres de l'Antiquité, et les premiers grands poètes - Chrétien de Troyes, Marie de France, Rutebeuf, Jean de Meung - sont avant tout traducteurs (à une époque où traduction, imitation et création ne sont guère différenciées). Ce n'est toutefois qu'au XIVe siècle, marqué par la création des premières universités, que la traduction quitte les monastères et connaît un bref essor grâce à la protection de la cour.#";;

let time f x y  =
    let t = Sys.time() in
    let fx = f x (sufix y) in
    Printf.printf "\nExecution time: %fs\n" (Sys.time() -. t);
    fx ;;
time souschaine "ANAN#"  "ANANAS#" ;;   

(* ___________________________________________________________________________________*)
(* sous chaine commune *)
let exemple1 =  Node ( ['.' ],  [Node(['#'; 'V' ;'B' ] ,[Node(['C'; 'V' ;'B'],[Node(['D'  ; 'V' ; 'B' ],[Node(['#' ],[])])])])   ; Node (['R' ; 'V' ; 'B' ],[Node(['#' ],[])]) ])  ;; 
let exemple3 =Node ( ['.'], [Node ( ['#'],[]) ; Node ( ['A' ],[  Node ( ['N'  ],[Node(['A' ],[])])  ; Node ( ['S' ; 'R'],[ Node ( ['#'],[])])]) ; Node ( ['N' ;'R' ;'V' ],[ Node ( ['A' ; 'R' ; 'V'],[Node(['#'],[])])]) ; Node ([ 'N' ; 'R' ; 'V'],[])  ]) ;;  

(* l'arbre des deux chaine (chaine1 et chaine 2 ) *)
let rec   souschaine_commune  arbre = 
          let  rec s_liste l = match l with 
          []-> " " 
          |q::t-> if (String.length (souschaine_commune q ) > String.length ( s_liste t) ) then souschaine_commune q 
                                                                                           else  s_liste t
           in 
match arbre with 
Empty->" "
|Node(e ,liste)-> if (List.length  e == 3 ) then  (( Char.escaped ( List.hd e ))^ s_liste liste )
                                         else s_liste liste ;;



print_string ("La plus longue sous chaine commune dans l'arbre exemple3 =>"^souschaine_commune  exemple3);;
 
(* _________________________________________________________________________________ *)
(* compression -------------------------------------------------------- *)


let rec ft liste = match liste with 
|[]->" " 
|q::t-> if ( List.length  (get_2_2 q)  ==1 )then  Char.escaped  (get_1_2 (q)) ^  ft  (get_2_2 q)     else   Char.escaped  ( get_1_2 (q))

let rec  compression abr =   
 let rec  s_liste l = match l with  
 []->[]
 |q::t-> [compression q ] @ s_liste t  in 
match  abr with
|Empty ->Empty 
|Node (e , liste)-> if (List.length liste  == 1)  then   Node ((Char.escaped e) ^  (ft liste) , s_liste ( suite liste))
                
                else   Node(Char.escaped e, s_liste liste )  ;;
 
;;


let  rec affichier_compression arbre =  
 let rec liste_affiche liste = 
      match liste with 
      |[]->""
      |q::t->( affichier_compression q)^ ( liste_affiche t ) in 
match arbre  with 
    |Empty-> " "
    |Node (e, l)->"Node ( " ^ e ^", ["  ^ liste_affiche l ^"] )" ;; 




 (* print_string ( affichier_compression ( compression exemple1 ));; *) 


(*TEST -------------------------------------------- *)

print_string("\n");;
print_string("la sous chaine (ANAN#) existe dans l'arbre ananas ?");;
let donne =sufix "ANANAS#" ;;
 Printf.printf "%B" ( souschaine "ANAN#"  donne );;  

print_string("\n");;





