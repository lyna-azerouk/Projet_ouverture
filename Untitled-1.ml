let   indice_PGSC c1  c2  =  let x1=String.length c1 and x2=String.length c2 in 
  let  tab= Array.make_matrix (x1+1) (x2+1) 0  and max= ref 0 and  fin1=ref 0 and fin2=ref 0  in 
  let s1  = Bytes.of_string c1  and s2= Bytes.of_string c2 in 
      for i =1 to x1 do 
        for j=1 to x2 do 
          if ( Bytes.get s1 (i-1)= Bytes.get  s2 (j-1) ) then tab.(i).(j)<-tab.(i-1).(j-1) + 1 ;
           
          if tab.(i).(j)>(!)max  then    (max:= tab.(i).(j);  fin1:= i; fin2 :=j); 
          
        done ;
       
      done ;
     
   [ (!) fin1 -(!)max +1;  (!) fin2 -(!)max +1; (!)max]
   ;;
let c1 ="D'abord confinée dans les monastères et limitée essentiellement au domaine religieux, la traduction s'étend au domaine profane dès le Xe  siècle. Bientôt apparaissent en langue romane des fabliaux, comédies ou romans imités d'oeuvres de l'Antiquité, et les premiers grands poètes - Chrétien de Troyes, Marie de France, Rutebeuf, Jean de Meung - sont avant tout traducteurs (à une époque où traduction, imitation et création ne sont guère différenciées). Ce n'est toutefois qu'au XIVe siècle, marqué par la création des premières universités, que la traduction quitte les monastères et connaît un bref essor grâce à la protection de la cour.";;
let c2="D'après Nama et al., la traduction a joué un rôle important dans l'évolution de la langue française. D'abord confinée dans les monastères et limitée essentiellement au domaine religieux, la traduction s'étend au domaine profane dès le Xe  siècle. Bientôt apparaissent en langue romane des fabliaux, comédies ou romans imités d'oeuvres de l'Antiquité, et les premiers grands poètes - Chrétien de Troyes, Marie de France, Rutebeuf, Jean de Meung - sont avant tout traducteurs (à une époque où traduction, imitation et création ne sont guère différenciées). Ce n'est toutefois qu'au XIVe siècle, marqué par la création des premières universités, que la traduction quitte les monastères et connaît un bref essor grâce à la protection de la cour. Pendant ces quelques siècles, la réflexion sur la traduction comme pratique évolue elle aussi.
" ;;


let rec affichage  =  function  
[]->()
|e::l-> print_int e ; print_string " " ; affichage l ;;

let a =indice_PGSC c1 c2;; 
print_string ("Dans l'ordre ( indcie c1, indice c2 , longeur maximale de c1 et c2 )===>" );;
affichage a ;;
let s1  = Bytes.of_string c1 ;;



let time f x y =
    let t = Sys.time() in
    let fx = f x y in
    Printf.printf "\nExecution time: %fs\n" (Sys.time() -. t);
    fx ;;
time indice_PGSC c1 c2;;    







