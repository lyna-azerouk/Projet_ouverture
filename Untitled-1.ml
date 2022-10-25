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
let c1 ="éééééé";;
let c2="éééééé" ;;


let rec affichage  =  function  
[]->()
|e::l-> print_int e ; print_string " " ; affichage l ;;

let a =indice_PGSC c1 c2;; 
affichage a ;;





