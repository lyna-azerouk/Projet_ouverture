

let   indice_PGSC c1  c2  =  let x1=String.length c1 and x2=String.length c2 in 
  let  tab= Array.make_matrix (x1+1) (x2+1) 0  and max= ref 0 and indice=ref [0,0] in 
  let s1  = Bytes.of_string c1  and s2= Bytes.of_string c2 in 
      for i =1 to x1 do 
        for j=1 to x2 do 
          if ( Bytes.get s1 (i-1)= Bytes.get  s2 (j-1) ) then tab.(i).(j)<-tab.(i-1).(j-1) + 1 ;
          if tab.(i).(j)>(!)max  then max:= tab.(i).(j);   indice:=[i,j];
        done ;
      done ;
   
    max 
   ;;
let c1 = "123" ;;
let c2="123" ;;

print_int (indice_PGSC c1 c2);; 



