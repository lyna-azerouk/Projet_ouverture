let rec to_list a = match a with
  |Empty -> []
  |Node(x,[])-> [x]
  |Node(x,l) -> x ::(List.fold_left List.append [] (List.map to_list l))
