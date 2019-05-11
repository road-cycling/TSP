open Base;;


type unionFind = {
  capacity: int;
  graph: int array;
  weights: int array;
  maxedges: int ref;
}


module UnionFind = struct 

  let empty size = {
    capacity = size;
    graph = Array.init size ~f:(fun i -> i);
    weights = Array.create ~len:size 1;
    maxedges = { contents = 0 };
  }

  let rec find structure idx = 
    match ( structure.graph.(idx) = idx ) with 
    | true -> idx 
    | false -> 
      structure.graph.(idx) <- find structure structure.graph.(idx);
      structure.graph.(idx);
    ;;

  let union structure x y : unit = 

    let root_x = find structure x in 
    let root_y = find structure y in 

    match ( root_x = root_y, structure.weights.(root_x) > structure.weights.(root_y) ) with 
    | ( true, _ ) -> ();
    | ( false, true  ) ->
          structure.weights.(root_x) <- structure.weights.(root_x) + structure.weights.(root_y);
          structure.graph.(root_y) <- structure.graph.(root_x);
          structure.maxedges := Caml.Pervasives.max !(structure.maxedges) structure.weights.(root_x);
    | ( false, false ) -> 
          structure.weights.(root_y) <- structure.weights.(root_x) + structure.weights.(root_y);
          structure.graph.(root_x) <- structure.graph.(root_y);
          structure.maxedges := Caml.Pervasives.max !(structure.maxedges) structure.weights.(root_y);
    ;;

end