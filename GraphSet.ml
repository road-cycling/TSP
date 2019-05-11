open Core;;

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

(*  We Cannot Seal This Functor *)

module IntSet =
  struct 
      type t = int * int * int
      
      let getKey k_x k_y = ( ( k_x + k_y ) * ( k_x + k_y + 1 ) / 2 ) + k_y

      let compare ((v, e, _) : t) ((o_v, o_e, _) : t) = 
        let this = getKey v e in 
        let other = getKey o_v o_e in 
        Caml.Pervasives.compare this other
      
end 

module GraphSet = Stdlib.Set.Make(IntSet);;
module EdgeSet = Stdlib.Set.Make(Int);;
