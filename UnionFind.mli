type unionFind = {
  capacity : int;
  graph : int array;
  weights : int array;
  maxedges : int ref;
}
module UnionFind :
  sig
    val empty : int -> unionFind
    val find : unionFind -> int -> int
    val union : unionFind -> int -> int -> unit
  end