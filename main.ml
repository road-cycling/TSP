open UnionFind;;
open Core;;
open Base;;
open GraphSet;;
open Stdio;;
(* dune build main.exe && ./_build/default/main.exe *)


let createMST heap ~size:size = 
  let uf = UnionFind.empty !size in 
  let rec mst heap uf = 
    match (Heap.pop heap) with 
    | Some (v_1, v_2, weight) -> (
      match (UnionFind.find uf v_1 <> UnionFind.find uf v_2) with 
      | true -> UnionFind.union uf v_1 v_2; (v_1, v_2, weight)::mst heap uf 
      | false -> mst heap uf
    )
    | None -> []
  in mst heap uf;;

let find_bfs_start_idx (graph : int array array) = 
  let rec r idx = 
  if idx = Array.length graph then None else 
  match Array.length graph.(idx) with 
  | 0 -> r (idx + 1)
  | _ -> Some (idx)
  in r 0;;

let graphFold graph = 
  GraphSet.fold (fun v accum -> v::accum) graph []
  
let bfs_graph (graph : int array array) =
  let set = EdgeSet.empty in 
  let rec recurse set idx init = 
    Array.fold_right graph.(idx) ~f:(fun idx init -> 
      match (EdgeSet.mem idx set) with 
      | true -> init 
      | false -> init @ idx :: recurse (EdgeSet.add idx set) idx []
    ) ~init:init
  in match find_bfs_start_idx graph with 
  | None -> []
  | Some (v) -> (recurse (EdgeSet.add v set) v [v]) @ [v];;

let rec fold_result mat lst =
  match lst with 
  | f::s::t -> 
    mat.(f).(s) + fold_result mat (s::t)
  | _ -> 0;
;;

let recreateMSTGraph mst ~lines:lines = 
  let matrix = Array.make_matrix ~dimx:!lines ~dimy:0 0 in 
  List.iteri mst ~f:(
    fun _ (edge_1, edge_2, _) -> 
      matrix.(edge_1) <- Array.append matrix.(edge_1) [| edge_2 |];
      matrix.(edge_2) <- Array.append matrix.(edge_2) [| edge_1 |];
  );
  matrix;;

(* 🙄 need a matrix to reconstruct value ffs - didn't think of that when i started *)
let createMatrix lines =
  let matrix = Array.make_matrix ~dimx:!lines ~dimy:0 0 in 
   List.iteri (In_channel.read_lines "matrix.txt") ~f:(fun row_idx row -> 
    matrix.(row_idx) <- (List.fold_right 
      (String.split ~on:' ' row) 
      ~f:(fun v accum -> Array.append [|Int.of_string v|] accum ) 
      ~init:[||]
      )
   );
   matrix;;

let main = 
  let lines = ref 0 in
  let graph = List.foldi (In_channel.read_lines "matrix.txt") ~init:[]
    ~f:(fun row_idx first_accum row -> 
      lines := !lines + 1; first_accum @ List.foldi (String.split ~on:' ' row) ~init:[] 
      ~f:(fun col_idx accum weight -> (row_idx, col_idx, Int.of_string weight)::accum))
  (* |> List.fold_right ~f:GraphSet.add ~init:GraphSet.empty *)
  (* |> graphFold  *)
  |> List.filter ~f:(fun (_, _, v) -> v <> 0)
  |> Heap.of_list ~cmp:(fun (_, _, w) (_, _, o_w) -> Caml.Pervasives.compare w o_w) 
  |> createMST ~size:lines
  |> recreateMSTGraph ~lines:lines
  |> bfs_graph in 
  List.iter graph ~f:(fun v -> print_endline (Int.to_string v));
  Core.print_endline ("Cost of Walk: -> " ^(Int.to_string (fold_result (createMatrix lines) graph)));
  ;;



(* let create_cycles (sparse_matrix: int array array) = 
  Array.iteri sparse_matrix ~f:(fun x row -> 
    Array.iter row ~f:(fun v -> 
      match ( Array.mem sparse_matrix.(v) ~equal:(=) x ) with 
      | true -> ()
      | false -> sparse_matrix.(v) <- Array.append sparse_matrix.(v) [| x |];
    )
  );; *)


(* 2  1  6              
4  5  8
4  1  9
3  1  9
4  0  10 *)

(* let t = [|
  (* 0 *) [||];
  (* 1 *) [||];
  (* 2 *) [|1|];
  (* 3 *) [|1|];
  (* 4 *) [|0; 1; 5;|];
  (* 5 *) [||];
|]

let g = [|
  (* 0 *) [|4;|];
  (* 1 *) [|2; 3; 4;|];
  (* 2 *) [|1;|];
  (* 3 *) [|1;|];
  (* 4 *) [|0;1;5;|];
  (* 5 *) [||];
|]

let h = [|
  (* 0 *) [||];
  (* 1 *) [||];
  (* 2 *) [||];
  (* 3 *) [||];
  (* 4 *) [||];
  (* 5 *) [||];
|]



 *)

(* 0 3 2 5
3 0 5 4
2 5 0 6
5 4 6 0

0 10 14 11 10 15
10 0 6 9 9 10
14 6 0 9 13 11
11 9 9 0 15 16
10 9 13 15 0 8
15 10 11 16 8 0 *)

(* [|
  [|0; 10; 14; 11; 10; 15; |];
  [|10; 0; 6; 9; 9; 10; |];
  [|14; 6; 0; 9; 13; 11; |];
  [|11; 9; 9; 0; 15; 16; |];
  [|10; 9; 13; 15; 0; 8; |];
  [|15; 10; 11; 16; 8; 0; |];
|]

let result  = [0; 4; 5; 1; 3; 2; 0] *)

