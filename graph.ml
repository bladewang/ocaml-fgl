(* Graph as an Inductive Data Structure *)
type node_index     = int
type node_name      = string
type edge_label     = string

type adjacency_list = (edge_label * node_index) list
type context        = (adjacency_list * node_index * node_name * adjacency_list)

type graph          = Empty | Graph of (context * graph)

(* Function to check if the graph is empty or not *)
let is_empty: graph -> bool =
  fun g -> match g with
             | Empty -> true
             | _ -> false
