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

(* Map function for graphs *)
let rec gmap: (context -> context) -> graph -> graph = 
  fun f g -> match g with
               | Empty -> Empty
               | Graph(c, g') -> Graph(f c, gmap f g')

(* Function to swap in-going and out-going adjacency lists for a graph *)
let swap: context -> context =
  fun c -> match c with
             | (a, idx, name, b) -> (b, idx, name, a)
 
(* Function to reverse a graph *)
let rec grev: graph -> graph =
  fun g -> match g with
	     | Empty -> Empty
             | Graph(c, g') -> Graph(swap c, grev g')
