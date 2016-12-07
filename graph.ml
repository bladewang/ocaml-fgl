(* Graph as an Inductive Data Structure *)
type node_index     = int
type node_name      = string
type edge_label     = string

type adjacency_list = (edge_label * node_index) list
type context        = (adjacency_list * node_index * node_name * adjacency_list)

type graph          = Empty | Graph of (context * graph)

(* Function to represent an adjacency list as a string *)
let rec string_of_adj_list: adjacency_list -> string =
  fun a -> match a with 
             | [] -> ""
             | (label, idx)::tail -> match tail with 
		                       | [] -> "(" ^ label ^ "," ^ string_of_int idx ^ ")"  
				       | _ -> "(" ^ label ^ "," ^ string_of_int idx ^ ")" ^ "," ^ string_of_adj_list tail
                                     

(* Function to represent a context as a string *)
let string_of_context: context -> string =
  fun c -> match c with 
             | (a, idx, name, b) -> "([" ^ string_of_adj_list a ^ "]," ^ string_of_int idx ^ "," ^ name ^ ",[" ^ string_of_adj_list b ^ "]) & "

(* Function to represent a graph as a string *)
let rec string_of_graph: graph -> string =
  fun g -> match g with
             | Empty -> "Empty"
             | Graph(c, g') -> string_of_context c ^ string_of_graph g'

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

(* Example mapping function *)
let map : context -> context = 
  fun ctx -> match ctx with
	     |(a, idx, name, b) -> (a, idx, "foo-" ^ name, b) 

(* Unordered Fold Function for graphs *)
let rec ufold: (context -> 'tau -> 'tau) -> 'tau -> graph -> 'tau =
  fun f z g -> match g with
                 | Empty -> z
                 | Graph(c, g') -> f c (ufold f z g')

let rec fold: context -> int -> int = 
  fun ctx acc -> match ctx with
                |(a, idx, name, b) -> idx + acc 

(* Function to swap in-going and out-going adjacency lists for a graph *)
let swap: context -> context =
  fun c -> match c with
             | (a, idx, name, b) -> (b, idx, name, a)

(* Function to reverse a graph *)
let rec grev: graph -> graph =
  fun g -> match g with
	     | Empty -> Empty
             | Graph(c, g') -> Graph(swap c, grev g')

(* Example Usage *)
let example_print =
  let g = Graph(([("left", 2);("up", 3)], 2, "c", [("right", 2)]), Graph(([("right", 1)], 2, "b", [("left", 1)]), Graph(([], 1, "a", []), Empty))) in

  let string_of_g = string_of_graph g in
  print_string ("Graph: " ^ string_of_g ^ "\n");
  print_string ("Is Empty: " ^ string_of_bool (is_empty g) ^ "\n");
  print_string ("Graph Map: " ^ string_of_graph (gmap map g) ^ "\n");
  print_string ("Graph Fold: " ^ string_of_int (ufold fold 0 g) ^ "\n");
  print_string ("Graph Reverse: " ^ string_of_graph (grev g) ^ "\n")
 
