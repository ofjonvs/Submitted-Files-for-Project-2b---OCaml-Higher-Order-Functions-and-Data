open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  IntLeaf -> (IntNode(x, None, IntLeaf, IntLeaf, IntLeaf))
  | IntNode(y, Some o, l, m ,r) -> 
    if x = y then IntNode(y, Some o, l, m, r)
    else if x > y then if x > o 
    then (*insert right*)IntNode(y, Some o, l, m, int_insert x r)
    else (*insert middle*)IntNode(y, Some o, l, int_insert x m, r)
    else (*insert left*)IntNode(y, Some o, int_insert x l, m, r)
  | IntNode(y, None, l, m, r) -> 
  if x = y then IntNode(y, None, l, m, r)
  else if x < y then IntNode(x, Some y, IntLeaf, IntLeaf, IntLeaf)
  else IntNode(y, Some x, IntLeaf, IntLeaf, IntLeaf)

let rec int_mem x t =
  match t with
  IntLeaf -> false
  |IntNode(y, Some o, l, m, r) -> if x = y then true else if x = o then true else 
  if x > y then if x > o then int_mem x r
  else int_mem x m
  else int_mem x l
  |IntNode (y, None, l, m, r) -> if x = y then true else false

let rec int_size t =
  match t with
  IntLeaf -> 0
  |IntNode(y, Some o, l, m , r) -> 2 + int_size l + int_size m + int_size r
  |IntNode(y, None, l, m, r) -> 1


let rec int_max t =
  if int_size t = 0 then invalid_arg "int_max" else
  match t with
  IntLeaf -> 0
  |IntNode(y, Some o, l, m, r) -> if r = IntLeaf then o else int_max r
  |IntNode(y, None, l, m, r) -> y


(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t =
  match t with
  MapLeaf -> (MapNode((k, v), None, MapLeaf, MapLeaf, MapLeaf))
  | MapNode((x, y), Some (o, z), l, m ,r) -> 
    if k = x then invalid_arg "map_put"
    else if k > x then if k > o 
    then (*insert right*)MapNode((x, y), Some (o,z), l, m, map_put k v r)
    else (*insert middle*)MapNode((x, y), Some (o,z), l, map_put k v m, r)
    else (*insert left*)MapNode((x, y), Some (o,z), map_put k v l, m, r)
  | MapNode((x, y), None, l, m, r) -> 
  if k = x then invalid_arg "map_put" 
  else if k < x then MapNode((k, v), Some (x, y), MapLeaf, MapLeaf, MapLeaf)
  else MapNode((x, y), Some (k, v), MapLeaf, MapLeaf, MapLeaf)

let rec map_contains k t = 
  match t with
  MapLeaf -> false
  |MapNode((x, y), Some (o,z), l, m, r) -> if k = x then true else if k = o then true else 
  if k > x then if k > o then map_contains k r
  else map_contains k m
  else map_contains k l
  |MapNode ((x, y), None, l, m, r) -> if k = x then true else false

let rec map_get k t =
  match t with
  |MapLeaf -> invalid_arg "map_get"
  |MapNode((x, y), Some (o,z), l, m , r) -> 
    if k = x then y else if k = o then z
    else if k > x then if k > o then map_get k r
    else map_get k m
    else map_get k l
  |MapNode((x, y), None, l, m , r) -> if k = x then y else invalid_arg "map_get"

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table =
|Empty
|OuterTable of lookup_table * (string * int) list
|InnerTable of (string * int) list * lookup_table

let empty_table : lookup_table = Empty

let push_scope (table : lookup_table) : lookup_table = 
  match table with
  Empty -> InnerTable([], Empty)
  |InnerTable(a, e) -> OuterTable(table, [])
  |OuterTable(inner, b) -> OuterTable(OuterTable(inner, b), [])

let pop_scope (table : lookup_table) : lookup_table =
  match table with
  Empty -> failwith "No scopes remain!"
  |InnerTable(a, e) -> Empty
  |OuterTable(InnerTable(a, e),b) -> InnerTable(a, Empty)
  |OuterTable(inner, a) -> inner


let contains_name lst name = 
  fold (fun a (n, v) -> if n = name then true else a)
  false lst  

let add_var name value (table : lookup_table) : lookup_table =
  match table with
  Empty -> failwith "There are no scopes to add a variable to!"
  |InnerTable(a, e) -> 
    if contains_name a name then
    failwith "Duplicate variable binding in scope!"
    else InnerTable((name, value)::a, Empty)
  |OuterTable(inner, a) -> 
    if contains_name a name then
      failwith "Duplicate variable binding in scope!"
      else OuterTable(inner, (name, value)::a)

  let ret_value lst name = 
    fold (fun a (n, v) -> if n = name then v else a)
    1 lst  

let rec lookup name (table : lookup_table) =
  match table with
  InnerTable(a, e) -> if contains_name a name then 
                      ret_value a name
                      else failwith "Variable not found!"
  |OuterTable(inner, a) -> if contains_name a name then
                            ret_value a name
                            else lookup name inner
  |Empty ->  (failwith "Variable not found!")
