open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 
;;

let empty_int_tree = IntLeaf;;

let rec int_insert x t = match t with
  | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (first, None, _, _, _) ->
      if first = x then t
      else if first > x then IntNode (x, Some first, IntLeaf, IntLeaf, IntLeaf)
      else IntNode (first, Some x, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (first, Some second, left, center, right) -> 
      if first = x then t else if second = x then t else
      if x < first then IntNode(first, Some second, int_insert x left, center, right) 
      else if x > second then IntNode(first, Some second, left, center, int_insert x right)
      else IntNode(first, Some second, left, int_insert x center, right)
;;

let rec int_mem x t = match t with
  | IntLeaf -> false
  | IntNode (first, None, _, _, _) -> first = x
  | IntNode (first, Some second, left, center, right) -> 
      if first = x then true else
      if first > x then int_mem x left else
      if second = x then true else
      if second < x then int_mem x right else 
      int_mem x center
;;

let rec int_size t = match t with
  | IntLeaf -> 0
  | IntNode (first, None, _, _ , _) -> 1
  | IntNode (first, Some second, left, center, right) ->
      int_size left + int_size center + int_size right + 2 
;;

let rec int_max t = match t with
  | IntLeaf -> raise(Invalid_argument("map_put"))
  | IntNode (first, None, _, _, _) -> first
  | IntNode (first, Some second, _, _, IntLeaf) -> second
  | IntNode (first, Some second, _, _, right) -> int_max right
;;

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map
;;

let empty_tree_map = MapLeaf;;


let rec map_put k v t =  match t with
  | MapLeaf -> MapNode((k, v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((fk, fv), None, _, _, _) ->
      if fk = k then raise(Invalid_argument("map_put"))
      else if fk > k then MapNode ((k, v), Some (fk, fv), MapLeaf, MapLeaf, MapLeaf)
      else MapNode ((fk, fv), Some (k, v), MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((fk, fv), Some (sk, sv), left, center, right) -> 
      if fk = k then raise(Invalid_argument("map_put")) else if sk = k then raise(Invalid_argument("map_put")) else
      if k < fk then MapNode((fk, fv), Some (sk, sv), map_put k v left, center, right) 
      else if k > sk then MapNode((fk, fv), Some (sk, sv), left, center, map_put k v right)
      else MapNode((fk, fv), Some (sk, sv), left, map_put k v center, right)
;;

let rec map_contains k t =  match t with
  | MapLeaf -> false
  | MapNode ((fk, fv), None, _, _, _) -> fk = k
  | MapNode ((fk, fv), Some (sk, sv), left, center, right) -> 
      if fk = k then true else
      if fk > k then map_contains k left else
      if sk = k then true else
      if sk < k then map_contains k right else 
      map_contains k center
;;

let rec map_get k t = match t with
  | MapLeaf -> raise(Invalid_argument("map_get"))
  | MapNode ((fk, fv), None, _, _, _) -> if fk = k then fv else raise(Invalid_argument("map_get"))
  | MapNode ((fk, fv), Some (sk, sv), left, center, right) -> 
      if fk = k then fv else
      if fk > k then map_get k left else
      if sk = k then sv else
      if sk < k then map_get k right else 
      map_get k center
;;

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = Empty | Scope of (string * int) list * lookup_table;;

let empty_table : lookup_table = Empty;;

let push_scope (table : lookup_table) : lookup_table = Scope ([], table);;

let pop_scope (table : lookup_table) : lookup_table = match table with
  | Empty -> failwith "No scopes remain!"
  | Scope (a, b) -> b
;;

let rec check_in name (table : lookup_table) = match table with
  | Empty -> false
  | Scope (a, b) -> look_helper name a b
and look_helper name a b = match a with
    | [] -> false
    | (c, d)::e -> if c = name then true else look_helper name e b
;;

let add_var name value (table : lookup_table) : lookup_table = match table with
  | Empty -> failwith "There are no scopes to add a variable to!"
  | Scope (a, b) -> if check_in name table then 
        failwith "Duplicate variable binding in scope!"
        else Scope([(name, value)]@a, b)
;;

let rec lookup name (table : lookup_table) = match table with
  | Empty -> failwith "Variable not found!"
  | Scope (a, b) -> look_helper name a b
and look_helper name a b = match a with
    | [] -> lookup name b
    | (c, d)::e -> if c = name then d else look_helper name e b
;;
