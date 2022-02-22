open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)
let is_present lst x = map (fun y -> if x = y then 1 else 0) lst;;

let count_occ lst target =   
    let ls = is_present lst target in 
    fold (fun acc x -> acc + x) 0 ls
;;

let contains_elem lst e = if count_occ lst e > 0 then true else false;;

let uniq lst = fold (fun a x -> if contains_elem a x then a else x::a) [] lst;;

let assoc_list lst = uniq (map (fun y -> (y, (count_occ lst y))) lst);;

let ap fns args = fold (fun a f -> a@(map f args)) [] fns;;
