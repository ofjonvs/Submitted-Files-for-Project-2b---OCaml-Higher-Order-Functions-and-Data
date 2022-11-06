open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
  fold (fun a x -> if x = e then true else a)
  false lst

let is_present lst x = map (fun y -> if y = x then 1 else 0) lst

let count_occ lst target = fold (fun x y -> if y = target then x + 1 else x) 0 lst

let uniq lst = fold (fun a b -> if contains_elem a b then a else b::a) [] lst

let assoc_list lst = let unlst = uniq lst in fold(fun a b -> (b, count_occ lst b)::a) [] unlst

let ap fns args = rev (fold(fun x y -> (fold(fun a b -> y b::a) x args)) [] fns)
