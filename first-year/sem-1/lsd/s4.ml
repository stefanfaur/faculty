

(*2*)
let reverse = 
  let rec int_reverse acc lst = match lst with
  | [] -> acc
  | h::t -> int_reverse (h::acc) t in
  int_reverse [];;

(*3*)
let eliminate param= 
  let rec int_elim acc lst = match lst with 
  |[] -> acc
  |h::t -> if h=param then int_elim acc t else int_elim (h::acc) t in
int_elim [];;

(*4*)

let pisc n =
    if n<1 then None else
  let numbers = List.init n (fun x -> x + 1) in 
  let impare = List.filter (fun x -> x mod 2 <> 0) numbers in
  let prod = List.fold_left (fun acc x -> x + acc) 0 impare in
  Some (prod) 


  let pisc n =
    if n<1 then None else 
  let numbers = List.init n (fun x -> x + 1) in 
  let div = List.filter (fun x -> x mod 5 = 0) numbers in
  let cubes = List.map (fun x -> x*x*x ) div in
  let sum = List.fold_left (fun acc x -> acc + x) 0 cubes in
  Some (sum)
  
(*5*)

let duplicate lst = List.concat_map (fun x -> x::x::[]) lst;;

(*6*)

let div3 lst = List.for_all ( fun x -> x mod 3 = 0 ) lst;;

(*7*)

let range n = List.init n ( fun x -> x + 1 );;

let double lst = List.rev_map ( fun x -> 2 * x ) lst |> List.rev;;

let lenght lst = List.length lst;;

let maxmax lst = List.concat_map ( fun x -> x ) lst |> List.sort compare |> List.rev |> List.hd;;

let first_n = 
  let rec internal acc n lst =
    match n with
  | _ when n<0 -> None
  | 0 -> Some (List.rev acc)
  | _ -> internal ( (List.hd lst)::acc ) (n-1) (List.tl lst) in
internal[];;