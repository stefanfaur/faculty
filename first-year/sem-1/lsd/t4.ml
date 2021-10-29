
(*1*)

let primes n = 

  let prime_check x =
    let rec int_prime acc vl = match vl with 
    | 0 -> if acc <= (x/2) && acc > 1 then ( if x mod acc = 0 then int_prime (acc-1) (vl+1) else int_prime (acc-1) vl ) else true
    | _ -> false in
  int_prime (x/2) 0 in

  List.init n ( fun x -> x + 1 ) |> List.filter ( prime_check );;

  let asDecimal =
    let rec internal acc binary_list = match binary_list with 
    | [] -> acc
    | h::t ->  if h = 1 then internal ( acc + int_of_float( 2. ** float_of_int(List.length binary_list - 1) ) ) t
                else internal acc t
  in internal 0;;


 let last_opt lst = match lst with
 | [] -> None
 | lst ->
    let rec last_int acc lst = match lst with 
    | [] -> Some (acc)
    | h::t -> last_int h t in 
    last_int 0 lst;;

    let isAscending lst = match lst with
    | [] | _::[] -> None
    | _ :: t -> 
      let front = List.rev lst |> List.tl |> List.rev in 
      let pairs = List.map2 ( fun x1 x2 -> (x1,x2) ) front t in 
      let result = List.for_all ( fun (x1,x2) -> x1<=x2 ) pairs in 
      Some(result);;