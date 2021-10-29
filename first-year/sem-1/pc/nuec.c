
let rec ab3r2 n = if n = 0 then 3 else 2 + ab3r2 (n-1);;
#trace ab3r2;;


let rec ap3r2 indice = match indice with
| 0 -> 3
| n -> 2 + ap3r2 (n-1);;

let pare x = match x with
| 0 -> "null"
| n when n mod 2 = 0 -> "par"
| m -> "impar";;

let rec pare2 = function
| 0 -> "null"
| n when n mod 2 = 0 -> "par"
| m -> "impar";;

let pozitie coord = match coord with
| (0,0) -> "origine"
| (0,y) -> "axa y: " ^ (string_of_int y)
| (_,0) -> "axa x"
| (_,_) -> "nu e pe axe";;

let doubleplus1 x = 
  let dublu y = 2 * y in 
    dublu x + 1;;
    

    let doublextripleyplus1 x y = 
      let dublu t = 2 * t in 
      let triplu r = 3 * r in 
        dublu x + triplu y + 1;;


let f n = if n mod 2 = 0 then n/2 else 3 * n + 1;;
let rec p n = if n = 1 then 0 else 1 + p((f n));;


let rec eval = function
| I i -> i
| Add (e1, e2) -> eval e1 + eval e2
| Sub (e1, e2) -> eval e1 - eval e2
| Mul (e1, e2) -> eval e1 * eval e2
| Div (e1, e2) -> eval e1 / eval e2;;