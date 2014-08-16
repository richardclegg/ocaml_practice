open Printf
open List

let rec f1 contents =
    map (fun (k,vl) -> (k, List.fold_left (+) 0 vl)) contents;;

(*using folds alone*)
let fold_f1 contents : (string * int) list =
List.fold_right
(fun (key, value) ac ->
 (key, List.fold_right (+) value 0) :: ac)
contents []

(*using map*)
let map_f1 contents : (string * int) list =
List.map
(fun (key, value) ->
 (key, List.fold_right (+) value 0))
contents 

let rec print_list contents =
    List.iter (fun item -> let (x,y)= item in printf "%s %d\n" x y) contents;;

let main() =
    let ans = f1 [("computer", [2;3;4]);("trouble", [2;3;5;1])] in
    print_list ans;
    let ans = fold_f1 [("computer", [2;3;4]);("trouble", [2;3;5;1])] in
    print_list ans;
    let ans = map_f1 [("computer", [2;3;4]);("trouble", [2;3;5;1])] in
    print_list ans;;
    
main();;
