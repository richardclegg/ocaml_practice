open Printf
open List

let rec f1 contents =
    map (fun (k,vl) -> (k, List.fold_left (+) 0 vl)) contents;;

let rec print_list contents =
    List.iter (fun item -> let (x,y)= item in printf "%s %d\n" x y) contents;;

let main() =
    let ans = f1 [("computer", [2;3;4]);("trouble", [2;3;5;1])] in
    print_list ans;;
    
main();;
