
open Printf
open List


let f contents =
    List.fold_left append [] (fun (k,vl) -> (k, (List.fold_left (+) 0 vl))) [] contents;;

let print_list contents =
    match contents with
        [] -> [] |
        h::t ->
            let (x,y) = h in
                printf "%s %d\n" x y ::  print_list t;;

let main() =
    let ans = f [("computer", [2;3;4]);("trouble", [2;3;5;1])] in
    print_list ans;;
   
main();;
