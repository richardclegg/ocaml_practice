open Printf
open List


let merge input = 
    List.flatten input;;

let rec print_list_of_pairs contents =  
    List.iter (fun outer -> printf "%d: %s \n" (fst outer) (snd outer)) contents;;

let main() =
    let input= [[(1,"Ocaml");(2,"is")];[(3,"not");(4,"yet");(2,"my")];[(3,"friend")]] in
    let ans= List.flatten input in
    print_list_of_pairs ans;;

main();;
