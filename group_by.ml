open Printf
open List

let rec group_by_list input list =
    match input with
        [] -> list |
        h::t -> 
        let tomerge= List.filter (fun x -> (fst h) == (fst x)) input in
        let mergevals= (List.map (fun y-> snd y) tomerge) in
        let merged= (fst h, mergevals) in
        let unmerged = List.filter (fun x -> (fst h) != (fst x)) input in
        group_by_list unmerged (merged::list);;

let group_by input =
    group_by_list input [];;

let rec print_list_of_key_lists contents =  
    List.iter (fun outer -> printf "%d: " (fst outer); 
        List.iter (fun inner -> printf "\"%s\" " inner) (snd outer); 
        printf "\n";) contents;;

let main() =
    let input= [(1,"Ocaml");(2,"is");(3,"not");(4,"yet");(2,"my");(3,"friend")] in
    let ans= group_by input in
    print_list_of_key_lists ans;;

main();;
