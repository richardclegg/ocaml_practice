open Printf
open List

(* Return a list with only those elements where the value is num *)
let filt list hashfunc num=
    List.filter (fun x -> (hashfunc (fst x) == num)) list
    
let rec splitlist contents hashfunc hashvals  lists  =
    match hashvals with
        [] -> lists |
        h::t -> let lists= (List.map snd (filt contents hashfunc h))::lists in
        splitlist contents hashfunc t lists;;

let split contents hashfunc hashvals =
    splitlist contents hashfunc hashvals [];;

let rec print_list_of_lists contents =  
    List.iter (fun innerlist ->  List.iter (printf "%s ") innerlist; printf "\n") contents;;

(*Hash to five lists*)
let hash no =   
    no mod 5;;
    
let hashvals = 
    [0;1;2;3;4];;

let main() =
    let input= [(1,"Ocaml");(2,"is");(3,"not");(4,"yet");(2,"my");(3,"friend")] in
    let ans= split input hash hashvals in
    print_list_of_lists ans;;
   
main();;
