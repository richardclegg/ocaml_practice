open Printf
open List
open Array

(* Return a list with only those elements where the value is num *)
let filt list hashfunc num=
    List.filter (fun x -> (hashfunc (fst x) == num)) list

(* recursive split *)
let rec split_rec contents hashfunc hashvals lists  =
    match hashvals with
        [] -> lists |
        h::t -> let lists= (List.map snd (filt contents hashfunc h))::lists in
        split_rec contents hashfunc t lists;;

(* map split *)
let split_map contents hashfunc hashvals =
    List.map (fun x -> List.map snd (filt contents hashfunc x)) hashvals;;

(* Terse map split *)
let terse_split_map contents hashfunc hashvals =
    List.map (fun y -> List.map snd (List.filter (fun x -> (hashfunc (fst x) == y)) contents)) hashvals;;   
        
(* Imperative split *)
let split_imp contents hashfunc hashvals =
    let a= Array.make (List.length hashvals) [] in 
    List.iter (fun x -> let n= hashfunc (fst x) in
    Array.set a n ((snd x)::(Array.get a n))) contents;
    Array.to_list a;;
    
(* Terse imperative split *)
let terse_split_imp contents hashfunc hashvals =
    let a= Array.make (List.length hashvals) [] in 
    List.iter (fun x -> Array.set a (hashfunc (fst x)) ((snd x)::(Array.get a (hashfunc (fst x))))) contents;
    Array.to_list a;;    
(* Mmmmm.... brackety *)

(* print contents of list of strings *)
let print_list_of_lists contents =  
    List.iter (fun innerlist ->  List.iter (printf "%s ") innerlist; printf "\n") contents;;

(*Hash to five lists*)
let hash no =   
    no mod 5;;
    
let hashvals = 
    [0;1;2;3;4];;

let main() =
    let input= [(1,"Ocaml");(2,"is");(3,"not");(4,"yet");(2,"my");(3,"friend")] in
    printf("\n*Imperative split*:\n");
    let ans= terse_split_imp input hash hashvals in
    print_list_of_lists ans;
    printf("\n*Recursive split*:\n");
    let ans= split_rec input hash hashvals [] in
    print_list_of_lists ans;
    printf("\n*Split with map*:\n");
    let ans= terse_split_map input hash hashvals in
    print_list_of_lists ans;;
   
   
main();;
