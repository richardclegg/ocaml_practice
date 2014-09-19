(* Use case for inverted_index reduce from https://sites.google.com/site/farazahmad/pumabenchmarks 
Reduce job input of list (word, docid) to list (word, list(docid))*)

open Printf;; 
open Random;;

(* Output a list of (string,int) *)
let print_list_of_pairs contents =  
    List.iter (fun outer -> printf "%s: " (fst outer);
        List.iter (fun inner -> printf "%d " inner) (snd outer);
        printf ("\n");
    ) contents;;   

(* Split a list into a word with a count and remaining words *)
let rec list_split word doclist input output =
    match input with 
        [] -> (word,doclist,output) |
        h::t ->
            (if word = (fst h) then
                list_split word ((snd h)::doclist) t output
            else 
                list_split word doclist t (h::output));;

(* Build a list of word, doclist *)
let rec rec_invert_index input matched = 
    match input with 
       [] -> matched |
       h::t -> 
            let (w,l,o) = list_split (fst h) [(snd h)] t [] in
                let m= (w,l)::matched in 
                    rec_invert_index o m;;

let main() =
    Random.self_init ();
    let words= Array.of_list ["OCaml";"is";"not";"my";"friend"] in
    let input= ref [] in
    for i = 1 to 1000 do
        let w = words.(Random.int (Array.length words)) in
        let doc= (Random.int 1000) in
        input:= (w,doc)::!input
    done;
    let ans= rec_invert_index !input [] in
    print_list_of_pairs ans;;

main();;
