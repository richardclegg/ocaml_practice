(* Use case for word-count reduce from https://sites.google.com/site/farazahmad/pumabenchmarks 
Reduce job input list of word int tuples output is sum by word *)

open Printf;; 

(* Output a list of (string,int) *)
let print_list_of_pairs contents =  
    List.iter (fun outer -> printf "%s: %d \n" (fst outer) (snd outer)) contents;;   

(* Split a list into a word with a count and remaining words *)
let rec list_split word count input output =
    match input with 
        [] -> (word,count,output) |
        h::t ->
            (if word = (fst h) then
                list_split word (count+(snd h)) t output
            else 
                list_split word count t (h::output));;

(* Build a list of word,count *)
let rec rec_word_count input matched = 
    match input with 
       [] -> matched |
       h::t -> 
            let (w,c,o) = list_split (fst h) (snd h) t [] in
                let m= (w,c)::matched in 
                    rec_word_count o m;;




let main() =
    Random.self_init ();
    let words= Array.of_list ["OCaml";"is";"not";"my";"friend"] in
    let input= ref [] in
    for i = 1 to 1000 do
        let w = words.(Random.int (Array.length words)) in
        let doc= (Random.int 1000) in
        input:= (w,doc)::!input
    done;
    let ans= rec_word_count !input [] in
    print_list_of_pairs ans;;

main();;
