(* Use case for word-count merge from https://sites.google.com/site/farazahmad/pumabenchmarks 
Reduce job input list of word int tuples output is sum by word *)

(* return a word,number tuple and a list of other words *)
let rec list_split word count input output =
    match input with 
        [] -> (word,count,output) |
        h :: t ->
            let (newword,newcount) = h in 
                match newword with 
                    word -> let ctot = count + newcount in
                        list_split word ctot t output |
                    _ -> list_split word count t output::h;;
                    
                
                

let rec word_count input output = 
    match input with 
       [] -> output |
       h::t -> 
            let (word,count) = h in
            let (word,count,o) = list_split word count input output in
                word_count input::(word,count) o;;

let print_list_of_pairs contents =  
    List.iter (fun outer -> printf "%s: %d \n" (fst outer) (snd outer)) contents;;


let main() =
    let input= [("OCaml",1);("can",1),("be",2),("OK",1),("OK",1),("OCaml",1)] in
    let ans= word_count input [] in
    print_list_of_pairs ans;;

