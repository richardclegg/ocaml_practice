(* Use case for word-count reduce from https://sites.google.com/site/farazahmad/pumabenchmarks 
Reduce job input list of word int tuples output is sum by word *)

(* build with 
ocamlfind ocamlopt -package core_bench -thread -linkpkg word_count.ml -o word_count 
*)

open Core.Std
open Core_bench.Std

(* Output a list of (string,int) *)
let print_list_of_pairs contents =  
    List.iter contents (fun outer -> printf "%s: %d \n" (fst outer) (snd outer)) 


(* Build a list of word,count *)
let rec rec_word_count input matched = 
    match input with 
       [] -> matched |
       h::t ->   (* Get first word from list *)
            let count= ref (snd h) in
            (* Partition remaining list into matching this word and not*)
            let (mat, no_mat) = List.partition_tf t
                ~f:(fun x -> 
                    if (fst h) = (fst x) then 
                        (count:= (!count + (snd x));true)
                    else 
                        false
                )  in
            rec_word_count no_mat ((fst h,!count)::matched)

(* Do the count with an associative array *)
let word_count2 input = 
    let counts= ref [] in
    List.iter input (fun (w,c) -> 
        let count= 
        match List.Assoc.find !counts w with
            | None -> 0
            | Some x -> x
        in counts:= List.Assoc.add !counts w (count + c)
    );
    !counts
    
   

(* Count words by looping through, counting new words and marking
counting words as used with an array *)
let word_count input =
    let used= Array.create (List.length input) 0 in
    let counts= ref [] in
    let remains= ref input in
    List.iteri input (fun i x ->
        ignore (match !remains with 
            [] -> () |
            h::t -> remains:= t); 
        if Array.get used i = 0 then (
            let count= ref (snd x) in
            List.iteri !remains (fun j y->
                if (fst y) = (fst x) then (
                    count:= !count + (snd y);
                    Array.set used ( i+j +1)  1
                )
            );
            counts:= ((fst x,!count)::!counts)
        )
    );
    !counts

let () =
    Random.self_init ();
    (* input is a randomly generated list example [("Ocaml",3);("is",4);("not",5)] *)
    let words= Array.of_list ["OCaml";"is";"not";"my";"friend"] in
    let input= ref [] in
    for i = 1 to 10000 do
        let w = words.(Random.int (Array.length words)) in
        let doc= (Random.int 1000) in
        input:= (w,doc)::!input
    done;
    Command.run (Bench.make_command [
    Bench.Test.create ~name:"rec_word_count" (fun () -> ignore (rec_word_count !input []));
    Bench.Test.create ~name:"word_count" (fun () -> ignore (word_count !input));
    Bench.Test.create ~name:"word_count2" (fun () -> ignore (word_count !input));
    ]);


