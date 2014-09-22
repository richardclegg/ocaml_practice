(* Use case for inverted_index reduce from https://sites.google.com/site/farazahmad/pumabenchmarks 
Reduce job input of list (word, docid) to list (word, list(docid))*)

(* build with 
ocamlfind ocamlopt -package core_bench -thread -linkpkg inverted.ml -o inverted
*)

open Core.Std
open Core_bench.Std



(* Output a list of (string,int) *)
let print_list_of_pairs contents =  
    List.iter contents (fun outer -> printf "%s: %d \n" (fst outer) (snd outer)) 

(* Output a list of (string, list of int) *)
let print_list_of_lists contents =  
    List.iter contents (fun outer -> printf "%s: " (fst outer);
        List.iter (snd outer) (fun inner -> printf "%d " inner) ;
        printf ("\n");
    ) 

(* Split a list into a word with a count and remaining words *)
let rec list_split word doclist input output =
    match input with 
        [] -> (word,doclist,output) |
        h::t ->
            (if word = (fst h) then
                list_split word ((snd h)::doclist) t output
            else 
                list_split word doclist t (h::output))
                
                

(* Build a list of word, doclist *)
let rec rec_invert_index input matched = 
    match input with 
       [] -> matched |
       h::t -> 
            let (w,l,o) = list_split (fst h) [(snd h)] t [] in
                let m= (w,l)::matched in 
                    rec_invert_index o m;;
                    
(* Build a list of string * list of int which is word, list of docs *)
let invert_index input =
    let docs= ref [] in
    List.iter input (fun (w,d) -> 
        let old_docs= 
        match List.Assoc.find !docs w with
            | None -> []
            | Some x -> x
        in docs:= List.Assoc.add !docs w (d::old_docs)
    );
    !docs

let invert_index2 input = 
    let docs= ref [] in
    let used= Array.of_list (List.map input (fun x-> 0)) in
    List.iteri input (fun i (word,docno) ->
        if (Array.get used i) = 0 then (
            let doclist= ref [] in
                List.iteri input (fun j (w, dno) ->
                if (Array.get used j) = 0 then if word = w then (
                    doclist:= dno::!doclist;
                    Array.set used j 1
                )
            );
            docs:= (word,!doclist)::!docs
        )
    );
    !docs
    
let invert_index3 input = 
    let remainder= ref input in
    let docs= ref [] in
    List.iter input (fun (w,d) ->
        let rem= ref [] in
        let doclist= ref [] in
        List.iter !remainder (
            fun (nw,nd) ->
                if nw = w then 
                    doclist:= nd::!doclist
                else
                    rem:= (nw,nd)::!rem
        );
        if List.length !doclist > 0 then (
            docs:= (w,!doclist)::!docs;
            remainder:= !rem
        )
    );
    !docs


let () =
    Random.self_init ();
    let words= Array.of_list ["OCaml";"is";"not";"my";"friend"] in
    let input= ref [] in
    for i = 1 to 10000 do
        let w = words.(Random.int (Array.length words)) in
        let doc= (Random.int 1000) in
        input:= (w,doc)::!input
    done;
(*
    printf "INPUT:\n"; 
    print_list_of_pairs !input;
    printf "OUTPUT:\n";
    let ans= invert_index3 !input in
    print_list_of_lists ans;
*)
    Command.run (Bench.make_command [
    Bench.Test.create ~name:"rec_invert_index" (fun () -> ignore (rec_invert_index !input []));
    Bench.Test.create ~name:"invert_index" (fun () -> ignore (invert_index !input));
    Bench.Test.create ~name:"invert_index2" (fun () -> ignore (invert_index2 !input));
    Bench.Test.create ~name:"invert_index3" (fun () -> ignore (invert_index3 !input));
    ]);

