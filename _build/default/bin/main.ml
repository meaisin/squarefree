let squarefree (number: int) : bool =
    let max = Float.of_int number |> sqrt |> Float.to_int |> (fun x -> x + 2) in
    let rec inner i number2 =
        if i == max
        then true
        else if number2 mod (i*i) == 0
             then false
             else inner (i+1) number2
    in inner 2 number
;;

(*
First pass: 2 12 -- i == max = false -> (2*2) mod 12 == 0 
*)

let list_squarefree_integers (x, y) =
    let rec inner start finish output =
        if start == finish
        then output
        else if squarefree start
             then inner (start+1) finish (start :: output)
             else inner (start+1) finish output
    in inner x y []
;;

let print_squarefree_integers (x, y) =
    let squarefrees_unrev = list_squarefree_integers (x, y) in
    let squarefrees = List.rev squarefrees_unrev in
    let rec inner sfs =
        match sfs with
          []    -> Printf.printf "\n"
        | h::t  -> (Printf.printf "%d " h; inner t;)
    in inner squarefrees
;;

let () = 
    print_squarefree_integers (1, 101);
    print_squarefree_integers (1000000000000, 1000000000146)
;;