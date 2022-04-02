let squarefree (number: int) : bool =
    let max = Float.of_int number |> sqrt |> Float.to_int |> (fun x -> x + 1) in
    let rec inner i number2 =
        if i == max
        then true
        else if number2 mod (i*i) == 0
             then false
             else inner (i+1) number2
    in inner 2 number
;;

let list_squarefree_integers x y =
    let rec inner start finish output =
        if start == finish
        then output
        else if squarefree start
             then inner (start+1) finish (start :: output)
             else inner (start+1) finish output
    in inner x y []
;;

let count_squarefree_integers x y : int =
    let rec inner start finish count =
        if start == finish
        then count
        else if squarefree start
             then inner (start+1) finish (count+1)
             else inner (start+1) finish count
    in inner x y 0

(*
let print_squarefree_integers x y : string =
    let squarefrees_unrev = list_squarefree_integers x y in
    let squarefrees = List.rev squarefrees_unrev in
    let rec inner sfs i count str =
        match sfs with
          []    -> count
        | h::t  -> (if (i+1) mod 5 == 0 then (Printf.printf "%d\n" h) else (Printf.printf "%d " h); inner t (i+1) (count+1);)
    in Printf.printf "\n\nTotal count of square-free numbers between %d and %d: %d\n" x y (inner squarefrees 0 0)
;;
*)

let print_squarefree_integers x y =
    let squarefrees = list_squarefree_integers x y |> List.rev in
    let rec inner sfs i count str =
        match sfs with
          []    -> str ^ "\n\nTotal count of square-free numbers between " ^ (string_of_int x) ^ " and " ^ (string_of_int y) ^ ": " ^ (string_of_int count)
        | h::t  -> if (i+1) mod 5 == 0 
                   then inner t (i+1) (count+1) (str ^ (string_of_int h) ^ "\n") 
                   else inner t (i+1) (count+1) (str ^ (string_of_int h) ^ " ")
    in inner squarefrees 0 0 ""
;;

let () = 
    let count1 = count_squarefree_integers 1 100 in
    let count2 = count_squarefree_integers 1 1000 in
    let count3 = count_squarefree_integers 1 10000 in
    let count4 = count_squarefree_integers 1 100000 in
    let count5 = count_squarefree_integers 1 1000000 in
    print_endline (print_squarefree_integers 1 146);
    print_endline (print_squarefree_integers 1000000000000 1000000000146);
    Printf.printf "Number of squarefree integers between 1 and 100: %d\n" count1;
    Printf.printf "Number of squarefree integers between 1 and 1000: %d\n" count2;
    Printf.printf "Number of squarefree integers between 1 and 10000: %d\n" count3;
    Printf.printf "Number of squarefree integers between 1 and 100000: %d\n" count4;
    Printf.printf "Number of squarefree integers between 1 and 1000000: %d\n" count5
;;
