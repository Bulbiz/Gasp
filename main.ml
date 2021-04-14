(* programme principal *)

(*open Deftype*)
open Graphics
(*open Turtle*)

let main () =
  let ast = Parser.s Lexeur.token (Lexing.from_channel stdin) in 
  open_graph(" 800x600");
  Printf.printf "Parse:\n%s\n" ast
;;

let () = main ()
