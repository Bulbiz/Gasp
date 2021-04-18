(* programme principal *)

open Graphics

let main () =
  let programme = Parser.s Lexeur.token (Lexing.from_channel stdin) in 
  open_graph(" 800x600");
  Turtle.interpret_programme programme
;;

let () = main ()
