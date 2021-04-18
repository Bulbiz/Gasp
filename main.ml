(* programme principal *)

open Graphics

let main () =
  let programme = Parser.s Lexeur.token (Lexing.from_channel stdin) in 
  open_graph(" 1000x600");
  Turtle.interpret_programme programme;
  let user_input = Graphics.wait_next_event [ Graphics.Key_pressed ] in
  match user_input.key with
  |_ -> ()

;;

let () = main ()
