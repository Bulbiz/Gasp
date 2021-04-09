(* programme principal *)

open Syntax
open Graphics
open Turtle

let main () =
  let ast = Parser.s Lexer.token (Lexing.from_channel stdin) in 
  open_graph(" 800x600")
;;

let () = main ()
