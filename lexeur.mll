{
open Parser
}

let values = ['1'-'9']['0'-'9']* | '0'
let id = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*
let layout = [ ' ' '\t' '\n' ]

(*TODO: implémenter epsilon*)
rule token = parse
|[ ' ' '\t' '\n' ] { token lexbuf }
|"Var"  { VAR }
|id    { ID (Lexing.lexeme lexbuf) }
|values     { NOMBRE (int_of_string (Lexing.lexeme lexbuf)) }
|"Debut"    { DEBUT }
|"Fin"    { FIN }  
|"HautPinceau"     { HAUTPINCEAU }
|"BasPinceau"      { BASPINCEAU }
|"Avance"      { AVANCE }
|"Tourne"      { TOURNE }
|"("      { LPAREN }
|")"      { RPAREN }
|"+"      { PLUS }
|"-"      { MOINS }
|"="      { EQUALS }
|"/"      { DIV }
|";"      { ENDLINE }
|eof	  { EOF }
| _			{ failwith "Unexpected character" }
