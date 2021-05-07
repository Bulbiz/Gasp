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
|"Si"     { SI }
|"Alors"    { ALORS }
|"Sinon"    { SINON }
|"Tant que"     { WHILE }
|"Faire"    { FAIRE }
|"Debut"    { DEBUT }
|"Fin"    { FIN }  
|"ChangeCouleur"     { CHANGECOULEUR }
|"ChangeEpaisseur"    { CHANGEEPAISSEUR }
|"BasPinceau"      { BASPINCEAU }
|"HautPinceau"     { HAUTPINCEAU }
|"Avance"      { AVANCE }
|"Tourne"      { TOURNE }
|"("      { LPAREN }
|")"      { RPAREN }
|"+"      { PLUS }
|"-"      { MOINS }
|"*"      { MUL }
|"="      { EQUALS }
|"/"      { DIV }
|";"      { ENDLINE }
|eof	  { EOF }
| _			{ failwith "Unexpected character" }
