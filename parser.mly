%{
(*open Deftype*)
%}

%token AVANCE TOURNE BASPINCEAU HAUTPINCEAU DEBUT FIN VAR EOF EQUALS ENDLINE PLUS MOINS RPAREN LPAREN DIV
%token<string> ID
%token<int> NOMBRE

%start<string(*Deftype.programme*)> s

%%
s: programme EOF {"aaaa"}

programme: declaration instruction {"aaaa"}

declaration:
| VAR ID ENDLINE declaration {"aaaa"}
| {"aaaa"}

blocInstruction: 
| instruction ENDLINE blocInstruction {"aaaa"}
| {"aaaa"}

expression: 
| NOMBRE expressionsuite {"aaaa"}
| ID expressionsuite {"aaaa"}
| LPAREN expression RPAREN expressionsuite {"aaaa"}

expressionsuite:
| PLUS expression {"aaaa"}
| MOINS expression {"aaaa"} 
| DIV expression {"aaaa"} 
| {"aaaa"}

instruction:
| DEBUT blocInstruction FIN {"aaaa"} (*TODO: compléter cette ligne*)
| BASPINCEAU {"aaaa"}
| HAUTPINCEAU {"aaaa"}
| AVANCE expression {"aaaa"}
| TOURNE expression {"aaaa"}
| ID EQUALS expression {"aaaa"}
