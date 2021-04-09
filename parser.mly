%{
open Deftype
%}

%token AVANCE TOURNE BASPINCEAU HAUTPINCEAU DEBUT FIN VAR EOF EQUALS ENDLINE PLUS MOINS RPAREN LPAREN
%token<string> ID
%token<int> NOMBRE

%start<Deftype.programme> s

%%
s: p=programme EOF {p}

programme: declaration instruction

declaration:
| VAR ID ENDLINE declaration 
| 

blocInstruction: 
| instruction ENDLINE blocInstruction 
| 

expression: 
| NOMBRE expressionsuite 
| ID expressionsuite
| LPAREN expression RPAREN expressionsuite

expressionsuite:
| PLUS expression
| MOINS expression
| 

instruction:
| DEBUT blocInstruction FIN { Debut } (*TODO: compléter cette ligne*)
| BASPINCEAU { BasPinceau }
| HAUTPINCEAU { HautPinceau }
| AVANCE e=expression { Avance(e) }
| TOURNE e=expression { Tourne(e) }
| v=identificateur EQUALS e=expression { Affectation(v, e) }
