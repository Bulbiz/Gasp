%{
open Deftype
%}

%token AVANCE TOURNE BASPINCEAU HAUTPINCEAU DEBUT FIN VAR EOF EQUALS ENDLINE PLUS MOINS EPSILON RPAREN LPAREN
%token<string> ID
%token<int> NOMBRE

%start<Deftype.instruction> input

%%

input: i = instruction EOF { i }

programme: declaration instruction

declaration:
| VAR ID ENDLINE declaration 
| EPSILON

blocInstruction: 
| instruction ENDLINE blocInstruction 
| EPSILON

expression: 
| NOMBRE expressionsuite 
| ID expressionsuite
| LPAREN expression RPAREN expressionsuite

expressionsuite:
| PLUS expression
| MOINS expression
| EPSILON

instruction:
| DEBUT blocInstruction FIN { Debut } (*TODO: compléter cette ligne*)
| VAR v=ID { Var(v) }
| BASPINCEAU { BasPinceau }
| HAUTPINCEAU { HautPinceau }
| AVANCE e=expression { Avance(e) }
| TOURNE e=expression { Tourne(e) }
| v=identificateur EQUALS e=expression { Affectation(v, e) }