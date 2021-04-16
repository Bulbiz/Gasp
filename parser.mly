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
| instruction ENDLINE blocInstruction {"aaaa"} (*TODO : définir la valeur de retour de ce cas*)
| {"aaaa"}

expression: 
| i=NOMBRE e=expressionsuite { Nombre (i, e) }
| s=ID e=expressionsuite { Id (s, e) }
| LPAREN e=expression RPAREN es=expressionsuite { Exp (e, es) }

expressionsuite:
| PLUS e=expression { Plus (e) }
| MOINS e=expression { Moins (e) } 
| DIV expression {"aaaa"} (*TODO : Implémenter la division*)
| {"aaaa"}

instruction:
| DEBUT blocInstruction FIN {"aaaa"} (*TODO: compléter cette ligne*)
| BASPINCEAU { BasPinceau }
| HAUTPINCEAU { HautPinceau }
| AVANCE e=expression { Avance (e) }
| TOURNE e=expression { Tourne (e) }
| x=ID EQUALS v=expression { Affectation (x, v) }
