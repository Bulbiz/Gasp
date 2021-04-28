%{
open Deftype
%}

%token AVANCE TOURNE BASPINCEAU HAUTPINCEAU DEBUT FIN VAR EOF EQUALS ENDLINE PLUS MOINS RPAREN LPAREN DIV MUL SI ALORS SINON WHILE FAIRE
%token<string> ID
%token<int> NOMBRE

%start<Deftype.programme> s

%%
s: p=programme EOF { p }

programme: d=declaration i=instruction { Programme (d, i) }

declaration:
| VAR id=ID ENDLINE d=declaration { [Var (id)] @ d }
| { [] }

blocInstruction: 
| i=instruction ENDLINE bi=blocInstruction { i @ bi } (*TODO : définir la valeur de retour de ce cas*)
| { [] }

expression: 
| i=NOMBRE e=expressionsuite { Nombre (i, e) }
| s=ID e=expressionsuite { Id (s, e) }
| LPAREN e=expression RPAREN es=expressionsuite { Exp (e, es) }

expressionsuite:
| PLUS e=expression { Plus (e) }
| MOINS e=expression { Moins (e) } 
| DIV e=expression { Division (e) } 
| { Rien }

instruction:
| DEBUT bi=blocInstruction FIN { bi } 
| SI e=expression ALORS ba=instruction SINON bs=instruction { [Si (e, ba, bs)] }
| WHILE e=expression FAIRE bi=instruction { [Tant_que (e, bi)] }
| BASPINCEAU { [BasPinceau] }
| HAUTPINCEAU { [HautPinceau] }
| AVANCE e=expression { [Avance (e)] }
| TOURNE e=expression { [Tourne (e)] }
| x=ID EQUALS v=expression { [Affectation (x, v)] }
