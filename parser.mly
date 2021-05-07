%{
open Deftype
%}

%token AVANCE TOURNE BASPINCEAU HAUTPINCEAU DEBUT FIN VAR EOF EQUALS ENDLINE PLUS MOINS RPAREN LPAREN DIV MUL SI ALORS SINON WHILE FAIRE CHANGECOULEUR CHANGEEPAISSEUR
%token<string> ID
%token<int> NOMBRE
%left PLUS MOINS
%right MUL DIV

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
| MOINS i=NOMBRE { Nombre (-i) }
| i=NOMBRE { Nombre (i) }
| x=ID { Id (x) }
| e1=expression PLUS e2=expression { Plus (e1, e2) }
| e1=expression MOINS e2=expression { Moins (e1, e2) }
| e1=expression MUL e2=expression { Multiplication (e1, e2) }
| e1=expression DIV e2=expression { Division (e1, e2) }
| LPAREN e=expression RPAREN { e }

(*expression: 
| i=NOMBRE DIV j=NOMBRE e=expressionsuite { Nombre (i/j, e) }
| i=NOMBRE MUL j=NOMBRE e=expressionsuite { Nombre (i*j, e) }
| i=NOMBRE e=expressionsuite { Nombre (i, e) }
| s=ID e=expressionsuite { Id (s, e) }
| LPAREN e=expression RPAREN es=expressionsuite { Exp (e, es) }*)

(*expressionsuite:
| PLUS e=expression { Plus (e) }
| MOINS e=expression { Moins (e) } 
(*| DIV e=expression { Division (e) } 
| MUL e=expression { Multiplication (e) }*)
| { Rien }*)

instruction:
| DEBUT bi=blocInstruction FIN { bi } 
| SI e=expression ALORS ba=instruction SINON bs=instruction { [Si (e, ba, bs)] }
| WHILE e=expression FAIRE bi=instruction { [Tant_que (e, bi)] }
| CHANGECOULEUR c=ID { [ChangeCouleur (c)] }
| CHANGEEPAISSEUR i=NOMBRE { [ChangeEpaisseur (i)] }
| BASPINCEAU { [BasPinceau] }
| HAUTPINCEAU { [HautPinceau] }
| AVANCE e=expression { [Avance (e)] }
| TOURNE e=expression { [Tourne (e)] }
| x=ID EQUALS v=expression { [Affectation (x, v)] }
