type identificateur = string
type nombre = int

type expression =
  | Nombre of nombre 
  | Id of identificateur 
  | Plus of expression * expression
  | Moins of expression * expression
  | Division of expression * expression
  | Multiplication of expression * expression

type instruction =
  | Avance of expression
  | Tourne of expression
  | ChangeCouleur of string
  | ChangeEpaisseur of nombre
  | BasPinceau
  | HautPinceau
  | Affectation of identificateur * expression
  | Si of expression * instruction list * instruction list
  | Tant_que of expression * instruction list

type declaration = 
  | Var of identificateur

type programme = Programme of declaration list * instruction list

