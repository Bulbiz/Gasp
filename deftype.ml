type identificateur = string
type nombre = int

(*type expression = 
|(nombre : int; suite : expressionsuite)
|(id : identificateur; suite : expressionsuite) 
|(exp : expression; suite : expressionsuite)*)

type expression =
  | Nombre of nombre 
  | Id of identificateur 
  | Plus of expression * expression
  | Moins of expression * expression
  | Division of expression * expression
  | Multiplication of expression * expression
  (*| Rien*)

type instruction =
  | Avance of expression
  | Tourne of expression
  | ChangeCouleur of identificateur
  | ChangeEpaisseur of nombre
  | BasPinceau
  | HautPinceau
  | Affectation of identificateur * expression
  | Si of expression * instruction list * instruction list
  | Tant_que of expression * instruction list
(* 
  | Debut
  | Fin
*)
type declaration = 
  | Var of identificateur

type programme = Programme of declaration list * instruction list

let as_string = function
  |_ -> "aaa"
