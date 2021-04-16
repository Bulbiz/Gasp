type identificateur = string
type nombre = int

(*type expression = 
|(nombre : int; suite : expressionsuite)
|(id : identificateur; suite : expressionsuite) 
|(exp : expression; suite : expressionsuite)*)

type expression =
  | Nombre of nombre * expressionsuite
  | Id of identificateur * expressionsuite
  | Exp of expression * expressionsuite

and expressionsuite =
  | Plus of expression
  | Moins of expression
  | Division of expression
  | Rien

type instruction =
  | Avance of expression
  | Tourne of expression
  | BasPinceau
  | HautPinceau
  | Affectation of identificateur * expression
  | Debut
  | Fin

type declaration = 
  | Var of identificateur
  | EndDeclaration

type programme = Programme of declaration list * instruction list

let as_string = function
  |_ -> "aaa"
