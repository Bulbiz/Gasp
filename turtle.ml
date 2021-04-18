open Deftype
(*open Graphics*)
open Printf

let interpret_avance env =
  printf "Avance\n";
  env
;;

let interpret_tourne env =
  printf "Tourne\n";
  env
;;

let interpret_bas_pinceau env =
  printf "Bas\n";
  env
;;

let interpret_haut_pinceau env =
  printf "Haut\n";
  env
;;

let interpret_affectation env =
  printf "Affectation\n";
  env
;;

let interpret_instruction env i =
  match i with
  |Avance (_) -> interpret_avance env
  |Tourne (_) -> interpret_tourne env
  |BasPinceau -> interpret_bas_pinceau env
  |HautPinceau -> interpret_haut_pinceau env
  |Affectation (_,_) -> interpret_affectation env

let rec interpret_instructions env il =
  match il with
  | i :: l -> 
    let new_env = interpret_instruction env i in 
    interpret_instructions new_env l
  | [] -> ()

let transform_declaration d =
  match d with
  |Var(s) -> (s,0)
;;

let interpret_programme programme =
  match programme with
  | Programme (dl,il) -> 
    let env = List.map transform_declaration dl in
    interpret_instructions env il
;;














































































(*
type position =
  { x : float (** position x *)
  ; y : float (** position y *)
  ; a : int (** angle of the direction *)
  ; pen : bool (** true -> pen down, false -> pen up *)
  }

let initial_position = { x = 0.; y = 0.; a = 0 ; pen = false }
let current_position = ref initial_position

let convert_degree_to_radian angle = angle *. (Float.pi /. 180.)

let update_current_position i a =
  let angle = convert_degree_to_radian (float_of_int !current_position.a) in
  let longueur = i in
  let new_x = !current_position.x +. (cos angle *. longueur) in
  let new_y = !current_position.y +. (sin angle *. longueur) in
  let new_a = !current_position.a + a in
  current_position := { x = new_x; y = new_y; a = new_a }
;;

let interpret_line i =
  update_current_position i 0;
  lineto (int_of_float !current_position.x) (int_of_float !current_position.y)
;;

let interpret_move i =
  update_current_position i 0;
  moveto (int_of_float !current_position.x) (int_of_float !current_position.y)
;;

let interpret_turn a = update_current_position 0. a 
;;
*)