open Deftype
open Graphics
open Printf

type position =
  { x : float (** position x *)
  ; y : float (** position y *)
  ; a : int (** angle of the direction *)
  ; pen : bool (** true -> pen down, false -> pen up *)
  }

let initial_position = { x = 0.; y = 0.; a = 90 ; pen = true }
let current_position = ref initial_position

let convert_degree_to_radian angle = angle *. (Float.pi /. 180.)

(** Avance l and turn the angle a *)
let update_current_position i a p=
  let angle = convert_degree_to_radian (float_of_int !current_position.a) in
  let longueur = i in
  let new_x = !current_position.x +. (cos angle *. longueur) in
  let new_y = !current_position.y +. (sin angle *. longueur) in
  let new_a = !current_position.a + a in
  current_position := { x = new_x; y = new_y; a = new_a; pen = p }
;;

let get_value id env =
  List.assoc id env

let rec eval_expr env exp =
  match exp with
  | Nombre (nb,expsuite) -> eval_exprsuite nb env expsuite
  | Id (id,expsuite) -> 
    let value = get_value id env in
    eval_exprsuite value env expsuite
  | Exp (exp2,expsuite) -> 
    let value = eval_expr env exp2 in
    eval_exprsuite value env expsuite

and eval_exprsuite value env expsuite =
  match expsuite with
  |Plus (exp) -> value + (eval_expr env exp)
  |Moins (exp) -> value - (eval_expr env exp)
  |Division (exp) -> value / (eval_expr env exp)
  |Rien -> value


let interpret_avance env value =
  printf "Avance %d\n" value;
  update_current_position (float_of_int value) 0 (!current_position.pen);
  if !current_position.pen then
    (moveto (int_of_float !current_position.x) (int_of_float !current_position.y);
    env)
  else 
    (lineto (int_of_float !current_position.x) (int_of_float !current_position.y);
    env)
;;

let interpret_tourne env value =
  printf "Tourne %d\n" value;
  update_current_position 0. value (!current_position.pen);
  env
;;

let interpret_bas_pinceau env =
  printf "Bas\n";
  update_current_position 0. 0 false;
  env
;;

let interpret_haut_pinceau env =
  printf "Haut\n";
  update_current_position 0. 0 true;
  env
;;

let interpret_affectation env id value =
  printf "Affectation %s : %d\n" id value;
  let new_env = (id,value) :: (List.remove_assoc id env) in
  new_env
;;


let interpret_instruction env i =
  match i with
  |Avance (exp) -> interpret_avance env (eval_expr env exp)
  |Tourne (exp) -> interpret_tourne env (eval_expr env exp)
  |BasPinceau -> interpret_bas_pinceau env
  |HautPinceau -> interpret_haut_pinceau env
  |Affectation (id,exp) -> interpret_affectation env id (eval_expr env exp)

let rec interpret_instructions env il =
  match il with
  | i :: l -> 
    let new_env = interpret_instruction env i in 
    interpret_instructions new_env l
  | [] -> ()

let transform_declaration d =
  match d with
  |Var(s) -> 
    printf "Declaration %s\n" s;
    (s,0)
;;

let interpret_programme programme =
  match programme with
  | Programme (dl,il) -> 
    let env = List.map transform_declaration dl in
    interpret_instructions env il
;;
