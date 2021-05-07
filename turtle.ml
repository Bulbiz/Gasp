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
  if (List.mem_assoc id env) then(
    List.assoc id env
  )else(
    Printf.printf "La variable %s n'est pas définie ! 0 par défaut\n" id;
    0
  )

let rec eval_expr env exp =
  match exp with
  | Nombre (nb) -> nb
  | Id (var) -> get_value var env
  | Plus (exp1, exp2) -> (eval_expr env exp1) + (eval_expr env exp2)
  | Moins (exp1, exp2) -> (eval_expr env exp1) - (eval_expr env exp2)
  | Division (exp1, exp2) -> 
    if (eval_expr env exp2) == 0 then(
      Printf.printf "Erreur Division par Zéro !\n";
      exit 0
    )else 
      (eval_expr env exp1) / (eval_expr env exp2)
  | Multiplication (exp1, exp2) -> (eval_expr env exp1) * (eval_expr env exp2)

let interpret_avance env value =
  printf "Avance %d\n" value;
  update_current_position (float_of_int value) 0 (!current_position.pen);
  let x = int_of_float !current_position.x in
  let y = int_of_float !current_position.y in
  if (x < 0 || y < 0 || x > 1000 || y > 600) then
  (
    Printf.printf "Erreur, le curseur est sorti du canvas !\n";
    exit 0
  )else(
    if !current_position.pen then
      (moveto x y;
      env)
    else 
      (lineto x y;
      env)
  )
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


let interpret_couleur env (id : string) =
  Printf.printf "ChangeCouleur %s\n" id;
  match id with
  |"noir" -> set_color black; env
  |"blanc" -> set_color white; env
  |"rouge" -> set_color red; env
  |"vert" -> set_color green; env
  |"bleu" -> set_color blue; env
  |"jaune" -> set_color yellow; env
  |"cyan" -> set_color cyan; env
  |"magenta" -> set_color magenta; env
  | _ -> set_color black; env
;;


let interpret_epaisseur env value =
  Printf.printf "ChangeEpaisseur %d\n" value;
  if value < 0 then(
    Printf.printf "Vous ne pouvez pas demander une valeur négative pour l'épaisseur\n";
    env
  )else(
    set_line_width value;
    env
  )


let rec interpret_instruction env i =
  match i with
  |Avance (exp) -> interpret_avance env (eval_expr env exp)
  |Tourne (exp) -> interpret_tourne env (eval_expr env exp)
  |ChangeCouleur (couleur) -> interpret_couleur env couleur
  |ChangeEpaisseur (taille) -> interpret_epaisseur env taille
  |BasPinceau -> interpret_bas_pinceau env
  |HautPinceau -> interpret_haut_pinceau env
  |Affectation (id,exp) -> interpret_affectation env id (eval_expr env exp)
  |Si (exp,itrue, ifalse) -> interpret_si env exp itrue ifalse 
  |Tant_que (exp,i) -> interpret_tant_que env exp i

and interpret_si env exp itrue ifalse = 
  if (eval_expr env exp != 0) then
    interpret_instructions env itrue
  else
    interpret_instructions env ifalse

and interpret_tant_que env exp i =
  if (eval_expr env exp != 0) then
    let new_env = interpret_instructions env i in
    interpret_tant_que new_env exp i
  else
    env

and interpret_instructions env il =
  match il with
  | i :: l -> 
    let new_env = interpret_instruction env i in 
    interpret_instructions new_env l
  | [] -> env

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
    let _ = interpret_instructions env il in
    ()
;;
