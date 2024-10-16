
open Ast;;
type ptype =  VarType of string |Arrow of ptype * ptype | Nat
and env = (string * ptype) list  
and equa = ptype * ptype
and equas = (equa) list
;; 

let rec ptype_to_string (t : ptype) : string =
  match t with
  VarType x -> x
  | Arrow (t1 , t2) -> "(" ^ ( ptype_to_string t1) ^" -> "^ ( ptype_to_string t2) ^")"
  | Nat -> "Natural"
;; 
let print_ptype  (t:ptype)  = Printf.printf "%s\n" (ptype_to_string t) ;; 
let equa_to_string (e:equa) =
  let (t1,t2)= e in 
  (ptype_to_string t1)^" = "^ (ptype_to_string t2)
;; 
let equas_to_string (el:equas) : string =
  let rec aux el = 
    match el with 
    | []->""
    | [a] -> equa_to_string a 
    | a::[q] -> equa_to_string a ^","^ equa_to_string q 
    | a::q ::tail -> (equa_to_string a ^","^ equa_to_string q ) ^ "," ^(aux tail)
  in 
  "[" ^ (aux el) ^"]"
;; 

let env_to_string (el:env) : string =
  let aux_env_string a t = "(" ^ a^", "^ (ptype_to_string t )^")"  in 
  let rec aux el = 
    match el with 
    | []->""
    | [(a,t)] -> (aux_env_string a t)
    | (a,t)::[(a',t')] -> (aux_env_string a t)^","^ (aux_env_string a' t') 
    | (a,t)::(a',t') ::tail -> ((aux_env_string a t)^","^ (aux_env_string a' t')) ^ "," ^(aux tail)
  in 
  "[" ^ (aux el) ^"]"
;; 

let print_env (el:env) = Printf.printf "%s \n" (env_to_string el);;
   

let var_counter_ptype : int ref = ref 0;; 

let new_var_ptype () : string = var_counter_ptype := ! var_counter_ptype + 1;
"T"^( string_of_int ! var_counter_ptype );; 

(* Recherche dans l'environnement *)
let rec search_type (v : string) (e : env) : ptype =
  match e with
  | [] -> failwith ("Variable non trouvée: " ^ v)
  | (v', t)::rest when  v = v' -> t
  | (_,_)::rest -> search_type v rest
;;

(* Génération d'équations de typage à partir d'un terme *)
let rec generate_equa (te : Ast.pterm) (ty : ptype) (env : env) : equas =
  match te with
  | Var v -> 
      (* Si le terme est une variable, on génère l'équation T_v = T *)
      let t_v = search_type v env in
      [(t_v, ty)]
  
  | Abs (x, body) -> 
      (* Si le terme est une abstraction, on prend deux variables de type fraiches *)
      let ta = VarType (new_var_ptype ()) in
      let tr = VarType (new_var_ptype ()) in
      let env' = (x, ta) :: env in
      (* On génère l'équation T = Ta -> Tr *)
      let eq1 = (ty, Arrow (ta, tr)) in
      (* On génère récursivement les équations du corps avec le type Tr *)
      let eqs_body = generate_equa body tr env' in
      eq1 :: eqs_body
  
  | App (t1, t2) -> 
      (* Si le terme est une application, on prend une variable de type fraiche Ta *)
      let ta = VarType (new_var_ptype ()) in
      (* On génère les équations pour t1 avec le type cible Ta -> T *)
      let eqs_t1 = generate_equa t1 (Arrow (ta, ty)) env in
      (* On génère les équations pour t2 avec le type cible Ta *)
      let eqs_t2 = generate_equa t2 ta env in
      eqs_t1 @ eqs_t2
;;

(* Fonction occur_check : vérifie si la variable `v` est présente dans le type `t` *)
let rec occur_check (v : string) (t : ptype) : bool =
  match t with
  | VarType x -> x = v
  | Arrow (t1, t2) -> (occur_check v t1) || (occur_check v t2)
  | Nat -> false
;;

(* ! unification  *)
(* Fonction de substitution d'une variable de type par un type dans un autre type *)
let rec substitute_type (v : string) (substitution : ptype) (t : ptype) : ptype =
  match t with
  | VarType x when x = v -> substitution (* Remplace si la variable est trouvée *)
  | VarType _ -> t                      (* Si ce n'est pas la variable, ne rien changer *)
  | Arrow (t1, t2) -> Arrow (substitute_type v substitution t1, substitute_type v substitution t2) 
  (* Substitue récursivement dans les deux sous-types de la flèche *)
  | Nat -> t                            (* Pas de substitution dans le type Nat *)
;;

(* Fonction de substitution dans une équation de typage *)
let substitute_in_equation (v : string) (type_of_substitution : ptype) ((t1, t2) : equa) : equa =
  (substitute_type v type_of_substitution t1, substitute_type v type_of_substitution t2)
;;

(* Fonction de substitution dans un système d'équations *)
let rec substitute_in_system (v : string) (type_of_substitution : ptype) (eqs : equas) : equas =
  List.map (substitute_in_equation v type_of_substitution) eqs
;;

(* Fonction d'unification pour une étape avec accumulateur de substitutions *)
let rec unify_step (eqs : equas) (substitutions_acc : env) : (equas * env) =
  match eqs with
  | [] ->  ([], substitutions_acc)  (* S'il n'y a plus d'équations, retournez la liste vide et les substitutions *)
  
  | (t1, t2)::rest when t1 = t2 -> 
      (* Si les deux types sont identiques, on supprime l'équation *)
      unify_step rest substitutions_acc
  
  | (VarType x, td) :: rest when not (occur_check x td) -> 
      (* Si le type gauche est une variable qui n'apparaît pas dans le type droit *)
      let substituted_system = substitute_in_system x td rest in
      let new_substitutions = (x, td) :: (List.map (fun (v, t) -> (v, substitute_type x td t)) substitutions_acc) in
      unify_step substituted_system new_substitutions
  
  | (td, VarType x) :: rest when not (occur_check x td) -> 
      let substituted_system = substitute_in_system x td rest in
      let new_substitutions = (x, td) :: (List.map (fun (v, t) -> (v, substitute_type x td t)) substitutions_acc) in
      unify_step substituted_system new_substitutions
  
  | (Arrow (t1_l, t1_r), Arrow (t2_l, t2_r)) :: rest -> 
      (* Si les deux types sont des flèches, on ajoute les équations pour les parties gauche et droite *)
      Printf.printf "%s = %s\n" (ptype_to_string (Arrow (t1_l, t1_r))) (ptype_to_string (Arrow (t2_l, t2_r)));
      unify_step ((t1_l, t2_l) :: (t1_r, t2_r) :: rest) substitutions_acc
  
  | _ -> failwith "Unification failed"  (* Sinon, on échoue *)
;;
(* Appliquer toutes les substitutions à un type *)
let rec apply_substitutions (t : ptype) (substitutions_acc : env) : ptype =
  match t with
  | VarType x -> 
      (try 
         let t' = List.assoc x substitutions_acc in
         apply_substitutions t' substitutions_acc
       with Not_found -> VarType x)
  | Arrow (t1, t2) -> Arrow (apply_substitutions t1 substitutions_acc, apply_substitutions t2 substitutions_acc)
  | Nat -> Nat
;;
(* Fonction pour mesurer le temps d'exécution avec Sys.time *)
let timeout f timeout_duration =
  let start_time = Sys.time () in
  let rec loop () =
    if (Sys.time () -. start_time) > timeout_duration then
      None  (* Timeout atteint *)
    else
      match f () with
      | Some result -> Some result  (* Résolution trouvée avant le timeout *)
      | None -> loop ()  (* Continuer à essayer *)
  in
  loop ()
;;


(* Fonction principale pour résoudre un système d'équations *)
let rec solve_system (eqs : equas) (substitutions_acc : env) : (equas * env) option =
  match eqs with
  | [] -> Some ([], substitutions_acc)  (* Le système est résolu si la liste est vide *)
  | _ -> 
      try
        (* Appel à la fonction d'unification pour une étape *)
        let (next_eqs, new_substitutions) = unify_step eqs substitutions_acc in
        solve_system next_eqs new_substitutions  (* Résoudre récursivement le système *)
      with Failure _ -> None  (* En cas d'échec d'unification, retourner None *)
;;

(* Fonction qui résout un système avec timeout et substitutions *)
let solve_with_timeout (eqs : equas) (timeout_duration : float) : (equas * env) option =
  timeout (fun () -> solve_system eqs []) timeout_duration
;;

(* Fonction d'inférence de type pour un terme *)
let infer_type (term : Ast.pterm) (env : env) (timeout_duration : float) : ptype option =
  (* Générer une variable de type fraîche pour le terme *)
  let fresh_ty = VarType (new_var_ptype ()) in
  (* Générer les équations de typage pour le terme *)
  let eqs = generate_equa term fresh_ty env in
  (* Résoudre les équations avec un timeout *)
  match solve_with_timeout eqs timeout_duration with
  | Some (_, substitutions) -> 
      (* Appliquer les substitutions au type fraîchement généré *)
      Some (apply_substitutions fresh_ty substitutions)
  | None -> None  (* Si les équations ne peuvent être résolues, retourner None *)
;;

let infer_type_  (term : Ast.pterm) : ptype = 
  match infer_type term [] 2.0 with 
  | Some t -> t 
  | _ -> failwith "Could not infer type "
;;  