
open Ast;;
type ptype =  
  VarType of string 
  | Arrow of ptype * ptype 
  | Nat
  | N
  | ListType of ptype
  | Forall of string list * ptype 
  (* 5.2 *)
  | RefType of ptype
  | UnitType
  | AddressType                     
  and env = (string * ptype) list  
and equa = ptype * ptype
and equas = (equa) list
;; 

let rec ptype_to_string (t : ptype) : string =
  match t with
  VarType x -> x
  | Arrow (t1 , t2) -> "(" ^ ( ptype_to_string t1) ^" -> "^ ( ptype_to_string t2) ^")"
  | Nat -> "Natural"
  | N -> "N"
  | ListType t -> "["^ (ptype_to_string t) ^"]"
  | Forall (vars, t) -> 
    let vars_str = String.concat ", " vars in
    "∀" ^ vars_str ^ ". " ^ (ptype_to_string t)
  | RefType t -> "ref ("^ (ptype_to_string t) ^")"
  | UnitType  -> "unit "
  | AddressType  -> "address "
  
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

(* Fonction pour obtenir les variables libres dans un type *)
let rec free_vars (t : ptype) : string list =
  match t with
  | VarType x -> [x]
  | Arrow (t1, t2) -> List.append (free_vars t1) (free_vars t2)
  | Nat  -> []
  (* 4.2 *)
  | N  -> []
  | ListType t -> free_vars t
  | Forall (vars, t) -> List.filter (fun v -> not (List.mem v vars)) (free_vars t)
  (* 5.2 *)
  | UnitType  -> []
  | RefType t -> free_vars t
  | AddressType -> []
;;

(* Fonction pour généraliser un type en ajoutant ∀ autour des variables libres non présentes dans l'environnement *)
let generalize (t: ptype) (env: env) : ptype =
  let env_vars = List.map fst env in
  let free_vars_in_t = List.filter (fun v -> not (List.mem v env_vars)) (free_vars t) in
  match free_vars_in_t with
  | [] -> t  
  | vars -> Forall (vars, t)  (* Ajout de ∀ pour chaque variable libre *)
;;
let rec rename_vars (t : ptype) (renamings : (string * string) list) : ptype =
  match t with
  | VarType x -> 
      (try VarType (List.assoc x renamings) with Not_found -> VarType x)
  | Arrow (t1, t2) -> Arrow (rename_vars t1 renamings, rename_vars t2 renamings)
  | Nat 
  (* 4.2 *)
  | N -> t
  | ListType t -> ListType (rename_vars t renamings)
  | Forall (vars, t') -> 
      (* Générer de nouveaux noms pour les variables liées *)
      let new_vars = List.map (fun _ -> new_var_ptype ()) vars in
      let new_renamings = List.combine vars new_vars in
      Forall (new_vars, rename_vars t' (new_renamings @ renamings))
  | UnitType -> t
  | RefType t'-> RefType (rename_vars t' renamings)
  | AddressType-> t
;;

(* Ouvrir un type quantifié universellement (enlever le ∀) *)
let open_forall (t: ptype) : ptype =
  match t with
  | Forall (_, t') -> t'  (* On enlève le ∀ et on retourne le type sous-jacent *)
  | _ -> t  (* Pas de ∀, on retourne le type tel quel *)
;;
(* Fonction occur_check : vérifie si la variable `v` est présente dans le type `t` *)
let rec occur_check (v : string) (t : ptype) : bool =
  match t with
  | VarType x -> x = v
  | Arrow (t1, t2) -> (occur_check v t1) || (occur_check v t2)
  | Nat -> false
  (* 4.2 *)
  | N-> false
  | ListType t-> occur_check v t
  | Forall (_,t) -> (occur_check v t)
  (* 5.2 *)
  | UnitType -> false 
  | RefType t' -> occur_check v t' 
  | AddressType -> false 
;;

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
  (* 4.2 Entiers  *)
  | Int _ -> [(ty, N)]  (* Les entiers ont toujours le type N *)
  | Add (t1, t2) -> 
    let eqs_t1 = generate_equa t1 N env in
    let eqs_t2 = generate_equa t2 N env in
    (ty, N) :: eqs_t1 @ eqs_t2 
  | Sub (t1, t2) -> 
    let eqs_t1 = generate_equa t1 N env in
    let eqs_t2 = generate_equa t2 N env in
    (ty, N) :: eqs_t1 @ eqs_t2 
  | Mult (t1, t2) -> 
    let eqs_t1 = generate_equa t1 N env in
    let eqs_t2 = generate_equa t2 N env in
    (ty, N) :: eqs_t1 @ eqs_t2 
  (* 4.2 List *)
  (* pour les liste, comme je ne veux pas trop nous faire chier, je considère que tout les éléments d'une liste 
    doit avoir le même type (sinon on devrait implémenter le SumType ce qui encore plus chiant mdr) *)
  | List lst ->
    let ta = VarType (new_var_ptype ()) in
    (* Générer les équations pour chaque élément de la liste : *)
    (* ! TODO : utilsier le résultat de cette fonction avec le SumType *)
    let rec generate_list_equa (lst: pterm liste) (ty_elem: ptype) : equas =
      match lst with
      | Empty -> []  
      | Cons (hd, tl) -> 
          let eq_hd = generate_equa hd ty_elem env in
          let eq_tl = generate_list_equa tl ty_elem in
          eq_hd @ eq_tl
    in
    let eqs_list = generate_list_equa lst ta in
    (ty, ListType ta) :: eqs_list
  | Head t -> 
    let ta = VarType (new_var_ptype ()) in
    let eqs_t = generate_equa t (ListType ta) env in 
    (ty, ta) :: eqs_t  
  | Tail t -> 
    let ta = VarType (new_var_ptype ()) in
    let eqs_t = generate_equa t (ListType ta) env in 
    (ty, ta) :: eqs_t  

  (* 4.2 If *)
  | IfZero (cond, cons, alt) -> 
    let eqs_cond = generate_equa cond N env in  (* La condition doit être un entier *)
    let eqs_cons = generate_equa cons ty env in  (* Le type du conséquent doit être le même que le type cible *)
    let eqs_alt = generate_equa alt ty env in    (* Le type de l'alternant doit être le même que le type cible *)
    eqs_cond @ eqs_cons @ eqs_alt
  | IfEmpty (cond, cons, alt) -> 
    let ta = VarType (new_var_ptype ()) in
    let eqs_cond = generate_equa cond (ListType ta) env in  (* La condition doit être une liste *)
    let eqs_cons = generate_equa cons ty env in  (* Le type du conséquent doit être le même que le type cible *)
    let eqs_alt = generate_equa alt ty env in    (* Le type de l'alternant doit être le même que le type cible *)
    eqs_cond @ eqs_cons @ eqs_alt
  (* 4.2 Fix *)
  (* Fix  *)
  | Fix(Abs (phi, m)) -> 
    (*  
    *     Γ[ϕ : T → U] ⊢ M : T → U
      ------------------------------
    *     Γ ⊢ fix λϕ.M : T → U 
    *)
    let type_T = VarType (new_var_ptype ()) in
    let type_U = VarType (new_var_ptype ()) in
    let env' = (phi, Arrow (type_T, type_U)) :: env in
    let eqs_m = generate_equa m (Arrow (type_T, type_U)) env' in
    (ty, Arrow (type_T, type_U)) :: eqs_m
  | Fix(t) -> failwith ("Fix should receive an Abstraction (lambda function )" ) 

  (* 4.2 Let *)
  | Let (x, e1, e2) -> 
    (* Inférer le type de e1 *)
    let t0 = infer_type e1 env 2.0  in
    (* Généraliser t0 : obtenir ∀X1, ..., Xk.T0 *)
    let gen_t0 = match t0 with
      | Some t -> generalize t env
      | None -> failwith ("Type inference failed for value for var"^ x)
    in
    (* Ajouter x avec le type généralisé à l'environnement et générer des équations pour e2 *)
    let env' = (x, gen_t0) :: env in
    generate_equa e2 ty env'
  (* 5.2 *)
  | Unit -> [(ty,UnitType)] 
  (* | Address a -> [(UnitType,ty)]  *)
  | Ref(m) -> 
    let t_type = VarType (new_var_ptype ()) in
    let eqs_m = generate_equa m t_type env in
    (ty, RefType t_type) :: eqs_m
  | DeRef(m) -> 
    let t_type = VarType (new_var_ptype ()) in
    let eqs_m = generate_equa m (RefType t_type) env in
    (ty, t_type) :: eqs_m
  | Address _ -> [(ty, AddressType)]  
  | Assign(e1,e2) -> 
    let te_type = VarType (new_var_ptype ()) in
    let eqs_e1 = generate_equa e1 (RefType te_type) env in
    let eqs_e2 = generate_equa e2 te_type env in
    (ty, UnitType) :: eqs_e1 @ eqs_e2
    


(* ! unification  *)
(* Fonction de substitution d'une variable de type par un type dans un autre type *)
and substitute_type (v : string) (substitution : ptype) (t : ptype) : ptype =
  match t with
  | VarType x when x = v -> substitution (* Remplace si la variable est trouvée *)
  | VarType _ -> t                      (* Si ce n'est pas la variable, ne rien changer *)
  | Arrow (t1, t2) -> Arrow (substitute_type v substitution t1, substitute_type v substitution t2) 
  (* Substitue récursivement dans les deux sous-types de la flèche *)
  | Nat | N-> t
  | ListType t -> 
    ListType (substitute_type v substitution t)  
  | Forall (vars, t') ->
    if List.mem v vars then 
      t  
    else 
      let new_t = substitute_type v substitution t' in
      Forall (vars, new_t)  
  | UnitType -> t 
  | RefType t'-> RefType(substitute_type v substitution t') 
  | AddressType -> AddressType   (* Pas de substitution dans AddressType *)
(* Fonction de substitution dans une équation de typage *)
and substitute_in_equation (v : string) (type_of_substitution : ptype) ((t1, t2) : equa) : equa =
  (substitute_type v type_of_substitution t1, substitute_type v type_of_substitution t2)


(* Fonction de substitution dans un système d'équations *)
and substitute_in_system (v : string) (type_of_substitution : ptype) (eqs : equas) : equas =
  List.map (substitute_in_equation v type_of_substitution) eqs


(* Fonction d'unification pour une étape avec accumulateur de substitutions *)
and unify_step (eqs : equas) (substitutions_acc : env) : (equas * env) =
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
  | (ListType t1, ListType t2) :: rest -> 
    (* Si les deux types sont des listes, on unifie leurs éléments *)
    unify_step ((t1, t2) :: rest) substitutions_acc
  | (Forall (vars, t1'), t2) :: rest ->
      (* Barendregtisation et ouverture du ∀ *)
      let t1_renamed = rename_vars t1' [] in
      let t1_open = open_forall t1_renamed in
      unify_step ((t1_open, t2) :: rest) substitutions_acc
    
  | (t1, Forall (vars, t2')) :: rest ->
      (* Barendregtisation et ouverture du ∀ *)
      let t2_renamed = rename_vars t2' [] in
      let t2_open = open_forall t2_renamed in
      unify_step ((t1, t2_open) :: rest) substitutions_acc
  
  (* 5.2 *)
  | (UnitType, UnitType) :: rest ->
    unify_step rest substitutions_acc

  | (RefType t1, RefType t2) :: rest ->
    (* si RefType t1 == RefType t2 => t1 == t2 *)
    unify_step ((t1, t2) :: rest) substitutions_acc
    
  | _ -> failwith "Unification failed"  (* Sinon, on échoue *)

(* Appliquer toutes les substitutions à un type *)
and apply_substitutions (t : ptype) (substitutions_acc : env) : ptype =
  match t with
  | VarType x -> 
      (try 
         let t' = List.assoc x substitutions_acc in
         apply_substitutions t' substitutions_acc
       with Not_found -> VarType x)
  | Arrow (t1, t2) -> Arrow (apply_substitutions t1 substitutions_acc, apply_substitutions t2 substitutions_acc)
  | Nat | N -> N
  | ListType t -> 
    ListType (apply_substitutions t substitutions_acc)  (* Substituer dans le type des éléments de la liste *)
  | Forall (vars, t') ->
      (* Filtrer les substitutions pour enlever celles qui concernent les variables liées dans le ∀ *)
      let filtered_substitutions = List.filter (fun (x, _) -> not (List.mem x vars)) substitutions_acc in
      Forall (vars, apply_substitutions t' filtered_substitutions)  
  (* 5.2 *)
  | UnitType -> t
  | RefType t' ->  RefType (apply_substitutions t' substitutions_acc)   
  | AddressType -> t  (* AddressType reste inchangé *)
(* Fonction pour mesurer le temps d'exécution avec Sys.time *)
and timeout f timeout_duration =
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



(* Fonction principale pour résoudre un système d'équations *)
and solve_system (eqs : equas) (substitutions_acc : env) : (equas * env) option =
  match eqs with
  | [] -> Some ([], substitutions_acc)  (* Le système est résolu si la liste est vide *)
  | _ -> 
      try
        (* Appel à la fonction d'unification pour une étape *)
        let (next_eqs, new_substitutions) = unify_step eqs substitutions_acc in
        solve_system next_eqs new_substitutions  (* Résoudre récursivement le système *)
      with Failure e -> None  (* En cas d'échec d'unification, retourner None *)


(* Fonction qui résout un système avec timeout et substitutions *)
and solve_with_timeout (eqs : equas) (timeout_duration : float) : (equas * env) option =
  timeout (fun () -> solve_system eqs []) timeout_duration


(* Fonction d'inférence de type pour un terme *)
and infer_type (term : Ast.pterm) (env : env) (timeout_duration : float) : ptype option =
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
  match infer_type term [] 1.0 with 
  | Some t -> t 
  | _ -> failwith "Could not infer type "
;;  