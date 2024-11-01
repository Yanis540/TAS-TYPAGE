
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
  | Weak of ptype                     
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
  | Weak t'  -> "weak (" ^(ptype_to_string t')^ ")"
  
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
  | Weak t' -> free_vars t'
;;

(* Fonction pour généraliser un type en ajoutant ∀ autour des variables libres non présentes dans l'environnement *)
let generalize (t: ptype) (env: env) (is_expansive: bool) : ptype =
  let env_vars = List.map fst env in
  let free_vars_in_t = List.filter (fun v -> not (List.mem v env_vars)) (free_vars t) in
  match free_vars_in_t with
  | [] -> t  (* Pas de variables libres, retourne le type tel quel *)
  | vars -> if is_expansive then Weak(Forall(vars,t)) else Forall(vars,t)
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
  | Weak t'-> Weak(rename_vars t' renamings)
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
  | Weak t' -> occur_check v t' 
;;

(* Recherche dans l'environnement *)
let rec search_type (v : string) (e : env) : ptype =
  match e with
  | [] -> failwith ("Variable non trouvée: " ^ v)
  | (v', t)::rest when  v = v' -> t
  | (_,_)::rest -> search_type v rest
;;
let contains_weak_poly (env: env) : bool =
  List.exists (fun (_, t) -> 
    match t with
    | Weak _ -> true
    | _ -> false
  ) env
;;
(* Mettre à jour les types Weak dans l'environnement après une assignation *)
let rec update_weak_types_in_env (env : env) (substitutions : env) : env =
  List.map (fun (v, t) ->
    match t with
    | Weak _ -> (v, apply_substitutions t substitutions)  (* Appliquer les substitutions aux types faibles *)
    | _ -> (v, t)
  ) env


(* Génération d'équations de typage à partir d'un terme *)
and generate_equa (te : Ast.pterm) (ty : ptype) (env : env) : equas *env =
  match te with
  | Var v -> 
      (* Si le terme est une variable, on génère l'équation T_v = T *)
      let t_v = search_type v env in
      ([(t_v, ty)],env)
  
  | Abs (x, body) -> 
      (* Si le terme est une abstraction, on prend deux variables de type fraiches *)
      let ta = VarType (new_var_ptype ()) in
      let tr = VarType (new_var_ptype ()) in
      let env' = (x, ta) :: env in
      (* On génère l'équation T = Ta -> Tr *)
      let eq1 = (ty, Arrow (ta, tr)) in
      (* On génère récursivement les équations du corps avec le type Tr *)
      let eqs_body,env'' = generate_equa body tr env' in
      (eq1 :: eqs_body,env'')
  
  | App (t1, t2) -> 
      (* Si le terme est une application, on prend une variable de type fraiche Ta *)
      if contains_weak_poly env then
        failwith "Cannot copy weakly polymorphic types in application context"
      else 
      let ta = VarType (new_var_ptype ()) in
      (* On génère les équations pour t1 avec le type cible Ta -> T *)
      let (eqs_t1,env') = generate_equa t1 (Arrow (ta, ty)) env in
      (* On génère les équations pour t2 avec le type cible Ta *)
      let (eqs_t2,env'') = generate_equa t2 ta env' in
      (eqs_t1 @ eqs_t2,env'')
  (* 4.2 Entiers  *)
  | Int _ -> ([(ty, N)],env)  (* Les entiers ont toujours le type N *)
  | Add (t1, t2) -> 
    let (eqs_t1,env') = generate_equa t1 N env in
    let (eqs_t2,env'') = generate_equa t2 N env' in
    ((ty, N) :: eqs_t1 @ eqs_t2,env'') 
  | Sub (t1, t2) -> 
    let (eqs_t1,env') = generate_equa t1 N env in
    let (eqs_t2,env'') = generate_equa t2 N env' in
    ((ty, N) :: eqs_t1 @ eqs_t2,env'')  
  | Mult (t1, t2) -> 
    let (eqs_t1,env') = generate_equa t1 N env in
    let (eqs_t2,env'') = generate_equa t2 N env' in
    ((ty, N) :: eqs_t1 @ eqs_t2,env'')  
  (* 4.2 List *)
  (* pour les liste, comme je ne veux pas trop nous faire chier, je considère que tout les éléments d'une liste 
    doit avoir le même type (sinon on devrait implémenter le SumType ce qui encore plus chiant mdr) *)
  | List lst ->
    let ta = VarType (new_var_ptype ()) in
      (match lst with
       | Empty ->(([(ty, Weak (ListType ta))]),env) (* Utilisation de `Weak` pour une liste vide *)
       | Cons (hd, tl) -> 
           let (eqs_hd,env') = generate_equa hd ta env in
           let (eqs_tl,env'') = generate_equa (List tl) (ListType ta) env' in
           ((ty, ListType ta) :: eqs_hd @ eqs_tl,env''))
  | Head t -> 
    let ta = VarType (new_var_ptype ()) in
    let (eqs_t,env') = generate_equa t (ListType ta) env in 
    ((ty, ta) :: eqs_t,env')  
  | Tail t -> 
    let ta = VarType (new_var_ptype ()) in
    let (eqs_t,env') = generate_equa t (ListType ta) env in 
    ((ty, ta) :: eqs_t,env')  

  (* 4.2 If *)
  | IfZero (cond, cons, alt) -> 
    let (eqs_cond,env') = generate_equa cond N env in  (* La condition doit être un entier *)
    let (eqs_cons,env'') = generate_equa cons ty env' in  (* Le type du conséquent doit être le même que le type cible *)
    let (eqs_alt,env''') = generate_equa alt ty env'' in    (* Le type de l'alternant doit être le même que le type cible *)
    ((eqs_cond @ eqs_cons @ eqs_alt),env''')
  | IfEmpty (cond, cons, alt) -> 
    let ta = VarType (new_var_ptype ()) in
    let (eqs_cond,env') = generate_equa cond (ListType ta) env in  (* La condition doit être une liste *)
    let (eqs_cons,env'') = generate_equa cons ty env' in  (* Le type du conséquent doit être le même que le type cible *)
    let (eqs_alt,env''') = generate_equa alt ty env'' in    (* Le type de l'alternant doit être le même que le type cible *)
    ((eqs_cond @ eqs_cons @ eqs_alt),env''')
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
    let (eqs_m,env'') = generate_equa m (Arrow (type_T, type_U)) env' in
    (((ty, Arrow (type_T, type_U)) :: eqs_m),env'')
  | Fix(t) -> failwith ("Fix should receive an Abstraction (lambda function )" ) 

  (* 4.2 Let *)
  | Let (x, e1, e2) -> 
    (* Inférer le type de e1 *)
    Printf.printf"Let term : ";print_pterm (Let(x,e1,e2)); 
    let t0 = infer_type e1 env 2.0 in
    let is_expansive = not (is_non_expansive e1) in
    (* Utiliser `Gen` ou `GenF` selon l’expansivité de `e1` *)
    let (gen_t0,env') = match t0 with
      | Some (t,env') ->
        Printf.printf"\t old env (e1) : "; print_env env;
        Printf.printf"\t new env (e1) : "; print_env env';
        if not (is_expansive  ) || not (contains_weak_poly env') then 
          ((generalize t env' is_expansive),env')
        else (
          failwith "Cannot copy weakly polymorphic types in expansive let context")
      | None -> failwith ("Type inference failed for value for var: " ^ x)
    in
    (* Ajouter x avec le type généralisé dans l'environnement *)
    let env'' = (x, gen_t0) :: env' in
    generate_equa e2 ty env''
  (* 5.2 *)
  | Unit -> ([(ty,UnitType)],env) 
  (* | Address a -> [(UnitType,ty)]  *)
  | Ref(m) -> 
    let t_type = VarType (new_var_ptype ()) in
    let (eqs_m,env') = generate_equa m t_type env in
    (((ty, RefType t_type) :: eqs_m),env')
  | DeRef(m) -> 
    let t_type = VarType (new_var_ptype ()) in
    let (eqs_m,env') = generate_equa m (RefType t_type) env in
    (((ty, t_type) :: eqs_m),env')
  | Address _ -> (([(ty, AddressType)] ),env) 
  | Assign(e1,e2) -> 
    let te_type = VarType (new_var_ptype ()) in
    let (eqs_e1,env') = generate_equa e1 (RefType te_type) env in
    let (eqs_e2,env'') = generate_equa e2 te_type env' in
    let eqs_assign = (ty,UnitType)::eqs_e1 @ eqs_e2 in
    let _, substitutions = unify_step eqs_assign [] in
    (* Mettre à jour l'environnement avec les substitutions *)
    Printf.printf"\t\tAssign : %s\n" (pterm_to_string (Assign(e1,e2)));
    Printf.printf"\t\t\tAssign Env befor assignmenet : %s\n" (env_to_string env);
    Printf.printf"\t\t\tAssign Env after assignmenet : %s\n" (env_to_string env'');
    Printf.printf"\t\t\tAssign EQS after assignmenet : %s\n" (equas_to_string eqs_assign);
    let env_updated = update_weak_types_in_env env'' substitutions in
    Printf.printf "\t\tEnv after assignment: %s\n" (env_to_string env_updated);
    (eqs_assign, env_updated)

    (* (eqs_assign, env'') *)
    (* (((ty, UnitType) :: eqs_e1 @ eqs_e2),env'') *)
    

and  is_non_expansive (term : Ast.pterm) : bool =
  match term with
  | Var _ -> true
  | Abs (_, _) -> true
  | Int _ -> true
  | Unit -> true
  | List l -> (
      match l with
      | Empty -> false
      | _ -> true 
    )
  | App (t1, t2) -> is_non_expansive t1 && is_non_expansive t2
  | Let (_, e1, e2) -> is_non_expansive e1 && is_non_expansive e2
  | Ref (_) -> true
  | _ -> false

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
  | Weak t' -> Weak (substitute_type v substitution t')   (* Pas de substitution dans AddressType *)
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
  | (Weak t1, t2) :: rest | (t2, Weak t1) :: rest -> 
      (* Cas spécial pour Weak : si une partie est Weak, remplacer Weak par le type concret lors de l'unification *)
      Printf.printf "Subs Weak t1 : %s, with t2 : %s\n" (ptype_to_string t1) (ptype_to_string t2); 
      let substituted_system = substitute_in_system (ptype_to_string t1) t2 rest in
      let new_substitutions = (ptype_to_string t1, t2) :: substitutions_acc in
      Printf.printf "Substitued system : %s \n" (equas_to_string substituted_system); 
      unify_step substituted_system new_substitutions
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
  | Weak t' -> Weak (apply_substitutions t' substitutions_acc)  (* Faible instantiation *)
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
and infer_type (term : Ast.pterm) (env : env) (timeout_duration : float) : (ptype * env) option =
  let fresh_ty = VarType (new_var_ptype ()) in
  let eqs, env' = generate_equa term fresh_ty env in
  match solve_with_timeout eqs timeout_duration with
  | Some (_, substitutions) -> 
      let final_ty = apply_substitutions fresh_ty substitutions in
      let final_env = List.map (fun (v, t) -> (v, apply_substitutions t substitutions)) env' in
      Some (final_ty, final_env)
  | None -> None
  ;;

let infer_type_  (term : Ast.pterm) : ptype = 
  match infer_type term [] 1.0 with 
  | Some (t,_) -> t 
  | _ -> failwith "Could not infer type "
;;  