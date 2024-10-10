
open Ast;;
type ptype =  VarType of string |Arrow of ptype * ptype | Nat
and env = (string * ptype) list  
and equa = ptype * ptype
and equas = (equa) list;; 

let rec ptype_to_string (t : ptype) : string =
  match t with
  VarType x -> x
  | Arrow (t1 , t2) -> "(" ^ ( ptype_to_string t1) ^" -> "^ ( ptype_to_string t2) ^")"
  | Nat -> "Natural"
;; 
let print_ptype  (t:ptype)  = Printf.printf "%s" (ptype_to_string t) ;; 
let equa_to_string (e:equa) =
  let (t1,t2)= e in 
  (ptype_to_string t1)^" = "^ (ptype_to_string t2)
;; 
let equas_to_string (el:equa list) : string =
  let rec aux el = 
    match el with 
    | []->""
    | [a] -> equa_to_string a 
    | a::[q] -> equa_to_string a ^","^ equa_to_string q 
    | a::q ::tail -> (equa_to_string a ^","^ equa_to_string q ) ^ "," ^(aux tail)
  in 
  "[" ^ (aux el) ^"]"
;; 

   

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