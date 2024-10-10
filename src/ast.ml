



(*  Termes  *)
type pterm = Var of string 
  | App of pterm * pterm  
  | Abs of string * pterm
;;

let rec pterm_to_string (t : pterm) : string =
  match t with
  Var x -> x
  | App (t1 , t2) -> "App ( " ^ ( pterm_to_string t1) ^" , "^ ( pterm_to_string t2) ^ " )"
  | Abs (x, t) -> "( fun "^ x ^" -> " ^ ( pterm_to_string t) ^" )"
;;

let print_pterm (t:pterm) = 
  Printf.printf "%s \n" (pterm_to_string t);;

let var_counter:int ref = ref 0;; 

let new_var (): string = var_counter := !var_counter +1; 
  "X"^(string_of_int !var_counter)
;; 


(*
  * acc : (old_variable_name,new_variable_name)
*)
let acc_element_to_string (a:string*string) = 
  match a with 
  | (o,n)->"( "^o^", "^n^" )" 
let rec acc_to_string (acc:(string*string) list) :string = 
  match acc with 
  | []->""
  | [a] -> acc_element_to_string a 
  | a::[q] -> acc_element_to_string a ^","^ acc_element_to_string q 
  | a::q ::tail -> (acc_element_to_string a ^","^ acc_element_to_string q ) ^ "," ^(acc_to_string tail)
;;
let print_acc (acc:(string*string)list ) = 
  Printf.printf "[ %s ]\n" (acc_to_string acc)
;;
let rec alpha_conv (t:pterm)  (acc:(string*string) list): pterm = 
 
  match t with 
  | Var (var_name) -> (
    match acc with 
      []-> 
        let new_var_name = new_var() in 
          Var new_var_name
      | (acc_var,new_acc_var)::rest-> 
        if(acc_var = var_name) 
          then 
              Var new_acc_var 
          else 
            alpha_conv (t) (rest)
    ) 
  | App(func,arg) -> 
      let renamed_left = alpha_conv func acc in 
      let renamed_right = alpha_conv arg acc in 
      App ( renamed_left , renamed_right )  
  | Abs(var_name,body) -> 
      let new_var_name = new_var() in 
      let new_acc = (var_name,new_var_name)::acc in 
      let new_body = alpha_conv body new_acc  in 
      Abs(new_var_name,new_body)
  
;; 
let rec free_vars (t:pterm) : string list = 
  match t with
  | Var x -> [x]
  | App (t1, t2) -> free_vars t1 @ free_vars t2
  | Abs (x, m) -> List.filter (fun y -> y <> x) (free_vars m)
;;
let print_string_list (l:string list) = 
  Printf.printf "[ %s ]\n" (String.concat ", " l)
;;


let rec substitution_v2 (x:string) (nterm:pterm) (t:pterm)  : pterm  = 
  match t with
  | Var y ->  if y = x then nterm else t  
  | App (t1, t2) ->  App (substitution_v2 x nterm t1 , substitution_v2 x nterm t2   )
  | Abs (y, m) ->   
      if y=x then 
        (*  λy. t et y == x =>  pas de substitution car X est protégé par l'abstraction (la variable est liée) *)
        Abs (y,m )
      else
        (* si y est apparrait dans le nterme (c'est à dire il peut y avoir des collisions)  faut renommer la variable y dans le corps m et substituion x dans le nouveau corps *)
        if List.exists (fun v -> v = y) (free_vars nterm) then
          (* Alpha-conversion si la variable liée est présente dans le terme à substituer *)
          let nv = new_var () in
          let m' = substitution_v2 y (Var nv) m in
          Abs (nv, substitution_v2 x nterm m')
        else
          (* Sinon, on continue la substitution normalement dans le corps de l'abstraction *)
          Abs (y, substitution_v2 x nterm m)
          
;;
let rec substitution (x:string) (nterm:pterm) (t:pterm)  : pterm  = 
  match t with
  | Var y ->  if y = x then nterm else t  
  | App (t1, t2) ->  App (substitution x nterm t1 , substitution x nterm t2   )
  | Abs (y, m) ->   
      if y=x then 
        (*  λy. t et y == x =>  pas de substitution car X est protégé par l'abstraction (la variable est liée) *)
        Abs (y,m )
      else
          (* Sinon, on continue la substitution normalement dans le corps de l'abstraction *)
          Abs (y, substitution x nterm m)
          
;;

(* Fonction pour vérifier si un terme est une valeur *)
let rec is_value (t: pterm) : bool =
  match t with
  | Var(x ) -> true
  | Abs (_, _) -> true
  | App ( Var(x) , t' ) ->  is_value (t') 
  | _ -> false   
;;

(* Fonction de réduction LtR-CbV *)
(* Fonction de réduction LtR-CbV mise à jour *)
let rec ltr_ctb_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None  (* Une variable ne peut pas être réduite *)
  
  | Abs (x, body) -> 
      (* Tenter de réduire le corps de l'abstraction *)
      (match ltr_ctb_step body with
       | Some new_body -> Some (Abs (x, new_body))
       | None -> None)
  
  | App (t1, t2) ->
      if not (is_value t1) then
        (* Réduire le terme de gauche *)
        (match ltr_ctb_step t1 with
         | Some new_t1 -> Some (App (new_t1, t2))
         | None -> None)
      else if not (is_value t2) then
        (* Si le terme de gauche est une valeur, réduire le terme de droite *)
        (match ltr_ctb_step t2 with
         | Some new_t2 -> Some (App (t1, new_t2))
         | None -> None)
      else
        (* Si les deux termes sont des valeurs, tenter une Beta : réduction *)
        (match t1 with
         | Abs (x, body) -> 
             (* Effectuer la substitution *)
             Some (substitution x t2 body)
         | Var v ->
             (* Cas où t1 est une variable appliquée à une valeur *)
             (* Cela dépend de votre interprétation, généralement non réductible *)
             None
         | App (_, _) ->
             (* Si t1 est une application, vérifier si elle peut être réduite *)
             (match ltr_ctb_step t1 with
              | Some reduced_t1 -> Some (App (reduced_t1, t2))
              | None -> None)
          (* Pour les autres cas d'applications de valeurs, aucune réduction possible *)
          | _ -> None
         )
;;


(* Fonction pour effectuer des réductions multiples jusqu'à atteindre la forme normale *)
let rec ltr_cbv_norm (t : pterm) : pterm =
  match ltr_ctb_step t with
  | Some t' -> ltr_cbv_norm t'
  | None -> t
;;
(* Fonction de normalisation avec timeout (limite de nombre d'étapes) *)
let rec ltr_cbv_norm_timeout (t : pterm) (limit : int) : pterm option =
  if limit <= 0 then None
  else
    match ltr_ctb_step t with
    | Some t' -> ltr_cbv_norm_timeout t' (limit - 1)
    | None -> Some t
;;

let rec print_reduction_steps t =
  print_pterm t;
  match ltr_ctb_step t with
  | Some t' ->
      Printf.printf "=> ";
      print_reduction_steps t'
  | None ->
      Printf.printf "=> (forme normale)\n"
;;