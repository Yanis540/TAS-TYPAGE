(* let rec substitution_v2 (x:string) (nterm:pterm) (t:pterm)  : pterm  = 
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
          
;; *)
(* let rec free_vars (t:pterm) : string list = 
  match t with
  | Var x -> [x]
  | App (t1, t2) -> free_vars t1 @ free_vars t2
  | Abs (x, m) -> List.filter (fun y -> y <> x) (free_vars m)
  (* 4.1 : entiers *)
  | Int _ -> []  (* Les entiers n'ont pas de variables libres *)
  | Add (t1, t2) -> free_vars t1 @ free_vars t2
  | Sub (t1, t2) -> free_vars t1 @ free_vars t2
  (* 4.1 : listes *)
  | List lst -> free_vars_list lst
  | Head t -> free_vars t
  | Tail t -> free_vars t
  (* 4.1 : conditions *)
  | IfZero (cond, cons, alt) -> free_vars cond @ free_vars cons @ free_vars alt
  | IfEmpty (cond, cons, alt) -> free_vars cond @ free_vars cons @ free_vars alt
  (* 4.1 : point fixe *)
  | Fix (x, t1, t2) -> free_vars t1 @ (List.filter (fun y -> y <> x) (free_vars t2))
  (* 4.1 : let *)
  | Let (x, t1, t2) -> free_vars t1 @ (List.filter (fun y -> y <> x) (free_vars t2))

(* Fonction auxiliaire pour récupérer les variables libres dans une liste *)
and free_vars_list (lst : pterm liste) : string list = 
  match lst with
  | Empty -> []
  | Cons (hd, tail) -> free_vars hd @ free_vars_list tail
;; *)

(* ! fonctionne pour les addition mais ne marche pas comme ltrcbv *)
let rec ltr_ctb_step (t : pterm) : pterm option =  
  match t with 
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
     ) *)