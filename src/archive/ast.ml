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