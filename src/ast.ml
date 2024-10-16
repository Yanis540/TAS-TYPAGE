

type 'a liste = Empty | Cons of 'a *'a liste;;
(*  Termes  *)
type pterm = Var of string 
  | App of pterm * pterm  
  | Abs of string * pterm
  (* 4.1 : entiers *)
  | Int of int 
  | Add of pterm * pterm 
  | Sub of pterm * pterm 
  (* 4.1 : list *)
  | List of pterm liste  
  | Head of pterm  
  | Tail of pterm  
  (* 4.1 : If *)
  | IfZero of pterm * pterm * pterm  
  | IfEmpty of pterm * pterm * pterm  
  (* 4.1 : Point fix *)
  | Fix  of string * pterm *pterm
  (* 4.1 : Let *)
  | Let  of string * pterm *pterm
   

;;
let rec liste_to_string (lst: pterm liste) : string =
  let rec aux lst'= 
    (match lst' with 
    |  Empty-> "" 
    |  Cons (hd,tail)-> (pterm_to_string hd) ^","^ (aux tail) 
    )  
  in "[" ^(aux lst)^ "]"  
and pterm_to_string (t : pterm) : string =
  match t with
  Var x -> x
  | App (t1 , t2) -> "App ( " ^ ( pterm_to_string t1) ^" , "^ ( pterm_to_string t2) ^ " )"
  | Abs (x, t) -> "( fun "^ x ^" -> " ^ ( pterm_to_string t) ^" )"
  (* 4.1 *)
  | Int n -> string_of_int n
  | Add (t1, t2) -> "Add ( " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | Sub (t1, t2) -> "Subs ( " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | List lst -> liste_to_string lst
  | Head t -> "Head ( " ^ (pterm_to_string t) ^ " )"
  | Tail t -> "Tail ( " ^ (pterm_to_string t) ^ " )"
  | IfZero (cond, t1, t2) -> "IfZero ( " ^ (pterm_to_string cond) ^ " , " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | IfEmpty (cond, t1, t2) -> "IfEmpty ( " ^ (pterm_to_string cond) ^ " , " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | Fix (var, t1, t2) -> "Fix ( " ^ var ^ " , " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | Let (var, t1, t2) -> "Let ( " ^ var ^ " = " ^ (pterm_to_string t1) ^ " in " ^ (pterm_to_string t2) ^ " )"
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
type binding = string * string;; 
type  rename_binding = binding list ;; 
let acc_element_to_string (a:binding) = 
  match a with 
  | (o,n)->"( "^o^", "^n^" )" 
let rec acc_to_string (acc:rename_binding) :string = 
  match acc with 
  | []->""
  | [a] -> acc_element_to_string a 
  | a::[q] -> acc_element_to_string a ^","^ acc_element_to_string q 
  | a::q ::tail -> (acc_element_to_string a ^","^ acc_element_to_string q ) ^ "," ^(acc_to_string tail)
;;
let print_acc (acc:(string*string)list ) = 
  Printf.printf "[ %s ]\n" (acc_to_string acc)
;;
let rec alpha_conv (t:pterm)  (acc:rename_binding): pterm = 
 
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
  (* 4.1 : entier*)
  | Int(n) -> Int n 
  | Add(t1,t2) -> Add(alpha_conv t1 acc, alpha_conv t2 acc)
  | Sub(t1,t2) -> Sub(alpha_conv t1 acc, alpha_conv t2 acc)
  (* 4.1 : list *)
  | List l  ->  List(alpha_conv_liste l acc) 
  | Head(t) -> Head(alpha_conv t acc)
  | Tail(t) -> Tail(alpha_conv t acc)
  (* 4.1 : if *)
  | IfZero(cond,cons,alt)-> IfZero(alpha_conv cond acc ,alpha_conv cons acc,alpha_conv alt acc)
  | IfEmpty(cond,cons,alt)-> IfEmpty(alpha_conv cond acc ,alpha_conv cons acc,alpha_conv alt acc)
  (* 4.1 : Point fix *)
  | Fix(x,t1,t2)-> Fix(x,alpha_conv t1 acc,alpha_conv t2 acc)
  (* 4.1 : Let *)
  | Let(x,t1,t2)-> 
      let new_var_name= new_var() in 
      let acc' = (x,new_var_name)::acc in 
      Let(new_var_name,(alpha_conv t1 acc'),(alpha_conv t2 acc'))

(* alpha conversion spéciale pour les listes *)
and alpha_conv_liste (lst : pterm liste ) (acc:rename_binding) : pterm liste = 
  let rec aux (lst : pterm liste ) (acc:rename_binding) : pterm liste= (match lst with
    | Empty -> Empty 
    | Cons (hd,tail) -> 
        let hd' = alpha_conv hd acc in 
        let tail' = aux tail acc in 
        Cons(hd',tail') 
  ) in 
  aux lst acc
;; 
let print_string_list (l:string list) = 
  Printf.printf "[ %s ]\n" (String.concat ", " l)
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

  (* 4.1 : entier *)
  | Int n -> Int n  (* Les entiers ne nécessitent pas de substitution *)
  | Add (t1, t2) -> Add (substitution x nterm t1, substitution x nterm t2)  (* Substitution dans les additions *)
  | Sub (t1, t2) -> Sub (substitution x nterm t1, substitution x nterm t2)  (* Substitution dans les soustractions *)
  (* 4.1 : list *)
  | List l -> List (substitution_liste x nterm l)  (* Substitution dans les listes *)
  | Head t -> Head (substitution x nterm t)  (* Substitution dans l'opération Head *)
  | Tail t -> Tail (substitution x nterm t)  (* Substitution dans l'opération Tail *)
  (* 4.1 : If *)
  | IfZero (cond, cons, alt) -> 
      let cond' = substitution x nterm cond in 
      let cons' = substitution x nterm cons in 
      let alt' = substitution x nterm alt in 
      IfZero (cond', cons', alt')

  | IfEmpty (cond, cons, alt) ->
      let cond' = substitution x nterm cond in 
      let cons' = substitution x nterm cons in 
      let alt' = substitution x nterm alt in 
      IfEmpty (cond', cons', alt')
  (* 4.1 : Point fix *)
  | Fix (phi, body, m) -> Fix (phi, (substitution x nterm body), m)
  |  Let (y, e1, e2) ->
      let e1' = substitution x nterm e1 in
      if y = x then
        Let (y, e1', e2)  (* Pas de substitution dans e2 car y est lié *)
      else
        Let (y, e1', substitution x nterm e2)

  
(* Fonction auxiliaire pour la substitution dans une liste de termes *)
and substitution_liste (x: string) (nterm: pterm) (lst: pterm liste) : pterm liste =
  match lst with
  | Empty -> Empty  (* Liste vide : pas de substitution *)
  | Cons (hd, tail) -> Cons (substitution x nterm hd, substitution_liste x nterm tail)
;;

(* Fonction pour vérifier si un terme est une valeur *)
let rec is_value (t: pterm) : bool =
  match t with
  | Var(x ) -> true
  | Int _ -> true
  | List _ -> true
  | Abs (_, _) -> true
  | App ( Var(x) , t' ) ->  is_value (t') 
  | _ -> false   
;;

(* ! Evaluation  *)
(* Fonction de réduction LtR-CbV *)
(* Fonction de réduction LtR-CbV mise à jour *)
let rec ltr_ctb_step (t : pterm) : pterm option =
  match t with
  (* Beta reduction *)
  | App (Abs (x, body), v) when is_value v ->
    Some (substitution x v body)
  | App (m, n) ->
      (match ltr_ctb_step m with
      (* M -> M' => M N -> M' N *)
      | Some m' -> Some (App (m', n))
      | None when (is_value m) = false ->None
      | _  ->
          (* Si la partie gauche est déjà une valeur, on essaie de réduire la partie droite *)
          (
          match ltr_ctb_step n with
          | Some n' -> Some (App (m, n'))
          | None -> None)
        )
  | _ -> None  (* Une variable ne peut pas être réduite *)

;;


(* Fonction pour effectuer des réductions multiples jusqu'à atteindre la forme normale *)
let rec ltr_cbv_norm (t : pterm) : pterm =
  match ltr_ctb_step t with
  | Some t' -> ltr_cbv_norm t'
  | None -> t
;;
(* Fonction de normalisation avec timeout (limite de nombre d'étapes) *)
let rec ltr_cbv_norm_timeout (t : pterm) (time_limit : float) : pterm option =
  let start_time = Sys.time () in
  let rec norm t =
    if Sys.time () -. start_time > time_limit then
      None  (* Timeout atteint *)
    else
      match ltr_ctb_step t with
      | Some t' -> norm t'
      | None -> Some t  (* Terminé, forme normale atteinte *)
  in
  norm t
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