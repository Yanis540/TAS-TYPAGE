



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

let rec substitution (x:string) (nterm:pterm) (t:pterm)  : pterm  = 
  match t with
  | Var y ->  if y = x then nterm else t  
  | App (t1, t2) ->  App (substitution x nterm t1 , substitution x nterm t2   )
  | Abs (y, m) ->   
      if y<>x then 
        Abs (y, substitution x nterm m  )
      else
        (* en gros l'idée c'est d'essayer de remplacer X par nterm   *)
        (* let nv = new_var() in *)
        (* Printf.printf "Trying to replace (%s) with new var (%s) : " x nv;   *)
        print_pterm nterm;
        substitution x (nterm) (m)
          
;;