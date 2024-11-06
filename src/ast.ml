

type 'a liste = Empty | Cons of 'a *'a liste
and pterm_list = pterm liste
(*  Termes  *)
and pterm = Var of string 
  | App of pterm * pterm  
  | Abs of string * pterm
  (* 4.1 : entiers *)
  | Int of int 
  | Add of pterm * pterm 
  | Sub of pterm * pterm 
  | Mult of pterm * pterm 
  (* 4.1 : list *)
  | List of pterm_list  
  | Head of pterm  
  | Tail of pterm  
  (* 4.1 : If *)
  | IfZero of pterm * pterm * pterm  
  | IfEmpty of pterm * pterm * pterm  
  (* 4.1 : Point fix *)
  | Fix  of pterm
  (* 4.1 : Let *)
  | Let  of string * pterm *pterm
  (* 5.1 : Unit *)
  | Unit 
  (* 5.1 : Ref *)
  | Ref of pterm
  | Address of address
  | DeRef of pterm
  | Assign of pterm * pterm
and address = int
and binding  = address * pterm
and memory = (binding) list;;

let address_counter = ref (-1);;

let new_address () : address =
  address_counter := !address_counter + 1;
  !address_counter
;;
