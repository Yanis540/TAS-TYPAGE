open Ast;; 

(* Fonctions de base pour manipuler la mémoire *)
let rec mem_lookup  (a : address)  (mem : memory): pterm option =
  try Some (List.assoc a mem) with Not_found -> None

and mem_update  (a : address) (v : pterm)  (mem : memory): memory =
  (a, v) :: List.remove_assoc a mem
and mem_add (v : pterm) (mem:memory) : (address*memory) =
  let adr = new_address() in 
  let mem' = mem_update adr v mem in 
  (adr,mem')
and address_to_string (a:address) = string_of_int a
and binding_to_string (b:binding)= 
  match b with 
    | (a,v)->  "("^(address_to_string a)^","^(pterm_to_string v)^")" 

and mem_to_string(mem:memory) : string =
  let rec aux (mem:memory) = 
    match mem with 
    | []-> "" 
    | b::[]-> binding_to_string b
    | b1::b2::t->  (binding_to_string b1)^ ","^(binding_to_string b2) ^ ","^(aux t)
  in 
  "["^(aux mem)^"]"
and print_mem mem = Printf.printf "%s\n" (mem_to_string mem) 
and liste_to_string (lst: pterm liste) : string =
  let rec aux lst'= 
    (match lst' with 
    |  Empty-> "" 
    |  Cons (hd,Empty)-> (pterm_to_string hd)
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
  | Mult (t1, t2) -> "Mult ( " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | List lst -> liste_to_string lst
  | Head t -> "Head ( " ^ (pterm_to_string t) ^ " )"
  | Tail t -> "Tail ( " ^ (pterm_to_string t) ^ " )"
  | IfZero (cond, t1, t2) -> "IfZero ( " ^ (pterm_to_string cond) ^ " , " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | IfEmpty (cond, t1, t2) -> "IfEmpty ( " ^ (pterm_to_string cond) ^ " , " ^ (pterm_to_string t1) ^ " , " ^ (pterm_to_string t2) ^ " )"
  | Fix (f) -> "Fix ( "  ^ (pterm_to_string f) ^ " )"
  | Let (var, t1, t2) -> "Let ( " ^ var ^ " = " ^ (pterm_to_string t1) ^ " in " ^ (pterm_to_string t2) ^ " )"
  (* 5.1 *)
  | Unit -> "()"
  | Address a -> "address (" ^(address_to_string a)^")"
  | Ref r -> "ref (" ^(pterm_to_string r)^")"
  | DeRef r -> "! " ^(pterm_to_string r)
  | Assign (e1,e2) -> (pterm_to_string e1) ^":= " ^(pterm_to_string e2)
  (* 6 : Sum  *)
  | G e -> "G (" ^(pterm_to_string e)^")"
  | D e -> "D (" ^(pterm_to_string e)^")"
  | Sum(e,x,e1,e2) -> "Sum ("^(pterm_to_string e) ^","^ x ^","  ^(pterm_to_string e1) ^","^(pterm_to_string e2) ^")"

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
type rename_binding = string * string;; 
type rename_bindings = rename_binding list ;; 
let acc_element_to_string (a:rename_binding) = 
  match a with 
  | (o,n)->"( "^o^", "^n^" )" 
let rec acc_to_string (acc:rename_bindings) :string = 
  match acc with 
  | []->""
  | [a] -> acc_element_to_string a 
  | a::[q] -> acc_element_to_string a ^","^ acc_element_to_string q 
  | a::q ::tail -> (acc_element_to_string a ^","^ acc_element_to_string q ) ^ "," ^(acc_to_string tail)
;;
let print_acc (acc:(string*string)list ) = 
  Printf.printf "[ %s ]\n" (acc_to_string acc)
;;
let rec alpha_conv (t:pterm)  (acc:rename_bindings): pterm = 
 
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
  | Mult(t1,t2) -> Mult(alpha_conv t1 acc, alpha_conv t2 acc)
  (* 4.1 : list *)
  | List l  ->  List(alpha_conv_liste l acc) 
  | Head(t) -> Head(alpha_conv t acc)
  | Tail(t) -> Tail(alpha_conv t acc)
  (* 4.1 : if *)
  | IfZero(cond,cons,alt)-> IfZero(alpha_conv cond acc ,alpha_conv cons acc,alpha_conv alt acc)
  | IfEmpty(cond,cons,alt)-> IfEmpty(alpha_conv cond acc ,alpha_conv cons acc,alpha_conv alt acc)
  (* 4.1 : Point fix *)
  | Fix(f)-> Fix(alpha_conv f acc)
  (* 4.1 : Let *)
  | Let(x,t1,t2)-> 
      let new_var_name= new_var() in 
      let acc' = (x,new_var_name)::acc in 
      Let(new_var_name,(alpha_conv t1 acc'),(alpha_conv t2 acc'))
  (* 5.2 : Unit *)
  | Unit -> t 
  | Address _ -> t
  | Ref e -> Ref(alpha_conv e acc)  
  | DeRef e -> DeRef(alpha_conv e acc)  
  | Assign (e1,e2) -> Assign(alpha_conv e1 acc,alpha_conv e2 acc)  
  (* 6 : Sum  *)
  | G e -> G(alpha_conv e acc)
  | D e -> D(alpha_conv e acc)
  | Sum(m,x,n1,n2) -> Sum ((alpha_conv m acc),x,(alpha_conv n1 acc),(alpha_conv n2 acc))


(* alpha conversion spéciale pour les listes *)
and alpha_conv_liste (lst : pterm liste ) (acc:rename_bindings) : pterm liste = 
  let rec aux (lst : pterm liste ) (acc:rename_bindings) : pterm liste= (match lst with
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
  | Int n -> Int n 
  | Add (t1, t2) -> Add (substitution x nterm t1, substitution x nterm t2)  
  | Sub (t1, t2) -> Sub (substitution x nterm t1, substitution x nterm t2)  
  | Mult (t1, t2) -> Mult (substitution x nterm t1, substitution x nterm t2)  
  (* 4.1 : list *)
  | List l -> List (substitution_liste x nterm l)  
  | Head t -> Head (substitution x nterm t)  
  | Tail t -> Tail (substitution x nterm t)  
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
  | Fix (f) -> Fix (substitution x nterm f)
  | Let (y, e1, e2) ->
      let e1' = substitution x nterm e1 in
      if y = x then
        (* Pas de substitution dans e2 car y est lié *)
        Let (y, e1', e2) 
      else
        Let (y, e1', substitution x nterm e2)
  (* 5.2 : Unit *)
  | Unit -> t 
  (* 5.2 : Ref *)

  | Ref e -> Ref(substitution x nterm e) 
  | Address _ -> t 
  | DeRef e -> DeRef(substitution x nterm e) 
  | Assign (e1,e2) -> Assign(substitution x nterm e1, substitution x nterm e2) 
  (* 6 : Sum  *)
  | G e -> G(substitution x nterm e)
  | D e -> D(substitution x nterm e)
  | Sum(m,x,n1,n2) -> Sum ((substitution x nterm m),x,(substitution x nterm n1),(substitution x nterm n2))
  
  
and substitution_liste (x: string) (nterm: pterm) (lst: pterm liste) : pterm liste =
  match lst with
  | Empty -> Empty
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


let rec get_tail_list (l': pterm liste) : pterm   =  (match l' with 
  | Cons(h,Empty) -> h
  | Cons(_,tail) -> get_tail_list tail 
  | Empty -> failwith "Can not access Empty list"
) ;;
let get_head_list (l: pterm liste) : pterm   =  (match l with 
| Cons(h,_) -> h
| Empty -> failwith "Can not access Empty list"
) ;;
(* ! Evaluation  *)
(* Fonction de réduction LtR-CbV *)
let rec ltr_ctb_step (t : pterm) (mem:memory) : (pterm*memory) option =
  match t with
  (* Beta reduction *)
  | App (Abs (x, body), v) when is_value v ->
    Some ((substitution x v body),mem)
  | App (m, n) ->(
    match ltr_ctb_step m mem with
      (* M -> M' => M N -> M' N *)
      | Some (m',mem') -> Some (App (m', n),mem')
      (* Si la partie gauche est déjà une valeur, on essaie de réduire la partie droite *)
      | _  ->(
          match ltr_ctb_step n mem with
          | Some (n',mem') -> Some (App (m, n'),mem')
          (* si on arrive pas à réduire faudra vérifier que le côté à gauche est une fonction *)
          | None -> (
            match m with 
            | Abs(_,_)-> None 
            | _-> failwith "Application Left operand should be an application" 
          )
      )
    )
  (*4.1 Entier *)
  | Add (t1, t2) ->( match ltr_ctb_step t1 mem with 
    | Some (t1',mem') -> Some (Add(t1',t2),mem')
    | None -> 
      match  ltr_ctb_step t2 mem with 
        Some (t2',mem')-> Some (Add(t1,t2'),mem')
        | None -> (match (t1,t2) with 
          | (Int(n1),Int(n2))-> Some(Int(n1+n2),mem)
          | _ -> failwith "Add operands should be integers"
        )
    )
  | Sub (t1, t2) ->( match ltr_ctb_step t1 mem with 
    | Some (t1',mem') -> Some (Sub(t1',t2),mem')
    | None -> 
      match  ltr_ctb_step t2 mem with 
        Some (t2',mem')-> Some (Sub(t1,t2'),mem')
        | None -> (match (t1,t2) with 
          | (Int(n1),Int(n2))-> Some(Int(n1-n2),mem)
          | _ -> failwith "Sub operands should be integers"
        )
    )
  | Mult (t1, t2) ->( 
    match ltr_ctb_step t1 mem with 
    | Some (t1',mem') -> Some (Mult(t1',t2),mem')
    | None -> 
      match  ltr_ctb_step t2 mem with 
        Some (t2',mem')-> Some (Mult(t1,t2'),mem')
        | None -> (match (t1,t2) with 
          | (Int(n1),Int(n2))-> Some(Int(n1*n2),mem)
          | _ -> failwith "Sub operands should be integers"
        )
    )
  (*4.1 List *)
  | Head (t) -> (
      match ltr_ctb_step t mem with 
      | Some (List(l),mem') -> Some(get_head_list l,mem') 
      | Some(l',mem') -> Some(Head(l'),mem')
      | None -> (
        match t with 
        | List(l) -> Some(get_head_list l,mem)
        | _ -> failwith "Not a list"
      )
        
    )
      
  | Tail (t) -> (
    match ltr_ctb_step t mem with 
    | Some (List(l),mem') -> Some(get_tail_list l,mem') 
    | Some(l',mem') -> Some(Tail(l'),mem') 
    | None -> (
      match t with 
      | List(l) -> Some(get_tail_list l,mem)
      | _ -> failwith "Not a list"
    )
  )
  (*4.1 If *)
  | IfZero (Int(0),cons,alt) -> Some(cons,mem) 
  | IfZero (Int(n),cons,alt) -> Some(alt,mem) 
  | IfZero (cond,cons,alt) -> (
    match ltr_ctb_step cond mem with 
    | Some (cond',mem') -> Some (IfZero (cond',cons,alt) ,mem' ) 
    | None -> failwith "If condition should be an integer " )
  | IfEmpty (List(t),cons,alt) -> ( match t with 
    | Empty -> Some(cons,mem) 
    | Cons(_,_) -> Some (alt,mem)
  )
  | IfEmpty (cond,cons,alt) -> (
    match ltr_ctb_step cond mem with 
    | Some(cond',mem') -> Some(IfEmpty(cond',cons,alt),mem') 
    | None -> failwith "If condition should be a list element" 
    )
    
  (*4.1 Fix *)
    
  | Fix (Abs (x, body)) -> 
    let body'= substitution x (Fix (Abs (x, body))) body in 
    Some (body',mem)
  | Fix (m) ->(
    match ltr_ctb_step m mem with 
    | Some(m',mem') -> Some(Fix(m'),mem')
    | _ ->  failwith "Fix should be an abstraction"
  )
    
   
  (*4.1 Let *)
  | Let (x,e1,e2) -> (
      let v = (
        match ltr_ctb_step e1 mem with 
        | Some(e',m') -> (e',m')
        | None -> (e1,mem)
      ) in 
      let (e1',mem') = v in  
      let e2' =  substitution x (e1')  e2 in 
      Some(e2',mem')
    )
  | Ref m -> (
    match ltr_ctb_step m mem with 
    | Some(m',mem') -> Some(Ref(m'),mem')
    | None -> 
        let (adr,mem') = mem_add m mem in 
        Some(Address(adr),mem')
  ) 
  | DeRef (Address(a)) -> (
    match mem_lookup a mem with 
    | Some v -> Some(v,mem)  
    | None -> print_mem mem; Printf.printf "%d" !address_counter;  failwith ("Could not find memory address " ^( address_to_string a )) 
  )
  | DeRef(m)-> (
    match ltr_ctb_step m mem with 
    | Some(m',mem') -> Some(DeRef(m'),mem')
    | None -> failwith "Deref should be used with referenced variables"
    )
    
  | Assign(Address(a),n)-> 
    let mem' = mem_update a n mem in 
    Some (Unit,mem')  
  | Assign(m,n)-> (
    match ltr_ctb_step m mem with 
    (* M-> M' => M:=n;sig -> M':=N;sig *)
    | Some(m',mem') -> Some(Assign(m',n),mem')
    | None ->(
      match ltr_ctb_step n mem with 
      | Some (n',mem') -> Some(Assign(m,n'),mem') 
      | None -> failwith "Should not Occur, can't reduce left and right" (* should not occur *) 
    )
  )
   (* 6 : Sum  *)
  | G m -> (
    match ltr_ctb_step m mem with 
    | Some(m',mem') -> Some(G(m),mem')
    | None -> None
  )
  (* 6 : Sum *)
  | D m -> (
    match ltr_ctb_step m mem with 
    | Some(m',mem') -> Some(D(m),mem')
    | None -> None
  )
  | Sum(G m,x,n1,_) -> 
    Some(substitution x m n1, mem )
  | Sum(D m,x,_,n2) -> 
    Some(substitution x m n2, mem )
   
  | _ -> None  

and ltr_cbv_norm (t : pterm) (mem:memory): (pterm *memory) =
  match ltr_ctb_step t mem with
  | Some (t',mem') -> ltr_cbv_norm t' mem'
  | None -> (t,mem)
;;
(* Fonction de normalisation avec timeout (limite de nombre d'étapes) *)
let rec ltr_cbv_norm_timeout (t : pterm) (mem:memory) (time_limit : float) : (pterm *memory) option =
  let start_time = Sys.time () in
  let rec norm t mem =
    if Sys.time () -. start_time > time_limit then
      None  (* Timeout atteint *)
    else
      match ltr_ctb_step t mem with
      | Some (t',mem') -> norm t' mem'
      | None -> Some (t,mem)  
      (* Terminé, forme normale atteinte *)
  in
  norm t mem
;;
let rec ltr_cbv_norm_ (t : pterm)  : pterm =
match ltr_cbv_norm_timeout t [] 1.0 with
| Some (nf,mem) -> nf
| None -> 
    failwith "Divergence détectée (limite de réduction atteinte).\n"
;;
let rec print_reduction_steps t mem =
  print_pterm t;
  match ltr_ctb_step t mem  with
  | Some (t',mem') ->
      Printf.printf "=> ";
      print_reduction_steps t' mem'
  | None ->
       Printf.printf "=> (forme normale)\n"
;;


let eval (t:pterm) =
  match ltr_cbv_norm_timeout t [] 1.0 with
  | Some (nf,mem) -> 
    let prog = pterm_to_string t in 
    let res = pterm_to_string nf in 
    Printf.printf"\tProgram : %s \n" (prog); 
    Printf.printf"\tResult : %s \n" (res); 
  | None -> 
      failwith "Divergence détectée (limite de réduction atteinte).\n"
;;