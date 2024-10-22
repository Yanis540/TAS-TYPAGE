open Ast;; 
(* Fonction de test pour la substitution *)

(*! Entiers  *)
let examples_arth = [
  ("Addition : 1+2",Add(Int(1),Int(2)),Int(3));
  ("Addition : (1+2) +2",Add(Add(Int(1),Int(2)),Int(2)),Int(5));
  ("Sub : 2-1",Sub(Int(2),Int(1)),Int(1));
  ("Sub et add : 2-1",Add(Sub(Int(5),Int(1)),Int(2)),Int(6));
];;



(*! List  *)


let examples_list =[
  ("Head [1]", (Head (List(Cons(Int(1),Empty)))), Int 1);
  ("Tail [1]", (Tail (List(Cons(Int(1),Empty)))), Int 1);
  ("Head [1,2,3]", (Head (List(Cons(Int(1),Cons(Int(2),Cons(Int(3),Empty)))))), Int 1);
  ("Tail [1,2,3]", (Tail (List(Cons(Int(1),Cons(Int(2),Cons(Int(3),Empty)))))), Int 3);
  ("Tail []", (Tail (List(Empty))), Int 3);
] 

(*! If  *)


let examples_if =[
  ("IfZero 0 1 2 ", (IfZero(Int(0), Int(1),Int(2))), Int 1);
  ("IfZero 1 1 2 ", (IfZero(Int(1), Int(1),Int(2))), Int 2);
  ("IfZero (1+2) 1 2 ", (IfZero(Add(Int(1),Int(2)), Int(1),Int(2))), Int 2);
  ("IfZero (1-1) 1 2 ", (IfZero(Sub(Int(1),Int(1)), Int(1),Int(2))), Int 1);
] 
(*! Fix  *)
let fact = Fix (Abs ("ϕ", Abs ("n",
  IfZero (Var "n",
          Int 1,
          Add (Var "n", App (Var "ϕ", Sub (Var "n", Int 1))))
  )))
;;

let succ : pterm = Fix (Abs ("ϕ", Abs ("n", Add (Var "n", Int 1))));;
let examples_fix =[
  ("Succssor 1", (App(succ,Int 3)), Int 4);
] 

let test_  (part:string)(name:string) (term:pterm) (expected:pterm) = 
  Printf.printf "\n--- Test %s : %s ---\n"  part name;
  try 
    match ltr_cbv_norm_timeout term 1.0 with
    | Some result -> 
        Printf.printf "Resultat : %s\n" (pterm_to_string result);
        if result = expected then
          Printf.printf "✅ Test réussi.\n"
        else
          Printf.printf "❌ Test échoué.\n" 
  
    | None -> 
        Printf.printf "Divergence détectée (timeout de 1 seconde atteinte).\n"
  with Failure msg ->
    Printf.printf "Erreur : %s\n" msg
;;



let _ =
  (* Exécution des tests de substitution *)
  Printf.printf "\n\n\n--- Substition  ---\n\n\n";
  Printf.printf "Forme normale (avec timeout): ";
  List.iter (fun(name,term,expected) -> test_ "Arithmétique" name term expected) examples_arth; 

  Printf.printf "\n\n\n--- List  ---\n\n\n";
  List.iter (fun (name,term,expected) -> test_ "List" name term expected ) examples_list ;
  Printf.printf "\n\n\n--- Fix  ---\n\n\n";
  List.iter (fun (name,term,expected) -> test_ "Fix" name term expected ) examples_fix;
  Printf.printf "\n\n\n--- If  ---\n\n\n";
  List.iter (fun (name,term,expected) -> test_ "If" name term expected ) examples_if;
;;