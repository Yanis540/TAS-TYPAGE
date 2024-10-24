open Ast;; 
(* Fonction de test pour la substitution *)



(* Fonction de test pour la mémoire *)


let ref_tests = [
  ("Ref (Int 3) => address = 0", Ref(Int 3), Address(0));
  ("Ref (2+3) => address = 1", Ref(Add( Int 2, Int 3)), Address(1));
  ("Ref (λx. x+2) => address = 2", Ref(Abs("x",Add(Var "x",Int 2))), Address(2));
  ("Ref (Addres(2)) => address = 2", Ref(Address(2)), Address(3));
]
let test_  (part:string)(name:string) (term:pterm) (expected:pterm) = 
  Printf.printf "\n--- Test %s : %s ---\n"  part name;
  try 
    match ltr_cbv_norm_timeout term [] 1.0 with
    | Some (result,mem) -> 
        Printf.printf "Resultat : %s\n" (pterm_to_string result);
        if result = expected then
          Printf.printf "✅ Test réussi.\n"
        else
          Printf.printf "❌ Test échoué.\n" 
  
    | None -> 
        Printf.printf "Divergence détectée (timeout de 1 seconde atteinte).\n"
  with Failure msg ->
    Printf.printf "❌ Erreur : %s\n" msg
;;



let _ =
  (* Exécution des tests de substitution *)
  Printf.printf "\n\n\n--- Ref  ---\n\n\n";
  List.iter (fun(name,term,expected) -> test_ "Ref" name term expected) ref_tests; 


;;