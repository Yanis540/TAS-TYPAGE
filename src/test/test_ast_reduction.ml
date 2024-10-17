open Ast;; 
(* Fonction de test pour la substitution *)

let examples = [
  ("Addition : 1+2",Add(Int(1),Int(2)),Int(3));
  ("Addition : (1+2) +2",Add(Add(Int(1),Int(2)),Int(2)),Int(5));
  ("Sub : 2-1",Sub(Int(2),Int(1)),Int(1));
  ("Sub et add : 2-1",Add(Sub(Int(5),Int(1)),Int(2)),Int(6));
];;

let test_arithmetic name term expected =
  Printf.printf "\n--- Test Arithmétique: %s ---\n" name;
  match ltr_cbv_norm_timeout term 1.0 with
  | Some result -> 
       Printf.printf "Forme normale (avec timeout): %s\n" (pterm_to_string result);
       if result = expected then
        Printf.printf "✅ Test réussi.\n"
      else
        Printf.printf "❌ Test échoué.\n" 
 
  | None -> 
      Printf.printf "Divergence détectée (timeout de 1 seconde atteinte).\n"
;; 

let _ =
  (* Exécution des tests de substitution *)
  Printf.printf "\n\n\n--- Substition  ---\n\n\n";
  Printf.printf "Forme normale (avec timeout): ";
  List.iter (fun(name,term,expected) -> test_arithmetic name term expected) examples; 

;;