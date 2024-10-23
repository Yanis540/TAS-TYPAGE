open Ast;; 
open Type;; 
open Ast;;




(* Test des opérateurs arithmétiques *)
let arithmetic_tests = [
  ("3", Int 3, N);
  ("Addition", Add(Int 3, Int 2), N);
  ("Multiplication", Mult(Int 4, Int 5), N);
  ("Subtraction", Sub(Int 10, Int 3), N);
  ("Nested arithmetic", Add(Mult(Int 2, Int 3), Sub(Int 7, Int 1)), N);
];;




(* Fonction de test pour le typage *)
let test_typing (part:string) (name:string) (term:pterm) (expected:ptype) = 
  Printf.printf "\n--- Test %s : %s ---\n" part name;
  try
    let inferred_type = infer_type_ term in
    Printf.printf "Type inféré : %s\n" (ptype_to_string inferred_type);
    if inferred_type = expected then
      Printf.printf "✅ Test réussi.\n"
    else
      Printf.printf "❌ Test échoué. Type attendu : %s\n" (ptype_to_string expected)
  with Failure msg ->
    Printf.printf "Erreur : %s\n" msg
;;

(* Exécution des tests *)
let _ =

  Printf.printf "\n\n--- Tests : Arithmétique ---\n\n";
  List.iter (fun (name, term, expected) -> test_typing "Arithmétique" name term expected) arithmetic_tests;

;;
