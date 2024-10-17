open Ast;; 
(* Fonction de test pour la substitution *)



(* Fonction de test pour l'alpha-conversion *)
let test_alpha_conv (name: string) (term: pterm) (expected: pterm) =
  Printf.printf "\n--- Test Alpha Conversion: %s ---\n" name;
  Printf.printf "Original: %s\n" (pterm_to_string term);
  let result = alpha_conv term [] in
  Printf.printf "Alpha-Converted: %s\n" (pterm_to_string result);
  Printf.printf "Attendu: %s\n" (pterm_to_string expected);
  if result = expected then
    Printf.printf "✅ Test réussi.\n"
  else
    Printf.printf "❌ Test échoué.\n"
;;
(* Exemples de tests d'alpha-conversion *)
let alpha_conv_tests = [
  ("Abs alpha_conv",
    Abs ("x", Add (Var "x", Int 1)),
    Abs ("X1", Add (Var "X1", Int 1))
  );
  ("Nested Abs alpha_conv",
    Abs ("x", Abs ("y", App (Var "x", Var "y"))),
    Abs ("X2", Abs ("X3", App (Var "X2", Var "X3")))
  );
  ("List alpha_conv",
    List (Cons (Var "x", Cons (Var "y", Empty))),
    List (Cons (Var "X4", Cons (Var "X5", Empty)))
  );
  ("IfZero alpha_conv",
    IfZero (Var "x", Add (Var "y", Int 2), Sub (Var "z", Int 1)),
    IfZero (Var "X6", Add (Var "X7", Int 2), Sub (Var "X8", Int 1))
  );
  ("Let alpha_conv",
    Let ("x", Int 5, Add (Var "x", Var "y")),
    Let ("X12", Int 5, Add (Var "X11", Var "X12"))
  );
]
;;

let _ =
  (* Exécution des tests d'alpha-conversion *)
  Printf.printf "\n--- Alpha conversion  ---\n";

  List.iter (fun (name, term, expected) ->
    test_alpha_conv name term expected
  ) alpha_conv_tests;
  

;;