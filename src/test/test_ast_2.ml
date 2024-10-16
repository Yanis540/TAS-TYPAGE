open Ast;; 
(* Fonction de test pour la substitution *)
let test_substitution (name: string) (term: pterm) (x: string) (nterm: pterm) (expected: pterm) =
  Printf.printf "\n--- Test Substitution: %s ---\n" name;
  Printf.printf "Original: %s\n" (pterm_to_string term);
  Printf.printf "Substituer %s par %s\n" x (pterm_to_string nterm);
  let result = substitution x nterm term in
  Printf.printf "Résultat: %s\n" (pterm_to_string result);
  Printf.printf "Attendu: %s\n" (pterm_to_string expected);
  if result = expected then
    Printf.printf "✅ Test réussi.\n"
  else
    Printf.printf "❌ Test échoué.\n"
;;
(* Exemples de tests de substitution *)
let substitution_tests = [
  (* Substitution dans une variable *)
  ("Var substitution", Var "x","x",Int 42,Int 42);
  (* Substitution dans une abstraction où la variable est liée *)
  ("Abs substitution - variable liée",
   Abs ("x", Add (Var "x", Int 1)),
   "x",
   Int 42,
   Abs ("x", Add (Var "x", Int 1)));

  (* Substitution dans une abstraction où la variable n'est pas liée *)
  ("Abs substitution - variable non liée",
   Abs ("y", Add (Var "x", Int 1)),
   "x",
   Int 42,
   Abs ("y", Add (Int 42, Int 1)));

  (* Substitution dans une application *)
  ("App substitution",
   App (Var "x", Int 5),
   "x",
   Abs ("y", Var "y"),
   App (Abs ("y", Var "y"), Int 5));

  (* Substitution dans une addition *)
  ("Add substitution",
   Add (Var "x", Int 3),
   "x",
   Int 7,
   Add (Int 7, Int 3));

  (* Substitution dans une liste *)
  ("List substitution",
   List (Cons (Var "x", Cons (Int 2, Empty))),
   "x",
   Int 1,
   List (Cons (Int 1, Cons (Int 2, Empty))));

  (* Substitution dans Head et Tail *)
  ("Head substitution",
   Head (Var "x"),
   "x",
   List (Cons (Int 10, Empty)),
   Head (List (Cons (Int 10, Empty))));

  ("Tail substitution",
   Tail (Var "x"),
   "x",
   List (Cons (Int 10, Cons (Int 20, Empty))),
   Tail (List (Cons (Int 10, Cons (Int 20, Empty)))));

  (* Substitution dans IfZero *)
  ("IfZero substitution",
   IfZero (Var "x", Int 1, Int 0),
   "x",
   Int 0,
   IfZero (Int 0, Int 1, Int 0));

  (* Substitution dans Fix *)
  ("Fix substitution",
   Fix ("phi", Abs ("x", App (Var "phi", Var "x")), Var "phi"),
   "x",
   Int 1,
   Fix ("phi", Abs ("x", App (Var "phi", Var "x")), Var "phi"));
  
  (* Substitution dans Let *)
  ("Let substitution",
   Let ("x", Int 5, Add (Var "x", Var "y")),
   "y",
   Int 10,
   Let ("x", Int 5, Add (Var "x", Int 10)));
]
;;

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
  (* Alpha-conversion dans une abstraction simple *)
  ("Abs alpha_conv",
   Abs ("x", Add (Var "x", Int 1)),
   Abs ("X1", Add (Var "X1", Int 1)));

  (* Alpha-conversion dans une abstraction imbriquée *)
  ("Nested Abs alpha_conv",
   Abs ("x", Abs ("y", App (Var "x", Var "y"))),
   Abs ("X2", Abs ("X3", App (Var "X2", Var "X3"))));

  (* Alpha-conversion avec des listes *)
  ("List alpha_conv",
   List (Cons (Var "x", Cons (Var "y", Empty))),
   List (Cons (Var "X4", Cons (Var "X5", Empty))));

  (* Alpha-conversion dans IfZero *)
  ("IfZero alpha_conv",
   IfZero (Var "x", Add (Var "y", Int 2), Sub (Var "z", Int 1)),
   IfZero (Var "X6", Add (Var "X7", Int 2), Sub (Var "X8", Int 1)));

  (* Alpha-conversion dans Fix *)
  ("Fix alpha_conv",
   Fix ("phi", Abs ("x", App (Var "phi", Var "x")), Var "phi"),
   Fix ("phi", Abs ("X10", App (Var "X11", Var "X10")), Var "X9"));

  (* Alpha-conversion dans Let *)
  ("Let alpha_conv",
   Let ("x", Int 5, Add (Var "x", Var "y")),
   Let ("X12", Int 5, Add (Var "X11", Var "X12")));
]
;;

let _ =
  (* Exécution des tests d'alpha-conversion *)
  Printf.printf "\n--- Alpha conversion  ---\n";

  List.iter (fun (name, term, expected) ->
    test_alpha_conv name term expected
  ) alpha_conv_tests;
  (* Exécution des tests de substitution *)
  Printf.printf "\n\n\n--- Substition  ---\n\n\n";

  List.iter (fun (name, term, x, nterm, expected) ->
    test_substitution name term x nterm expected
  ) substitution_tests;
  

;;