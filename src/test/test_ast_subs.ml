open Ast;; 
open Eval;; 

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


let substitution_tests = [
  ("Var substitution", Var "x","x",Int 42,Int 42);
  ("Abs substitution - variable liée",Abs ("x", Add (Var "x", Int 1)),"x",Int 42,Abs ("x", Add (Var "x", Int 1)));
  ("Abs substitution - variable non liée",Abs ("y", Add (Var "x", Int 1)), "x", Int 42, Abs ("y", Add (Int 42, Int 1)));
  ("App substitution",App (Var "x", Int 5),"x",Abs ("y", Var "y"),App (Abs ("y", Var "y"), Int 5));
  ("Add substitution", Add (Var "x", Int 3),"x", Int 7, Add (Int 7, Int 3));
  ("List substitution",List (Cons (Var "x", Cons (Int 2, Empty))),"x",Int 1,List (Cons (Int 1, Cons (Int 2, Empty))));
  ("Head substitution",Head (Var "x"),"x",List (Cons (Int 10, Empty)),Head (List (Cons (Int 10, Empty))));
  ("Tail substitution",Tail (Var "x"),"x",List (Cons (Int 10, Cons (Int 20, Empty))),Tail (List (Cons (Int 10, Cons (Int 20, Empty)))));
  ("IfZero substitution",IfZero (Var "x", Int 1, Int 0),"x",Int 0,IfZero (Int 0, Int 1, Int 0));
  ("Let substitution", Let ("x", Int 5, Add (Var "x", Var "y")), "y", Int 10, Let ("x", Int 5, Add (Var "x", Int 10)));
]
;;


let _ =
  (* Exécution des tests de substitution *)
  Printf.printf "\n\n\n--- Substition  ---\n\n\n";

  List.iter (fun (name, term, x, nterm, expected) ->
    test_substitution name term x nterm expected
  ) substitution_tests;
  

;;