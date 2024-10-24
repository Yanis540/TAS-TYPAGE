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

let basic_infering = [
  ("3", Int 3, N); 
  ("(λx.x)", Abs("x",Add( Int 3, Var "x")), Arrow(N,N)); 
  ("(λx.x)", Abs("x",Var "x"), Arrow(VarType "T7",VarType "T7")); 
]

let list_tests = [
  ("[1]", List(Cons(Int(1),Empty)), ListType(N));
  ("[1, (λx.x)]", List(Cons(Int(1),Cons(Abs("x",Var "x"),Empty))), ListType(N)); 
  ("[1, (1+2)]", List(Cons(Int(1),Cons(Add(Int(1),Int(2)),Empty))), ListType(N)); 
  ("[x]", List(Cons(Var "x",Empty)), ListType(VarType "x"));  (* should fail car il n'a pas me type de x *)
  ("head [1]", Head(List(Cons(Int(1),Empty))), N); 
  ("head [(1+2)]", Head(List(Cons(Add(Int 1,Int 2),Empty))), N); 
  ("tail [1]", Tail(List(Cons(Int(1),Empty))), N); 
  ("tail [(1+2),3]", Tail(List(Cons(Add(Int 1,Int 2),Cons(Int 3,Empty)))), N); 
  ("head [(λx.x)]", Head(List(Cons(Abs("x",Add( Int 3, Var "x")),Empty))), Arrow(N,N)); 
  ("head [(λx.x)]", Head(List(Cons(Abs("x", Var "x"),Empty))), Arrow(VarType "T44",VarType "T44"));  (* l'essentiel que ça soit un truc alpha -> alpha *)
  ("tail [(λx.x)]", Tail(List(Cons(Abs("x",Add( Int 3, Var "x")),Empty))), Arrow(N,N)); 

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

  Printf.printf "\n\n--- Tests : Entiers Basic Infering ---\n\n";
  List.iter (fun (name, term, expected) -> test_typing "Basic Infering" name term expected) basic_infering;
  Printf.printf "\n\n--- Tests : Entiers Arithmétique ---\n\n";
  List.iter (fun (name, term, expected) -> test_typing "Arithmétique" name term expected) arithmetic_tests;
  Printf.printf "\n\n--- Tests : List ---\n\n";
  List.iter (fun (name, term, expected) -> test_typing "List" name term expected) list_tests;

;;
