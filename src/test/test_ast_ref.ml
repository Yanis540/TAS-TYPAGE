open Ast;; 
(* Fonction de test pour la substitution *)



(* Fonction de test pour la mémoire *)


let ref_tests = [
  ("Ref (Int 3) => address = 0", Ref(Int 3), Address(0));
  ("Ref (2+3) => address = 1", Ref(Add( Int 2, Int 3)), Address(1));
  ("Ref (λx. x+2) => address = 2", Ref(Abs("x",Add(Var "x",Int 2))), Address(2));
  ("Ref (Addres(2)) => address = 2", Ref(Address(2)), Address(3));
  ("let x = ref 3 in x ", Let("x",Ref(Int 1),Var "x"), Address(4));
]
(* for deref try to modify the empty array in the test_ function  for example : [(3,Address(2))] cause the memory is not saved but rather passed *)
let deref_tests = [
  ("let x = ref 3 in !x  ", Let("x",Ref(Int 1),DeRef(Var "x")), Int 1);
  ("let x = ref (2+3) in !x  ", Let("x",Ref(Add(Int 2,Int 3)),DeRef(Var "x")), Int 5);
  ("let x = ref (λx. x+2) in (!x) 2  ", Let("x",Ref(Abs("x",Add(Var"x",Int 2))),App(DeRef(Var "x"),Int 2)), Int 4);
]
let assign_tests = [
  ("let x = ref 3 in  let x:= 1 in !x  ", Let("x",Ref(Int 3),Let("y",Assign(Var "x",Int 1),DeRef(Var "x"))), Int 1);
  ("let x = ref () in  let x:= (2+3) in !x  ", Let("x",Ref(Unit),Let("y",Assign(Var "x",Add(Int 2,Int 3)),DeRef(Var "x"))), Int 5);
  ("let x = ref () in  let x:= (λx. x+2) in (!x) 2  ", Let("x",Ref(Unit),Let("y",Assign(Var "x",Abs("x",Add(Var"x",Int 2))),App(DeRef(Var "x"),Int 2))), Int 4);
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

  Printf.printf "\n\n\n--- DeRef  ---\n\n\n";
  List.iter (fun(name,term,expected) -> test_ "DeRef" name term expected) deref_tests; 
  Printf.printf "\n\n\n--- Assign  ---\n\n\n";
  List.iter (fun(name,term,expected) -> test_ "Assign" name term expected) assign_tests; 


;;