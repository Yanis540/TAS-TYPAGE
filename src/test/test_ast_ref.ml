open Ast;; 
(* Fonction de test pour la substitution *)



(* Fonction de test pour la mémoire *)
let test_memory_operations () =
  Printf.printf "\n--- Test des opérations sur la mémoire ---\n";
  let mem = [] in
  let addr1 = new_address () in
  let addr2 = new_address () in
  let addr3 = new_address () in
  Printf.printf "Adresse 1 : %d\n" addr1;
  Printf.printf "Adresse 2 : %d\n" addr2;
  Printf.printf "Adresse 3 : %d\n" addr3;
  let mem = mem_update mem addr1 (Int 42) in
  let mem = mem_update mem addr2 (Abs ("x", Var "x")) in
  let mem = mem_update mem addr3 (Add (Int 1, Int 2)) in
  Printf.printf "État de la mémoire après les ajouts : %s\n" (mem_to_string mem);
  (try 
     let _ = mem_lookup mem 999 in
     Printf.printf "Erreur : l'adresse 999 aurait dû lever une exception\n"
   with Failure msg ->
     Printf.printf "Accès à une adresse inconnue levé correctement : %s\n" msg);
;;


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
    Printf.printf "❌ Erreur : %s\n" msg
;;



let _ =
  (* Exécution des tests de substitution *)
  (* Printf.printf "\n\n\n--- Substition  ---\n\n\n";
  List.iter (fun(name,term,expected) -> test_ "Arithmétique" name term expected) examples_arth;  *)
  (* Appeler la fonction de test *)
  test_memory_operations ();


;;