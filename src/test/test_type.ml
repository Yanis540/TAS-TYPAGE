open Ast;; 
open Type;; 

(* Exemple de terme et génération des équations *)

(* Exemple : terme Var "x" *)
let env_ex = [("x", VarType "int")];;
let test_term = App(Abs("x", Var "x"), Var "x");;
let eqs = generate_equa test_term (VarType "T2") env_ex;;
(* Test *)
let t1 = Arrow (VarType "T1", VarType "T2");;  (* T1 -> T2 *)
let t2 = Arrow (VarType "T1", Arrow (VarType "T1", VarType "T3"));;  (* T1 -> (T1 -> T3) *)
let t3 = Nat;;
let occur_check_examples = [
  (true,"T1",t1);
  (false,"T3",t1);
  (true,"T1",t2);
  (false,"T1",t3)
]
let occur_check_test (e) = 
  let (expected_res,v ,t) = e in 
  let res = occur_check v t in 
  Printf.printf "Occur check %s in (%s): %b expected %b \n" (v) (ptype_to_string t) (res) (expected_res)
;;
let _ =
  Printf.printf "Equations générées: %s\n" (equas_to_string eqs); 
  Printf.printf "\n--- Réduction de l'exemple SKK ---\n";
  List.iter occur_check_test occur_check_examples
;; 