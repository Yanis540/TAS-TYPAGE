open Ast ;;


let abs_func : Ast.pterm = Abs("X",Abs("Y", App(Var("X"),Var("Y"))));;
let varToReplace = "X";;
let ntermToReplace = Abs("N",Abs("Z",App (Var("N"),Var("Z"))));;
let abs_func_substitued_by_substitued_v2 = Ast.substitution varToReplace ntermToReplace abs_func;;
let t1 = Abs ("x", App (Var "x", Var "y"));;
let t2 = Var "x";;
let sub_t2_in_t1_result_v2 = substitution "y" t2 t1;;
let example_II = App (Abs ("x", Var "x"), Abs ("y", Var "y"));;
let combinator_k = Abs ("x", Abs ("y", Var "x"))
let combinator_s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let combinator_i = Abs ("x", Var "x") ;;
let combinator_k = Abs ("x", Abs ("y", Var "x")) ;;
let combinator_s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))) ;;
let combinator_delta = Abs ("x", App (Var "x", Var "x")) ;;
let combinator_omega = App (combinator_delta, combinator_delta) ;;  (* Ω = (λx.x x) (λx.x x) *)
let combinator_skK = App (App (combinator_s, combinator_k), combinator_k) ;;
let combinator_sI = App (App (combinator_s, combinator_i), combinator_i) ;;
let example_skk = App (App (combinator_s, combinator_k), combinator_k);; 

(* Nombres de Church *)
let church_zero = Abs ("f", Abs ("x", Var "x")) ;;
let church_one = Abs ("f", Abs ("x", App (Var "f", Var "x"))) ;;
let church_two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))) ;;
let church_three = Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x"))))) ;;

(* Opérations Arithmétiques *)
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x"))))) ;;
let plus_ = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x")))))) ;;
let mult = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f"))))) ;;
(* Fonction qui convertit un entier en notation Church en un entier OCaml *)
let church_to_int (church_num: pterm) : int =
  match church_num with
  | Abs (_, Abs (_, body)) -> (
      let rec eval n = match n with
        | Var "x" -> 0
        | App (Var "f", t) -> 1 + eval t
        | _ -> failwith "Invalid Church numeral"
      in eval body
    )
  | _ -> failwith "Not a valid Church numeral"
;;

let examples = [
  ("I", combinator_i);
  ("K", combinator_k);
  ("S", combinator_s);
  (* ("delta", combinator_delta); *)
  (* ("Omega", combinator_omega); *)
  ("S K K", combinator_skK);
  ("S I I", combinator_sI);
  ("Church 0", church_zero);
  ("Church 1", church_one);
  ("Church 2", church_two);
  ("Church 3", church_three);
  ("Successeur", succ);
  ("Addition", plus_);
  ("Multiplication", mult);
];; 

let test_normalization (name, term) =
  Printf.printf "\n--- Test: %s ---\n" name;
  (* Tenter de normaliser avec un timeout de 10 étapes *)
  match ltr_cbv_norm_timeout term 1.0 with
  | Some nf -> 
      Printf.printf "Forme normale (avec timeout): %s\n" (pterm_to_string nf)
  | None -> 
      Printf.printf "Divergence détectée (limite de réduction atteinte).\n"
;;


let main () = 

  Printf.printf "\n--- Substition  ---\n";
  print_pterm sub_t2_in_t1_result_v2;
  Printf.printf "\n--- Réduction de l'exemple II ---\n";
  print_reduction_steps example_II;
  Printf.printf "\n--- Réduction de l'exemple SKK ---\n";
  print_reduction_steps example_skk;
  Printf.printf "\n--- Normalisation avec timeout ---\n";
  (match ltr_cbv_norm_timeout example_skk 1.0 with
  | Some nf -> 
    Printf.printf "Forme normale (avec timeout): ";
    print_pterm nf
    | None -> 
      Printf.printf "Divergence détectée (limite de réduction atteinte).\n"
  );
  Printf.printf "\n--- Normalisation avec timeout ---\n";
  List.iter test_normalization examples;
 
;;

let _ = main ();;