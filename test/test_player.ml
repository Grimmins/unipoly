open Alcotest
open Unipoly
open Player

(* Test de la création de joueur *)
let test_create_player () =
  let player = create_player "A" in
  check string "Nom correct" "A" (name_player player);
  check int "Position initiale" 0 (pos_player player);
  check int "Argent initial" 1500 (money_player player);
  check bool "Pas en prison" false (is_in_jail player);
  check bool "Pas éliminé" false (is_eliminated player)

(* Test du changement de position *)
let test_change_position () =
  let player = create_player "B" in
  let updated_player = change_pos player 10 in
  check int "Nouvelle position" 10 (pos_player updated_player);
  let updated_player = change_pos player (-5) in
  check int "Position ajustée (mod 40)" 35 (pos_player updated_player)

(* Test de l'argent *)
let test_change_money () =
  let player = create_player "C" in
  let updated_player = change_money player 500 in
  check int "Ajout d'argent" 2000 (money_player updated_player);
  let updated_player = change_money player (-200) in
  check int "Retrait d'argent" 1300 (money_player updated_player)

(* Test de la prison *)
let test_prison () =
  let player = create_player "D" in
  let jailed_player = toogle_to_jail player true in
  check bool "En prison" true (is_in_jail jailed_player);
  let unjailed_player = toogle_to_jail jailed_player false in
  check bool "Pas en prison" false (is_in_jail unjailed_player)

(* Test de la carte Alibi *)
let test_alibi_card () =
  let player = create_player "E" in
  let player_with_alibi = receive_alibi_card player in
  check bool "Possède une carte alibi" true (can_use_alibi player_with_alibi);
  let player_after_use = use_alibi_card player_with_alibi in
  check bool "Alibi utilisé" false (can_use_alibi player_after_use)

(* Test de l'élimination *)
let test_elimination () =
  let player = create_player "F" in
  let eliminated_player = eliminate_player player in
  check bool "Est éliminé" true (is_eliminated eliminated_player)

(* Test de l'ajout de tours en prison *)
let test_add_turn_jail () =
  let player = create_player "G" in
  let player_in_jail = toogle_to_jail player true in
  let player_after_turn_1 = add_turn_jail player_in_jail in
  let player_after_turn_2 = add_turn_jail player_after_turn_1 in
  check int "Premier tour en prison" 1 (get_turn_jail player_after_turn_1);
  check int "Deuxième tour en prison" 2 (get_turn_jail player_after_turn_2)

(* Test de la recherche d'index du joueur *)
let test_find_index_player () =
  let players = [| create_player "A"; create_player "B"; create_player "C" |] in
  let player_b = create_player "B" in
  let index = find_index_player player_b players in
  check (option int) "Index de B" (Some 1) index;
  let player_d = create_player "D" in
  let index_d = find_index_player player_d players in
  check (option int) "Joueur D non trouvé" None index_d

(* Groupe de tests pour le module Player *)
let () =
  run "Player tests" [
    "Creation", [test_case "Création de joueur" `Quick test_create_player];
    "Position", [test_case "Changement de position" `Quick test_change_position];
    "Money", [test_case "Changement d'argent" `Quick test_change_money];
    "Prison", [test_case "Gestion prison" `Quick test_prison];
    "Alibi", [test_case "Carte alibi" `Quick test_alibi_card];
    "Elimination", [test_case "Élimination" `Quick test_elimination];
    "Turn in Jail", [test_case "Ajout de tours en prison" `Quick test_add_turn_jail];
    "Index Finding", [test_case "Recherche de l'index du joueur" `Quick test_find_index_player];
  ]