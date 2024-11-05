open Alcotest
open Unipoly
open Card
open Player

(* Test de la création de carte *)
let test_create_card () =
  let card = create_card "Test Card" "This is a test card" (GainMoney 100) in
  check string "Nom de la carte" "Test Card" (get_name card);
  check string "Description de la carte" "This is a test card" (get_description card);
  match get_effect card with
  | GainMoney 100 -> ()
  | _ -> fail "Effet incorrect de la carte"

(* Test de l'application de l'effet Move *)
let test_apply_card_effect_move () =
  let player = create_player "A" in
  let card = create_card "Move" "Move by 3" (Move 3) in
  let updated_player, _ = apply_card_effect player card in
  check int "Position après mouvement" 3 (pos_player updated_player)

(* Test de l'application de l'effet GainMoney *)
let test_apply_card_effect_gain_money () =
  let player = create_player "B" in
  let card = create_card "Gain Money" "Gain 100€" (GainMoney 100) in
  let updated_player, _ = apply_card_effect player card in
  check int "Argent après gain" 1600 (money_player updated_player)

(* Test de l'application de l'effet LoseMoney *)
let test_apply_card_effect_lose_money () =
  let player = create_player "D" in
  let card = create_card "Lose Money" "Lose 50€" (LoseMoney (-50)) in
  let updated_player, _ = apply_card_effect player card in
  check int "Argent après perte" 1450 (money_player updated_player)

(* Test de l'application de l'effet GoTo *)
let test_apply_card_effect_goto () =
  let player = create_player "D" in
  let card = create_card "Go To" "Go to position 10" (GoTo 10) in
  let updated_player, _ = apply_card_effect player card in
  check int "Position après GoTo" 10 (pos_player updated_player)

(* Test de l'application de l'effet GoToJail *)
let test_apply_card_effect_goto_jail () =
  let player = create_player "D" in
  let card = create_card "Go To Jail" "Go directly to jail" GoToJail in
  let updated_player, _ = apply_card_effect player card in
  check int "Position après GoToJail" 10 (pos_player updated_player);
  check bool "Le joueur est en prison" true (is_in_jail updated_player)

(* Test de l'application de l'effet GetOutOfJail *)
let test_apply_card_effect_get_out_of_jail () =
  let player = create_player "D" in
  let card = create_card "Get Out of Jail" "Use to get out of jail" GetOutOfJail in
  let updated_player, _ = apply_card_effect player card in
  check bool "Possède la carte alibi" true (can_use_alibi updated_player)


(* Groupe de tests pour le module Card *)
let () =
  run "Card tests" [
    "Create Card", [test_case "Création de carte" `Quick test_create_card];
    "Apply Card Effect", [
      test_case "Effet Move" `Quick test_apply_card_effect_move;
      test_case "Effet GainMoney" `Quick test_apply_card_effect_gain_money;
      test_case "Effet LoseMoney" `Quick test_apply_card_effect_lose_money;
      test_case "Effet GoTo" `Quick test_apply_card_effect_goto;
      test_case "Effet GoToJail" `Quick test_apply_card_effect_goto_jail;
      test_case "Effet GetOutOfJail" `Quick test_apply_card_effect_get_out_of_jail;
    ];
  ]
