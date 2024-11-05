open Alcotest
open Unipoly
open Deck

let test_init_email_deck () =
  let email_deck = init_email_deck () in
  check int "Nombre de cartes dans le deck email" 6 (List.length email_deck);
  let expected_names = ["Ascenseur en panne"; "Crous fermé"; "Bourse étudiante"; "Frais CVEC"; "Suspicion de triche"; "Alibi"] in
  List.iter2 (fun card expected_name ->
      check string "Nom de la carte" expected_name (Card.get_name card)
    ) email_deck expected_names

let test_init_stlife_deck () =
  let stlife_deck = init_stlife_deck () in
  check int "Nombre de cartes dans le deck vie étudiante" 6 (List.length stlife_deck);
  let expected_names = ["Ascenseur en panne"; "Crous fermé"; "Bourse étudiante"; "Frais CVEC"; "Suspicion de triche"; "Alibi"] in
  List.iter2 (fun card expected_name ->
      check string "Nom de la carte" expected_name (Card.get_name card)
    ) stlife_deck expected_names

(* Test de la pioche de carte *)
let test_draw_card () =
  let deck = init_email_deck () in
  let initial_length = List.length deck in
  let (drawn_card, new_deck) = draw_card deck in

  (* Vérifie que la carte piochée est dans le deck initial *)
  check bool "La carte piochée était dans le deck initial" true (List.mem drawn_card deck);

  (* Vérifie que la carte piochée n'est plus dans le nouveau deck *)
  check bool "La carte piochée n'est plus dans le nouveau deck" false (List.mem drawn_card new_deck);

  (* Vérifie que le deck a une carte en moins *)
  check int "Le deck a une carte en moins" (initial_length - 1) (List.length new_deck)

(* Groupe de tests pour le module Deck *)
let () =
  run "Deck tests" [
    "Init Email Deck", [test_case "Initialisation du deck email" `Quick test_init_email_deck];
    "Init Student Life Deck", [test_case "Initialisation du deck vie étudiante" `Quick test_init_stlife_deck];
    "Draw Card", [test_case "Pioche de carte" `Quick test_draw_card];
  ]
