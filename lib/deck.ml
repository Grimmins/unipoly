open Card

type deck = card list

let init_email_deck () : deck = [
  create_card "Ascenseur en panne" "Ascenseur de Gare de Lyon en panne, reculez de trois cases" (Move (-3));
  create_card "Crous fermé" "Crous fermé, avancez jusqu'à la barge" (GoTo 28);
  (* create_card "Bourse étudiante" "Bourse acceptée, recevez 150€" (GainMoney 150);
  create_card "Frais CVEC" "Vous devez payer la CVEC qui s'élève à 50 €" (LoseMoney (-50));
  create_card "Suspicion de triche" "Vous avez sorti votre téléphone en examen" GoToJail;
  create_card "Alibi" "Utilisez cette carte lorsque vous serez suspecté de triche" GetOutOfJail; *)
]

let init_stlife_deck () : deck = [
  create_card "Ascenseur en panne" "Ascenseur de Gare de Lyon en panne, reculez de trois cases" (Move (-3));
  create_card "Crous fermé" "Crous fermé, avancez jusqu'à la barge" (GoTo 28);
  (* create_card "Bourse étudiante" "Bourse acceptée, recevez 150€" (GainMoney 150);
  create_card "Frais CVEC" "Vous devez payer la CVEC qui s'élève à 50 €" (LoseMoney (-50));
  create_card "Suspicion de triche" "Vous avez sorti votre téléphone en examen" GoToJail;
  create_card "Alibi" "Utilisez cette carte lorsque vous serez suspecté de triche" GetOutOfJail; *)
]


let draw_card (deck: deck) : card * deck =
  let card_index = Random.int (List.length deck) in
  let card = List.nth deck card_index in
  let new_deck = List.filter (fun c -> c != card) deck in
  (card, new_deck)
