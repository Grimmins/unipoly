open Alcotest
open Unipoly
open Square
open Property

(* Fonctions auxiliaires pour les tests *)
let set_owner square_buyable owner_index =
  match square_buyable with
  | { proprietaire_index = _; type_square } ->
    { proprietaire_index = Some owner_index; type_square }

(* Création des types `square_buyable` en utilisant les fonctions de Square.ml *)
let example_library = set_owner (Option.get (get_square_buyable (create_library "Bibliothèque"))) 0
let example_restaurant = set_owner (Option.get (get_square_buyable (create_restaurant "Restaurant"))) 0
let example_course_1 = set_owner (Option.get (get_square_buyable (create_cours Math 100 "Mathématiques 1" 20 40 80 160 50))) 0
let example_course_2 = set_owner (Option.get (get_square_buyable (create_cours Math 100 "Mathématiques 2" 20 40 80 160 50))) 0
let example_course_3 = set_owner (Option.get (get_square_buyable (create_cours Physique 100 "Physique 2" 20 40 80 160 50))) 0
let example_course_4 = set_owner (Option.get (get_square_buyable (create_cours Physique 100 "Physique 1" 8 40 80 160 50))) 0
let example_course_5 = Option.get (get_square_buyable (create_cours Physique 100 "Physique 3" 20 40 80 160 50))
let example_course_info_1 = set_owner (Option.get (get_square_buyable (create_cours Info 100 "Info 1" 10 40 80 160 50))) 0
let example_course_info_2 = set_owner (Option.get (get_square_buyable (create_cours Info 100 "Info 2" 12 40 80 160 50))) 0


(* Utiliser update_degre pour simuler des cours avec différents diplômes *)
let set_course_to_degree course_buyable degree =
  match degree with
  | None -> course_buyable
  | Some Licence -> update_degre course_buyable
  | Some Master -> update_degre (update_degre course_buyable)
  | Some Doctorat -> update_degre (update_degre (update_degre course_buyable))

let course_with_licence = set_course_to_degree example_course_1 (Some Licence)
let course_with_master = set_course_to_degree example_course_2 (Some Master)
let course_with_doctorat = set_course_to_degree example_course_3 (Some Doctorat)

(* Créer le plateau de test avec des `square` en utilisant `Buyable` pour encapsuler chaque `square_buyable` *)
let board_example = [|
  Buyable example_library;
  Buyable example_restaurant;
  Buyable course_with_licence;
  Buyable course_with_master;
  Buyable course_with_doctorat;
  Buyable example_course_4;
  Buyable example_course_5;
  Buyable example_course_info_1;
  Buyable example_course_info_2;
  House  (* Case non achetable *)
|]

(* Tests *)

(* Test de la fonction is_restaurant *)
let test_is_restaurant () =
  check bool "Est un restaurant" true (is_restaurant (get_type_square (Option.get (get_square_buyable (board_example.(1))))));
  check bool "N'est pas un restaurant" false (is_restaurant (get_type_square (Option.get (get_square_buyable (board_example.(0))))))

(* Test de la fonction is_library *)
let test_is_library () =
  check bool "Est une bibliothèque" true (is_library (get_type_square (Option.get (get_square_buyable (board_example.(0))))));
  check bool "N'est pas une bibliothèque" false (is_library (get_type_square (Option.get (get_square_buyable (board_example.(1))))))

(* Test de la fonction is_cours *)
let test_is_cours () =
  check bool "Est un cours" true (is_cours (get_type_square (Option.get (get_square_buyable (board_example.(2))))));
  check bool "N'est pas un cours" false (is_cours (get_type_square (Option.get (get_square_buyable (board_example.(1))))))

(* Test pour count_properties_owned *)
let test_count_properties_owned () =
  let count_libs = count_librairies_owned 0 board_example in
  let count_rests = count_restaurants_owned 0 board_example in
  let count_courses = count_cours_owned 0 board_example in
  check int "Nombre de bibliothèques possédées par joueur 0" 1 count_libs;
  check int "Nombre de restaurants possédés par joueur 0" 1 count_rests;
  check int "Nombre de cours possédés par joueur 0" 6 count_courses

(* Test pour owns_all_courses_in_ufr *)
let test_owns_all_courses_in_ufr () =
  check bool "Possède tous les cours en Math" true (owns_all_courses_in_ufr Math 0 board_example);
  check bool "Ne possède pas tous les cours en Physique" false (owns_all_courses_in_ufr Physique 0 board_example)

(* Test pour get_courses_owned_by_player *)
let test_get_courses_owned_by_player () =
  let courses = get_courses_owned_by_player 0 board_example in
  check int "Nombre de cours possédés par le joueur 0" 6 (List.length courses)

(* Test pour get_properties_owned_by_player *)
let test_get_properties_owned_by_player () =
  let properties = get_properties_owned_by_player 0 board_example in
  check int "Nombre de propriétés possédées par le joueur 0" 8 (List.length properties)

(* Test pour get_adjusted_course_landing_price avec tous les cours dans un UFR sans diplôme *)
let test_get_adjusted_course_landing_price_all_courses_owned () =
  (* On s'assure que tous les cours dans Math sont possédés par le joueur 0 sans diplôme *)
  let course_phys_1 = get_cours_from_square (board_example.(5)) in
  let course_info_1 = get_cours_from_square (board_example.(7)) in
  let course_info_2 = get_cours_from_square (board_example.(8)) in
  let price_info_1 = get_adjusted_course_landing_price course_info_1 0 board_example in
  let price_info_2 = get_adjusted_course_landing_price course_info_2 0 board_example in
  let price_phys_1 = get_adjusted_course_landing_price course_phys_1 0 board_example in
  (* Vérification que le prix est doublé car le joueur possède tous les cours dans Math sans diplôme *)
  check int "Prix ajusté pour cours Physique 1 sans diplôme, pas tous les cours de l'UFR possédés" 8 price_phys_1;
  check int "Prix ajusté pour cours Info 1 sans diplôme, tous les cours de l'UFR possédés" 20 price_info_1;
  check int "Prix ajusté pour cours Info 2 sans diplôme, tous les cours de l'UFR possédés" 24 price_info_2


(* Test pour get_landing_price selon le diplôme *)
let test_get_landing_price_with_degrees () =
  (* Initialisation de cours avec différents diplômes *)
  let cours_without_diploma = get_cours_from_square (Buyable example_course_4) in
  let cours_licence = get_cours_from_square (Buyable course_with_licence) in
  let cours_master = get_cours_from_square (Buyable course_with_master) in
  let cours_doctorat = get_cours_from_square (Buyable course_with_doctorat) in
  check int "Prix landing pour cours sans diplôme" 8 (get_landing_price cours_without_diploma);
  check int "Prix landing pour cours avec Licence" 40 (get_landing_price cours_licence);
  check int "Prix landing pour cours avec Master" 80 (get_landing_price cours_master);
  check int "Prix landing pour cours avec Doctorat" 160 (get_landing_price cours_doctorat)

(* Test pour get_next_degree_price *)
let test_get_next_degree_price () =
  (* Cours initial sans diplôme *)
  let course_no_degree = get_cours_from_square (board_example.(2)) in
  let price_no_degree = get_next_degree_price course_no_degree in
  check int "Prix de mise à niveau pour un cours sans diplôme" 50 price_no_degree;

  (* Cours avec licence *)
  let course_with_licence = get_cours_from_square (Buyable (set_course_to_degree example_course_1 (Some Licence))) in
  let price_with_licence = get_next_degree_price course_with_licence in
  check int "Prix de mise à niveau pour un cours avec Licence" 50 price_with_licence;

  (* Cours avec master *)
  let course_with_master = get_cours_from_square (Buyable (set_course_to_degree example_course_1 (Some Master))) in
  let price_with_master = get_next_degree_price course_with_master in
  check int "Prix de mise à niveau pour un cours avec Master" 50 price_with_master;

  (* Cours avec doctorat *)
  let course_with_doctorat = get_cours_from_square (Buyable (set_course_to_degree example_course_1 (Some Doctorat))) in
  let price_with_doctorat = get_next_degree_price course_with_doctorat in
  check int "Prix de mise à niveau pour un cours avec Doctorat" 0 price_with_doctorat

(* Tests pour get_price *)

let test_get_price () =
  (* Cas Tax *)
  let tax_square = create_tax 150 "Taxe foncière" in
  let tax_price = get_price board_example 0 tax_square in
  check int "Prix pour une case de taxe" 150 tax_price;

  (* Cas Restaurant (prix toujours 0) *)
  let restaurant_price = get_price board_example 0 (Buyable example_restaurant) in
  check int "Prix pour un restaurant" 0 restaurant_price;

  (* Cas Library sans propriétaire *)
  let library_no_owner = Option.get (get_square_buyable (create_library "Library")) in
  let library_no_owner_price = get_price board_example 0 (Buyable library_no_owner) in
  check int "Prix pour une bibliothèque sans propriétaire" 200 library_no_owner_price;

  (* Cas Library avec propriétaire, une bibliothèque possédée *)
  let library_one_owned_price = get_price board_example 0 (Buyable example_library) in
  check int "Prix pour une bibliothèque avec un propriétaire possédant une bibliothèque" 25 library_one_owned_price;

  (* Cas Library avec propriétaire, deux bibliothèques possédées *)
  let library_two_owned = set_owner (Option.get (get_square_buyable (create_library "Library2"))) 0 in
  let board_with_two_libraries = [|
    Buyable example_library;
    Buyable example_restaurant;
    Buyable example_course_1;
    Buyable example_course_2;
    Buyable example_course_3;
    Buyable example_course_4;
    Buyable example_course_5;
    Buyable library_two_owned
  |] in
  let library_two_owned_price = get_price board_with_two_libraries 0 (Buyable example_library) in
  check int "Prix pour une bibliothèque avec un propriétaire possédant deux bibliothèques" 50 library_two_owned_price;

  (* Cas Library avec propriétaire, trois bibliothèques possédées *)
  let library_three_owned = set_owner (Option.get (get_square_buyable (create_library "Library3"))) 0 in
  let board_with_three_libraries = [|
    Buyable example_library;
    Buyable example_restaurant;
    Buyable example_course_1;
    Buyable example_course_2;
    Buyable example_course_3;
    Buyable example_course_4;
    Buyable example_course_5;
    Buyable library_two_owned;
    Buyable library_three_owned
  |] in
  let library_three_owned_price = get_price board_with_three_libraries 0 (Buyable example_library) in
  check int "Prix pour une bibliothèque avec un propriétaire possédant trois bibliothèques" 100 library_three_owned_price;

  (* Cas Library avec propriétaire, quatre bibliothèques possédées *)
  let library_four_owned = set_owner (Option.get (get_square_buyable (create_library "Library4"))) 0 in
  let board_with_four_libraries = [|
    Buyable example_library;
    Buyable example_restaurant;
    Buyable example_course_1;
    Buyable example_course_2;
    Buyable example_course_3;
    Buyable example_course_4;
    Buyable example_course_5;
    Buyable library_two_owned;
    Buyable library_three_owned;
    Buyable library_four_owned
  |] in
  let library_four_owned_price = get_price board_with_four_libraries 0 (Buyable example_library) in
  check int "Prix pour une bibliothèque avec un propriétaire possédant quatre bibliothèques" 200 library_four_owned_price;

  (* Cas IMPOSSIBLE library avec propriétaire, 5 bibliothèques possédées par exemple *)
    let library_five_owned = set_owner (Option.get (get_square_buyable (create_library "Library5"))) 0 in
    let board_with_five_libraries = [|
      Buyable example_library;
      Buyable example_restaurant;
      Buyable example_course_1;
      Buyable example_course_2;
      Buyable example_course_3;
      Buyable example_course_4;
      Buyable example_course_5;
      Buyable library_two_owned;
      Buyable library_three_owned;
      Buyable library_four_owned;
      Buyable library_five_owned
    |] in
    let library_five_owned_price = get_price board_with_five_libraries 0 (Buyable example_library) in
    check int "Prix pour une bibliothèque avec un propriétaire possédant 5 bibliothèques ce qui est impossible" 0 library_five_owned_price;

  (* Cas Cours sans propriétaire *)
  let course_no_owner = example_course_5 in
  let course_no_owner_price = get_price board_example 0 (Buyable course_no_owner) in
  check int "Prix pour un cours sans propriétaire" 100 course_no_owner_price;

  (* Cas Cours avec propriétaire unique sans diplôme (prix doublé si tous les cours de l'UFR sont possédés) *)
  let course_price_all_owned = get_price board_example 0 (Buyable example_course_1) in
  check int "Prix pour un cours avec propriétaire unique sans diplôme (tous les cours de l'UFR possédés)" 40 course_price_all_owned;

  (* Cas Cours avec propriétaire ayant un diplôme (licence) *)
  let course_with_licence_price = get_price board_example 0 (Buyable course_with_licence) in
  check int "Prix pour un cours avec une licence" 40 course_with_licence_price;

  (* Cas Cours avec propriétaire ayant un master *)
  let course_with_master_price = get_price board_example 0 (Buyable course_with_master) in
  check int "Prix pour un cours avec un master" 80 course_with_master_price;

  (* Cas Cours avec propriétaire ayant un doctorat *)
  let course_with_doctorat_price = get_price board_example 0 (Buyable course_with_doctorat) in
  check int "Prix pour un cours avec un doctorat" 160 course_with_doctorat_price

(* Cas pour les types de square qui ne sont ni Tax ni Buyable *)
let test_non_buyable_or_tax_price () =
  let house_price = get_price board_example 0 House in
  let email_price = get_price board_example 0 Email in
  let stlife_price = get_price board_example 0 StLife in
  check int "Prix pour une case House (non Tax ni Buyable)" 0 house_price;
  check int "Prix pour une case Email (non Tax ni Buyable)" 0 email_price;
  check int "Prix pour une case StLife (non Tax ni Buyable)" 0 stlife_price


(* Test pour remove_all_properties *)
let test_remove_all_properties () =
  remove_all_properties 0 board_example;
  let owned_properties = get_properties_owned_by_player 0 board_example in
  check int "Toutes les propriétés ont été supprimées du joueur 0" 0 (List.length owned_properties)

(* Groupe de tests *)
let () =
  run "Property Tests" [
    "Type Checks", [
      test_case "Vérifie si est un restaurant" `Quick test_is_restaurant;
      test_case "Vérifie si est une bibliothèque" `Quick test_is_library;
      test_case "Vérifie si est un cours" `Quick test_is_cours;
    ];
    "Ownership Count", [
      test_case "Compte les propriétés par type" `Quick test_count_properties_owned;
      test_case "Vérifie si le joueur possède tous les cours en UFR" `Quick test_owns_all_courses_in_ufr;
      test_case "Récupère les cours possédés par le joueur" `Quick test_get_courses_owned_by_player;
      test_case "Récupère toutes les propriétés du joueur" `Quick test_get_properties_owned_by_player;
    ];
    "Pricing", [
      test_case "Prix ajusté pour un cours possédé" `Quick test_get_adjusted_course_landing_price_all_courses_owned;
      test_case "Prix de landing en fonction du diplôme" `Quick test_get_landing_price_with_degrees;
      test_case "Prix d'upgrade du diplôme" `Quick test_get_next_degree_price;
      test_case "Récupération du prix pour Tax / Buyable" `Quick test_get_price;
      test_case "Récupération du prix pour =/= de Tax / Buyable" `Quick test_non_buyable_or_tax_price;
    ];
    "Remove Properties", [
      test_case "Suppression de toutes les propriétés d'un joueur" `Quick test_remove_all_properties;
    ];
  ]
