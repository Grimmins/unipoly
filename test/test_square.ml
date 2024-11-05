open Alcotest
open Unipoly
open Square

(* Création de `square_buyable` en utilisant les fonctions de Square.ml *)
let example_library = Option.get (get_square_buyable (create_library "Library"))
let example_restaurant = Option.get (get_square_buyable (create_restaurant "Restaurant"))
let example_course = Option.get (get_square_buyable (create_cours Math 100 "Mathématiques" 20 40 80 160 50))

(* Création du tableau `board_example` avec des objets `square` *)
let board_example = [|
  Buyable example_library;
  Buyable example_restaurant;
  Buyable example_course;
  create_tax 50 "Taxe";
  House;
  Cheating
|]

(* Tests pour la fonction `price_buyable` *)
let test_price_buyable () =
  let example_course_cours = Option.get (get_cours example_course) in
  let example_library_type = get_type_square example_library in
  let example_restaurant_type = get_type_square example_restaurant in

  (* Vérification du prix de la bibliothèque *)
  (match example_library_type with
  | Library lib -> check int "Prix de la bibliothèque" 200 (price_buyable (Library lib))
  | _ -> fail "Le type extrait n'est pas une bibliothèque");

  (* Vérification du prix du restaurant *)
  (match example_restaurant_type with
  | Restaurant r -> check int "Prix du restaurant" 150 (price_buyable (Restaurant r))
  | _ -> fail "Le type extrait n'est pas un restaurant");

  (* Vérification du prix du cours *)
  check int "Prix du cours" 100 (price_buyable (Cours example_course_cours))

(* Tests pour `create_library`, `create_restaurant`, et `create_cours` *)
let test_create_course () =
  let course_square = create_cours Math 100 "Mathématiques" 20 40 80 160 50 in
  match course_square with
  | Buyable b ->
    let course = Option.get (get_cours b) in
    check string "Nom du cours" "Mathématiques" (get_name_cours course);
    check int "Prix du cours" 100 (price_buyable (Cours course))
  | _ -> fail "La création du cours a échoué"

(* Test pour `change_owner` *)
let test_change_owner () =
  let new_owner_course = change_owner example_course (Some 1) in
  match new_owner_course with
  | Buyable b -> check (option int) "Nouveau propriétaire" (Some 1) b.proprietaire_index
  | _ -> fail "Échec du changement de propriétaire"

(* Tests pour `get_tax_amount` *)
let test_get_tax_amount () =
  check int "Montant de la taxe" 50 (get_tax_amount (board_example.(3)));
  check int "Montant d'une case non Tax" 0 (get_tax_amount (board_example.(4)))

(* Tests pour `name_square` *)
let test_name_square () =
  check string "Nom de la bibliothèque" "Library" (name_square (board_example.(0)));
  check string "Nom du restaurant" "Restaurant" (name_square (board_example.(1)));
  check string "Nom de la case Tax" "Taxe" (name_square (board_example.(3)))

(* Test pour `update_degre` *)
let test_update_degre () =
  let updated_course = update_degre example_course in
  match get_type_square updated_course with
  | Cours c -> check string "Diplôme mis à jour en Licence" "Licence" (match (get_degre c) with
    | Some Licence -> "Licence"
    | Some Master -> "Master"
    | Some Doctorat -> "Doctorat"
    | None -> "Pas de diplôme")
  | _ -> fail "Le type extrait n'est pas un cours"

(* Tests pour `get_index_from_square` et `get_index_from_square_buyable` *)
let test_get_index_from_square () =
  check (option int) "Index de la bibliothèque" (Some 0) (get_index_from_square (board_example.(0)) board_example);
  check (option int) "Index de la taxe" (Some 3) (get_index_from_square (board_example.(3)) board_example)

let test_get_index_from_square_buyable () =
  check (option int) "Index de la bibliothèque en tant que Buyable" (Some 0) (get_index_from_square_buyable example_library board_example);
  check (option int) "Index du restaurant en tant que Buyable" (Some 1) (get_index_from_square_buyable example_restaurant board_example)

(* Lancer les tests *)
let () =
  run "Tests pour Square" [
    "Création et prix", [
      test_case "Prix des Buyables" `Quick test_price_buyable;
      test_case "Créer un cours" `Quick test_create_course;
      test_case "Changer propriétaire" `Quick test_change_owner;
    ];
    "Propriétés des cases", [
      test_case "Montant de la taxe" `Quick test_get_tax_amount;
      test_case "Nom des cases" `Quick test_name_square;
    ];
    "Diplômes et prix d'atterrissage", [
      test_case "Mise à jour du diplôme" `Quick test_update_degre;
    ];
    "Index dans le plateau", [
      test_case "Index d'une case" `Quick test_get_index_from_square;
      test_case "Index d'un Buyable" `Quick test_get_index_from_square_buyable;
    ];
  ]
