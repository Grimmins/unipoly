open Alcotest
open Unipoly
open Square
open Property

(* Créons des exemples de propriétés en utilisant les fonctions de Square.ml pour retourner directement des valeurs de type square *)

let restaurant1 = match create_restaurant "Restaurant1" with
  | Buyable b -> { b with proprietaire_index = Some 0 }
  | _ -> failwith "Erreur de création de restaurant"

let restaurant2 = match create_restaurant "Restaurant2" with
  | Buyable b -> { b with proprietaire_index = Some 1 }
  | _ -> failwith "Erreur de création de restaurant"

let library1 = match create_library "Library1" with
  | Buyable b -> { b with proprietaire_index = Some 0 }
  | _ -> failwith "Erreur de création de bibliothèque"

let library2 = match create_library "Library2" with
  | Buyable b -> { b with proprietaire_index = Some 1 }
  | _ -> failwith "Erreur de création de bibliothèque"

let cours1 = match create_cours Math 100 "Maths 101" with
  | Buyable b -> { b with proprietaire_index = Some 0 }
  | _ -> failwith "Erreur de création de cours"

let cours2 = match create_cours Info 150 "Info 101" with
  | Buyable b -> { b with proprietaire_index = Some 1 }
  | _ -> failwith "Erreur de création de cours"

let house = House  (* Une propriété non achetable *)

(* Créer le tableau example_board avec des éléments de type square *)
let example_board = [|
  Buyable restaurant1;
  Buyable library1;
  Buyable cours1;
  Buyable restaurant2;
  Buyable library2;
  Buyable cours2;
  house
|]

(* Test pour is_restaurant *)
let test_is_restaurant () =
  check bool "Restaurant1 est un restaurant" true (is_restaurant restaurant1.type_square);
  check bool "Library1 n'est pas un restaurant" false (is_restaurant library1.type_square)

(* Test pour is_library *)
let test_is_library () =
  check bool "Library1 est une bibliothèque" true (is_library library1.type_square);
  check bool "Cours1 n'est pas une bibliothèque" false (is_library cours1.type_square)

(* Test pour is_cours *)
let test_is_cours () =
  check bool "Cours1 est un cours" true (is_cours cours1.type_square);
  check bool "Restaurant1 n'est pas un cours" false (is_cours restaurant1.type_square)

(* Test pour count_properties_owned *)
let test_count_properties_owned () =
  let count_restaurants = count_properties_owned is_restaurant 0 example_board in
  let count_libraries = count_properties_owned is_library 0 example_board in
  let count_courses = count_properties_owned is_cours 0 example_board in
  check int "Nombre de restaurants détenus par le joueur 0" 1 count_restaurants;
  check int "Nombre de bibliothèques détenues par le joueur 0" 1 count_libraries;
  check int "Nombre de cours détenus par le joueur 0" 1 count_courses

(* Test pour count_restaurants_owned *)
let test_count_restaurants_owned () =
  let count = count_restaurants_owned 0 example_board in
  check int "Nombre de restaurants détenus par le joueur 0" 1 count

(* Test pour count_librairies_owned *)
let test_count_librairies_owned () =
  let count = count_librairies_owned 0 example_board in
  check int "Nombre de bibliothèques détenues par le joueur 0" 1 count

(* Test pour count_cours_owned *)
let test_count_cours_owned () =
  let count = count_cours_owned 0 example_board in
  check int "Nombre de cours détenus par le joueur 0" 1 count

(* Groupe de tests pour Property *)
let () =
  run "Property tests" [
    "Type Checks", [
      test_case "Check if property is a restaurant" `Quick test_is_restaurant;
      test_case "Check if property is a library" `Quick test_is_library;
      test_case "Check if property is a cours" `Quick test_is_cours;
    ];
    "Count Properties", [
      test_case "Count properties owned by type" `Quick test_count_properties_owned;
      test_case "Count restaurants owned" `Quick test_count_restaurants_owned;
      test_case "Count libraries owned" `Quick test_count_librairies_owned;
      test_case "Count courses owned" `Quick test_count_cours_owned;
    ];
  ]
