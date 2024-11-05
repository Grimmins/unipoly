open Alcotest
open Unipoly
open Square
open Property

(* Fonction auxiliaire pour définir un propriétaire sur un Buyable square *)
let set_owner square owner_index =
  match square with
  | Buyable b -> Buyable { b with proprietaire_index = Some owner_index }
  | _ -> square

(* Créons des exemples de propriétés en utilisant les fonctions de Square.ml pour retourner directement des valeurs de type square *)
let restaurant1 = set_owner (create_restaurant "Restaurant1") 0
let restaurant2 = set_owner (create_restaurant "Restaurant2") 1
let library1 = set_owner (create_library "Library1") 0
let library2 = set_owner (create_library "Library2") 1
let cours1 = set_owner (create_cours Math 100 "Maths 101" 10 30 60 100 20) 0
let cours2 = set_owner (create_cours Info 150 "Info 101" 15 40 75 120 25) 1
let house = House  (* Une propriété non achetable *)

(* Créer le tableau example_board avec des éléments de type square *)
let example_board = [|
  restaurant1;
  library1;
  cours1;
  restaurant2;
  library2;
  cours2;
  house
|]

(* Test pour is_restaurant *)
let test_is_restaurant () =
  check bool "Restaurant1 est un restaurant" true (is_restaurant (get_type_square (Option.get (get_square_buyable restaurant1))));
  check bool "Library1 n'est pas un restaurant" false (is_restaurant (get_type_square (Option.get (get_square_buyable library1))))

(* Test pour is_library *)
let test_is_library () =
  check bool "Library1 est une bibliothèque" true (is_library (get_type_square (Option.get (get_square_buyable library1))));
  check bool "Cours1 n'est pas une bibliothèque" false (is_library (get_type_square (Option.get (get_square_buyable cours1))))

(* Test pour is_cours *)
let test_is_cours () =
  check bool "Cours1 est un cours" true (is_cours (get_type_square (Option.get (get_square_buyable cours1))));
  check bool "Restaurant1 n'est pas un cours" false (is_cours (get_type_square (Option.get (get_square_buyable restaurant1))))

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
      test_case "Check if property is un cours" `Quick test_is_cours;
    ];
    "Count Properties", [
      test_case "Count restaurants owned" `Quick test_count_restaurants_owned;
      test_case "Count libraries owned" `Quick test_count_librairies_owned;
      test_case "Count courses owned" `Quick test_count_cours_owned;
    ];
  ]
