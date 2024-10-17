open Unipoly 

(* Create a board with 40 squares *)
let init_board = [|
  Square.create_square House;
  Square.create_cours Lettres 60 "Littérature";
  Square.create_square Email;
  Square.create_cours Lettres 60 "Philosophie";
  Square.create_square (Tax {price = 200; name = "Examens"});
  Square.create_square (Library {name = "Bibliothèque"});
  Square.create_cours Langues 100 "Allemand";
  Square.create_square StLife;
  Square.create_cours Langues 100 "Italien";
  Square.create_cours Langues 120 "Anglais";
  Square.create_square HouseCheating;
  Square.create_cours Hggsp 140 "Histoire";
  Square.create_square (Restaurant {name = "Crous"});
  Square.create_cours Hggsp 140 "Géographie";
  Square.create_cours Hggsp 160 "Droit";
  Square.create_square (Library {name = "Bibliothèque"});
  Square.create_cours Economie 180 "Sociologie";
  Square.create_square Email;
  Square.create_cours Economie 180 "Finances";
  Square.create_cours Economie 200 "Marketing";
  Square.create_square Holiday;
  Square.create_cours SVT 220 "Géologie";
  Square.create_square StLife;
  Square.create_cours SVT 220 "Biologie";
  Square.create_cours SVT 240 "Chimie";
  Square.create_square (Library {name = "Bibliothèque"});
  Square.create_cours Physique 260 "Optique";
  Square.create_cours Physique 260 "Electronique";
  Square.create_square (Restaurant {name = "Barge"});
  Square.create_cours Physique 280 "Mécanique";
  Square.create_square Cheating;
  Square.create_cours Math 300 "Probabilités";
  Square.create_cours Math 300 "Analyse";
  Square.create_square Email;
  Square.create_cours Math 320 "Algèbre";
  Square.create_square (Library {name = "Bibliothèque"});
  Square.create_square StLife;
  Square.create_cours Info 350 "Algorithmie";
  Square.create_square (Tax {price = 100; name = "Frais de scolarité"});
  Square.create_cours Info 400 "OCaml";
  |]

(* Main game loop *)
let rec play board () = 
  Board.display board;
  print_endline "";
  print_endline "Appuyez sur Entrée pour terminer votre tour, ou tapez 'end' pour terminer le jeu : ";
  match read_line () with
  | "end" -> exit 0
  | _ -> play board () 

let () = play init_board ()