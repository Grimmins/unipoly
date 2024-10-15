open Unipoly 

(* Create a board with 40 squares *)
let board : Board.board = [|
  Square.create_square Holiday;
  Square.create_cours Math 25 "Géologie";
  Square.create_square House;
  Square.create_cours SVT 25 "Biologie";
  Square.create_cours SVT 25 "Chimie";
  Square.create_square (Library {name = "Bibliothèque"});
  Square.create_cours Physique 25 "Optique";
  Square.create_cours Physique 25 "Electronique";
  Square.create_cours Physique 25 "Mécanique";
  Square.create_square House
  |]

(* Main game loop *)
let rec play board () = 
  Board.show_board board ();
  print_endline "";
  print_endline "Appuyez sur Entrée pour terminer votre tour, ou tapez 'end' pour terminer le jeu : ";
  match read_line () with
  | "end" -> exit 0
  | _ -> play board () 

let () = play board ()