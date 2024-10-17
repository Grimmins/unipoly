
type board = Square.square array
open Player
open Square

let get_players (list_players : player list) (padding_size : int) : string =
    (* Récupérer la liste des noms des joueurs *)
    let name_list = List.map (fun (p : player) -> name_player p) list_players in

    (* Construire la chaîne représentant les joueurs avec les séparateurs (@) *)
    let players_str =
      match name_list with
      | [p1] -> "  @" ^ p1 ^ "   "  (* Un seul joueur *)
      | [p1; p2] -> " @" ^ p1 ^ " @" ^ p2 ^ " "  (* Deux joueurs *)
      | [p1; p2; p3] -> "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3  (* Trois joueurs *)
      | [p1; p2; p3; p4] ->
          if padding_size > 7 then
            "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "@" ^ p4  (* Affiche le 4ème joueur si padding > 7 *)
          else
            "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "@"  (* Sinon, seulement les 3 premiers joueurs *)
      | _ -> ""  (* Aucun joueur ou plus de 4 joueurs, cas non géré ici *)
    in

    (* Calculer le padding à appliquer pour que la longueur totale soit égale à padding_size *)
    let total_length = String.length players_str in
    if total_length >= padding_size then
      (* Si la chaîne de joueurs dépasse ou est égale à la taille désirée, on la renvoie telle quelle *)
      players_str
    else
      (* Sinon, on calcule le nombre d'espaces à ajouter *)
      let padding = padding_size - total_length in
      let left_padding = padding / 2 in
      let right_padding = padding - left_padding in
      String.make left_padding ' ' ^ players_str ^ String.make right_padding ' '




(* return infos for kth square in board *)
let get_infos (board : Square.square array) (players : Player.player array) (k : int) : string =
   let list_players = List.filter (fun p -> (pos_player p) = k ) (Array.to_list players) in
   let nb_players = List.length list_players in

   (* Fonction pour déterminer le padding précis en fonction de l'index `k` *)
   let padding_size =
     match k with
     | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 -> 6
     | 11 | 13 | 14 | 16 | 18 | 19 | 31 | 32 | 34 | 37 | 39 -> 7
     | _ -> 9  (* Par défaut, le padding est de 9 pour les autres cas *)
   in

   (* Déterminer le texte à afficher : soit les joueurs, soit le prix ou "  " *)
   let content = 
     match nb_players with
     | 0 ->
         if List.mem k [2; 7; 17; 22; 33; 36] then
           "  "  (* Afficher "  " pour les cases spécifiques *)
         else
           string_of_int (get_price board.(k).square_type) ^ "k"  (* Afficher le prix sinon *)
     | _ -> get_players list_players padding_size  (* Afficher les joueurs s'il y en a *)
   in

   (* Calculer le padding pour centrer le texte *)
   let padding = padding_size - String.length content in
   let left_padding = padding / 2 in
   let right_padding = padding - left_padding in
   String.make left_padding ' ' ^ content ^ String.make right_padding ' '



let display (board : board) (players : player array) =
  let infos k = get_infos board players k in

  (* Affichage du plateau *)
  print_endline "____________________________________________________________________________________";
  print_endline "|Vacances |Géolog|      | Bio  |Chimie|Bibli |Optiqu| Elec |      | Meca |Suspicion|";
  print_endline ("|"^ (infos 20) ^"|"^ (infos 21) ^"|"^ (infos 22) ^"|"^ (infos 23) ^"|"^ (infos 24) ^"|"^ (infos 25) ^"|"^ (infos 26) ^"|"^ (infos 27) ^"|"^ (infos 28) ^"|"^ (infos 29) ^"|"^ (infos 30) ^"|");
  print_endline "|         |______|      |______|______|      |______|______|      |______|De Triche|";
  print_endline "|_________|\027[42m*_____\027[0m|______|\027[42m*_____\027[0m|\027[42m*_____\027[0m|______|\027[46m°_____\027[0m|\027[46m°_____\027[0m|______|\027[46m°_____\027[0m|_________|";
  print_endline "|Marketi|\027[103m$\027[0m|                                                              |\027[45m+\027[0m| Proba |";
  print_endline ("|"^ (infos 19) ^"|\027[103m \027[0m|                                                              |\027[45m \027[0m|"^ (infos 31) ^"|");
  print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
  print_endline "|Finance|\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Analyse|";
  print_endline ("|"^ (infos 18) ^"|\027[103m \027[0m|                                                              |\027[45m \027[0m|"^ (infos 32) ^"|");
  print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
  print_endline "|         |                                                              |         |";
  print_endline ("|"^ (infos 17) ^"|                                                              |"^ (infos 33) ^"|");
  print_endline "|_________|                                                              |_________|";
  print_endline "| Socio |\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Algèbre|";
  print_endline ("|"^ (infos 16) ^"|\027[103m \027[0m|                                                              |\027[45m \027[0m|"^ (infos 34) ^"|");
  print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
  print_endline "|  Bibli  |                                                              |  Bibli  |";
  print_endline ("|"^ (infos 15) ^"|                                                              |"^ (infos 35) ^"|");
  print_endline "|_________|                                                              |_________|";
  print_endline "| Droit |\027[44m^\027[0m|                                                              |         |";
  print_endline ("|"^ (infos 14) ^"|\027[44m \027[0m|                                                              |"^ (infos 36) ^"|");
  print_endline "|_______|\027[44m_\027[0m|                                                              |_________|";
  print_endline "|Géograp|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| Algo  |";
  print_endline ("|"^ (infos 13) ^"|\027[44m \027[0m|                                                              |\027[100m \027[0m|"^ (infos 37) ^"|");
  print_endline "|_______|\027[44m_\027[0m|                                                              |\027[100m_\027[0m|_______|";
  print_endline "|         |                                                              |         |";
  print_endline ("|"^ (infos 12) ^"|                                                              |"^ (infos 38) ^"|");
  print_endline "|_________|                                                              |_________|";
  print_endline "|Histoir|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| OCaml |";
  print_endline ("|"^ (infos 11) ^"|\027[44m \027[0m|                                                              |\027[100m \027[0m|"^ (infos 39) ^"|");
  print_endline "|_______|\027[44m_\027[0m|______________________________________________________________|\027[100m_\027[0m|_______|";
  print_endline "|  |Triche|\027[102m%_____\027[0m|\027[102m%_____\027[0m|      |\027[102m%_____\027[0m| BNF  |Examen|\027[41m&_____\027[0m|      |\027[41m&_____\027[0m| Maison  |";
  print_endline "|  |      |Anglai|Italie|      |Allema|      |      |Philo |      |Litter|         |";
  print_endline ("|  |______|"^ (infos 9) ^"|"^ (infos 8) ^"|"^ (infos 7) ^"|"^ (infos 6) ^"|"^ (infos 5) ^"|"^ (infos 4) ^"|"^ (infos 3) ^"|"^ (infos 2) ^"|"^ (infos 1) ^"|"^ (infos 0) ^"|");
  print_endline "|_________|______|______|______|______|______|______|______|______|______|_________|";;


let init_board () = [|
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

