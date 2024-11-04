open Player
open Square

type board = square array

let get_prop (board : board) (k : int) players =
  let square = board.(k) in
  let char =
    match k with
    | 1 | 3 | 6 | 8 | 9 | 21 | 23 | 24 | 25 | 26 | 27 | 28 | 29 -> "_"
    | _ -> " "
  in
  match square with
  | Buyable b -> (
      match get_owner b players with
      | None -> char
      | Some p -> name_player p
    )
  | _ -> char

let get_players (list_players : player list) (padding_size : int) : string =
    (* Récupérer la liste des noms des joueurs *)
    let name_list = List.map (fun (p : player) -> name_player p) list_players in

    (* Construire la chaîne représentant les joueurs avec les séparateurs (@) *)
    let players_str =
      match name_list with
      | [p1] -> "\027[107m" ^ "@" ^ p1 ^ "\027[0m" ^ " " (* Un seul joueur *)
      | [p1; p2] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2  ^ "\027[0m "  (* Deux joueurs *)
      | [p1; p2; p3] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "\027[0m"  (* Trois joueurs *)
      | [p1; p2; p3; p4] ->
          if padding_size > 7 then
            "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "@" ^ p4 ^ "\027[0m"  (* Affiche le 4ème joueur si padding > 7 *)
          else if padding_size = 7 then
            "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "@" ^ "\027[0m"  (* Affiche le 4ème joueur si padding > 7 *)
          else
            "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ p3 ^ p4 ^ "\027[0m"  (* Sinon, seulement les 3 premiers joueurs *)
      | _ -> ""  (* Aucun joueur ou plus de 4 joueurs, cas non géré ici *)
    in

    (* Calculer le padding à appliquer pour que la longueur totale soit égale à padding_size *)
    let total_length = (String.length players_str) - 10 in
    if total_length >= padding_size then
      (* Si la chaîne de joueurs dépasse ou est égale à la taille désirée, on la renvoie telle quelle *)
      players_str
    else
      (* Sinon, on calcule le nombre d'espaces à ajouter *)
      let padding = padding_size - total_length in
      let left_padding = padding / 2 in
      let right_padding = padding - left_padding in
      String.make left_padding ' ' ^ players_str ^ String.make right_padding ' '


let infos_j (list_players: Player.player list) (is_jail : bool) : string =
  (* Partitionner la liste des joueurs en prisonniers et passants *)
  let cheaters, passersby = List.partition (fun p -> is_in_jail p) list_players in
  if is_jail then (* display des joueurs triche*)
    match List.map name_player cheaters with
    | [p1] -> "\027[107m" ^ "@" ^ p1  ^ "\027[0m" ^ "    "
    | [p1; p2] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2  ^ "\027[0m" ^ "  "
    | [p1; p2; p3] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "\027[0m"
    | [p1; p2; p3; p4] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ p3 ^ p4 ^ "\027[0m"
    | _ -> "      "
  else (* display des passants jail*)
    match List.map name_player passersby with
    | [p1] -> "\027[107m" ^ "@" ^ p1  ^ "\027[0m" ^ "______"
    | [p1; p2] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2  ^ "\027[0m" ^ "____"
    | [p1; p2; p3] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3  ^ "\027[0m" ^ "__"
    | [p1; p2; p3; p4] -> "\027[107m" ^ "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "@" ^ p4 ^ "\027[0m"
    | _ -> "________"



(* return infos for kth square in board *)
let get_infos (board : board) (players : Player.player array) (k : int) (is_jail : bool) : string =
   let players_not_eliminated = List.filter (fun p -> not (is_eliminated p)) (Array.to_list players) in
   let list_players = List.filter (fun p -> (pos_player p) = k ) players_not_eliminated in
   let nb_players = List.length list_players in
    if k = 10 then
        infos_j list_players is_jail
    else (
   (* Fonction pour déterminer le padding précis en fonction de l'index `k` *)
   let padding_size =
     match k with
     | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 -> 6
     | 11 | 13 | 14 | 16 | 18 | 19 | 31 | 32 | 34 | 37 | 39 -> 7
     | 12 | 15 | 35 -> 8
     | _ -> 9  (* Par défaut, le padding est de 9 pour les autres cas *)
   in

   (* Déterminer le texte à afficher : soit les joueurs, soit le prix ou "  " *)
   let content = 
     match nb_players with
     | 0 ->
         if List.mem k [0; 2; 7; 17; 20; 22; 30; 33; 36] then
           "  "  (* Afficher "  " pour les cases spécifiques *)
         else
            let owner = Option.get (get_owner (Option.get (get_square_buyable board.(k))) players) in
            let owner_index = Option.get (find_index_player owner players) in
            let num_libraries_owned = Property.count_librairies_owned owner_index board in
            let owns_all_courses = Property.owns_all_courses_in_ufr (get_ufr (get_cours_from_square board.(k))) owner_index board in
            let price = get_price num_libraries_owned owns_all_courses board.(k) in
           string_of_int price ^ "k"  (* Afficher le prix sinon *)
     | _ -> get_players list_players padding_size  (* Afficher les joueurs s'il y en a *)
   in
(* Calculer le padding pour centrer le texte, tout en vérifiant que le padding n'est pas négatif *)
   let total_length = String.length content in
   if total_length >= padding_size then
     (* Si le contenu dépasse la taille désirée, on le renvoie tel quel *)
     content
   else
     (* Calcul du padding avec gestion des valeurs positives uniquement *)
   let padding = max 0 (padding_size - total_length) in
   let left_padding = padding / 2 in
   let right_padding = padding - left_padding in
   String.make left_padding ' ' ^ content ^ String.make right_padding ' ')

let get_degre_char (k : int) (board : board) : string =
  match board.(k) with
  | Buyable { type_square = Cours cours; _ } -> (
      match (get_degre cours) with
      | None -> "_"
      | Some Licence -> "L"
      | Some Master -> "M"
      | Some Doctorat -> "D"
    )
  | _ -> " "

let display_player_status player =
  if is_eliminated player then
    name_player player ^ " est éliminé"
  else
    name_player player ^ " a " ^ string_of_int (money_player player) ^ "€"


let display (board : board) (players : player array) current_index_player =
  let infos k = get_infos board players k false in
    let infos_j k = get_infos board players k true in
    let prop k = get_prop board k players in
    let deg k = get_degre_char k board in
    let stat i = display_player_status players.(i) in

  (* Affichage du plateau *)
  print_endline "____________________________________________________________________________________";
  print_endline "|Vacances |Géolog| Vie  | Bio  |Chimie|St-Gen|Optiqu| Elec |Barge | Meca |Suspicion|";
  print_endline ("|"^ (infos 20) ^"|"^ (infos 21) ^"|"^ (infos 22) ^"|"^ (infos 23) ^"|"^ (infos 24) ^"|"^ (infos 25) ^"|"^ (infos 26) ^"|"^ (infos 27) ^"|"^ (infos 28) ^"|"^ (infos 29) ^"|"^ (infos 30) ^"|");
  print_endline "|         |______|étudia|______|______|      |______|______|      |______|De Triche|";
  print_endline ("|_________|\027[42m*__"^ (prop 21)^"_"^ (deg 21) ^"\027[0m|______|\027[42m*__"^ (prop 23)^"_"^ (deg 23) ^"\027[0m|\027[42m*__"^ (prop 24)^"_"^ (deg 24) ^"\027[0m|__"^ (prop 25)^"___|\027[46m°__"^ (prop 26)^"_"^ (deg 26) ^"\027[0m|\027[46m°__"^ (prop 27)^"_"^ (deg 27) ^"\027[0m|__"^ (prop 28)^"___|\027[46m°__"^ (prop 29)^"_"^ (deg 29) ^"\027[0m|_________|");
  print_endline ("|Marketi|\027[103m$\027[0m|                                                              |\027[45m+\027[0m| Proba |        C'est au tour de " ^ name_player (players.(current_index_player)));
  print_endline ("|"^ (infos 19) ^"|\027[103m"^ (prop 19)^"\027[0m|                                                              |\027[45m"^ (prop 31)^"\027[0m|"^ (infos 31) ^"|");
  print_endline ("|_______|\027[103m"^ (deg 19) ^"\027[0m|                                                              |\027[45m"^ (deg 31) ^"\027[0m|_______|");
  print_endline ("|Finance|\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Analyse|                   " ^ stat 0);
  print_endline ("|"^ (infos 18) ^"|\027[103m"^ (prop 18)^"\027[0m|                                                              |\027[45m"^ (prop 32)^"\027[0m|"^ (infos 32) ^"|");
  print_endline ("|_______|\027[103m"^ (deg 18) ^"\027[0m|                                                              |\027[45m"^ (deg 32) ^"\027[0m|_______|                   " ^ stat 1);
  print_endline "|  Email  |                                                              |  Email  |";
  print_endline ("|"^ (infos 17) ^ "|                                                              |"^ (infos 33) ^"|                   " ^ stat 2);
  print_endline "|_________|                                                              |_________|";
  print_endline ("| Socio |\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Algèbre|                   " ^ stat 3);
  print_endline ("|"^ (infos 16) ^"|\027[103m"^ (prop 16)^"\027[0m|                                                              |\027[45m"^ (prop 34)^"\027[0m|"^ (infos 34) ^"|");
  print_endline ("|_______|\027[103m"^ (deg 16) ^"\027[0m|                                                              |\027[45m"^ (deg 34) ^"\027[0m|_______|");
  print_endline "| Tolbiac |                                                              |   BPI   |";
  print_endline ("|"^ (infos 15) ^ (prop 15)^"|                                                              |"^ (prop 35) ^ (infos 35) ^"|");
  print_endline "|_________|                                                              |_________|";
  print_endline "| Droit |\027[44m^\027[0m|                                                              |Vie étudi|";
  print_endline ("|"^ (infos 14) ^"|\027[44m"^ (prop 14)^"\027[0m|                                                              |"^ (infos 36) ^"|");
  print_endline ("|_______|\027[44m"^ (deg 14) ^"\027[0m|                                                              |_________|");
  print_endline "|Géograp|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| Algo  |";
  print_endline ("|"^ (infos 13) ^"|\027[44m"^ (prop 13)^"\027[0m|                                                              |\027[100m"^ (prop 37)^"\027[0m|"^ (infos 37) ^"|");
  print_endline ("|_______|\027[44m"^ (deg 13) ^"\027[0m|                                                              |\027[100m"^ (deg 37) ^"\027[0m|_______|");
  print_endline "|  Crous  |                                                              |         |";
  print_endline ("|"^ (infos 12) ^ (prop 12)^"|                                                              |"^ (infos 38) ^"|");
  print_endline "|_________|                                                              |_________|";
  print_endline "|Histoir|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| OCaml |";
  print_endline ("|"^ (infos 11) ^"|\027[44m"^ (prop 11)^"\027[0m|                                                              |\027[100m"^ (prop 39)^"\027[0m|"^ (infos 39) ^"|");
  print_endline ("|_______|\027[44m"^ (deg 11) ^"\027[0m|______________________________________________________________|\027[100m"^ (deg 39) ^"\027[0m|_______|");
  print_endline ("|  |Triche|\027[102m%__"^ (prop 9)^"_"^ (deg 9) ^"\027[0m|\027[102m%__"^ (prop 8)^"_"^ (deg 8) ^"\027[0m|  Vie |\027[102m%__"^ (prop 6)^"_"^ (deg 6) ^"\027[0m|  "^ (prop 5)^"   |Examen|\027[41m&__"^ (prop 3)^"_"^ (deg 3) ^"\027[0m|      |\027[41m&__"^ (prop 1)^"_"^ (deg 1) ^"\027[0m| Maison  |");
  print_endline ("|  |"^ (infos_j 10) ^"|Anglai|Italie|étudia|Allema| BNF  |      |Philo |Email |Litter|         |");
  print_endline ("|  |______|"^ (infos 9) ^"|"^ (infos 8) ^"|"^ (infos 7) ^"|"^ (infos 6) ^"|"^ (infos 5) ^"|"^ (infos 4) ^"|"^ (infos 3) ^"|"^ (infos 2) ^"|"^ (infos 1) ^"|"^ (infos 0) ^"|");
  print_endline ("|_"^ (infos 10) ^"|______|______|______|______|______|______|______|______|______|_________|");;


let init_board () = [|
  House;
  Square.create_cours Lettres 60 "Littérature" 2 10 90 250 50;
  Email;
  Square.create_cours Lettres 60 "Philosophie" 4 20 180 450 50;
  create_tax 200 "Examens";
  create_library "Bibliothèque";
  Square.create_cours Langues 100 "Allemand" 6 30 270 550 50;
  StLife;
  Square.create_cours Langues 100 "Italien" 6 30 270 550 50;
  Square.create_cours Langues 120 "Anglais" 8 40 300 600 50;
  HouseCheating;
  Square.create_cours Hggsp 140 "Histoire" 10 50 450 750 100;
  create_restaurant "Crous";
  Square.create_cours Hggsp 140 "Géographie" 10 50 450 750 100;
  Square.create_cours Hggsp 160 "Droit" 12 60 500 900 100;
  create_library "Bibliothèque";
  Square.create_cours Economie 180 "Sociologie" 14 70 550 950 100;
  Email;
  Square.create_cours Economie 180 "Finances" 14 70 550 950 100;
  Square.create_cours Economie 200 "Marketing" 16 80 600 1000 100;
  Holiday;
  Square.create_cours SVT 220 "Géologie" 18 90 700 1050 150;
  StLife;
  Square.create_cours SVT 220 "Biologie" 18 90 700 1050 150;
  Square.create_cours SVT 240 "Chimie" 20 100 750 1100 150;
  create_library "Bibliothèque";
  Square.create_cours Physique 260 "Optique" 22 110 800 1150 150;
  Square.create_cours Physique 260 "Electronique" 22 110 800 1150 150;
  create_restaurant "Barge";
  Square.create_cours Physique 280 "Mécanique" 24 120 850 1200 150;
  Cheating;
  Square.create_cours Math 300 "Probabilités" 26 130 900 1275 200;
  Square.create_cours Math 300 "Analyse" 26 130 900 1275 200;
  Email;
  Square.create_cours Math 320 "Algèbre" 28 150 1000 1400 200;
  create_library "Bibliothèque";
  StLife;
  Square.create_cours Info 350 "Algorithmie" 35 175 1100 1500 200;
  create_tax 100 "Frais de scolarité";
  Square.create_cours Info 400 "OCaml" 50 200 1400 2000 200;
  |]


let index_square (square : Square.square) board = 
  let n = Array.length board in
    let rec loop i =
      if i = n then None
      else if board.(i) == square then Some i
      else loop (i + 1) in
    loop 0

(* return the square from index k in board *)
let get_square (k : int) board =
  board.(k)

let change_square (k : int) (square : Square.square) board =
  board.(k) <- square

let get_adjusted_course_landing_price_board cours owner_index board  = Property.get_adjusted_course_landing_price cours owner_index board

let count_restaurants_owned_board owner_index board = Property.count_restaurants_owned owner_index board

let count_librairies_owned_board owner_index board = Property.count_librairies_owned owner_index board

let get_index_from_square_buyable_board square board = Square.get_index_from_square_buyable square board

let owns_all_courses_in_ufr_board ufr player_index board = Property.owns_all_courses_in_ufr ufr player_index board

let get_index_from_square_board square board = Square.get_index_from_square square board

let get_properties_owned_by_player_board player_index board = Property.get_properties_owned_by_player player_index board

let get_courses_owned_by_player_board player_index board = Property.get_courses_owned_by_player player_index board

let remove_all_properties_board player_index board = Property.remove_all_properties player_index board