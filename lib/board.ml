
type board = Square.square array
open Player
open Square

let get_prop (board : Square.square array) (k : int) players =
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
let get_infos (board : Square.square array) (players : Player.player array) (k : int) (is_jail : bool) : string =
   let list_players = List.filter (fun p -> (pos_player p) = k ) (Array.to_list players) in
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
           string_of_int (get_price board.(k)) ^ "k"  (* Afficher le prix sinon *)
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



let display (board : board) (players : player array) current_index_player =
  let infos k = get_infos board players k false in
    let infos_j k = get_infos board players k true in
    let prop k = get_prop board k players in

  (* Affichage du plateau *)
  print_endline "____________________________________________________________________________________";
  print_endline "|Vacances |Géolog| Vie  | Bio  |Chimie|Bibli |Optiqu| Elec |      | Meca |Suspicion|";
  print_endline ("|"^ (infos 20) ^"|"^ (infos 21) ^"|"^ (infos 22) ^"|"^ (infos 23) ^"|"^ (infos 24) ^"|"^ (infos 25) ^"|"^ (infos 26) ^"|"^ (infos 27) ^"|"^ (infos 28) ^"|"^ (infos 29) ^"|"^ (infos 30) ^"|");
  print_endline "|         |______|étudia|______|______|      |______|______|      |______|De Triche|";
  print_endline ("|_________|\027[42m*__"^ (prop 21)^"__\027[0m|______|\027[42m*__"^ (prop 23)^"__\027[0m|\027[42m*__"^ (prop 24)^"__\027[0m|__"^ (prop 25)^"___|\027[46m°__"^ (prop 26)^"__\027[0m|\027[46m°__"^ (prop 27)^"__\027[0m|__"^ (prop 28)^"___|\027[46m°__"^ (prop 29)^"__\027[0m|_________|");
  print_endline ("|Marketi|\027[103m$\027[0m|                                                              |\027[45m+\027[0m| Proba |        C'est au tour de " ^ name_player (players.(current_index_player)));
  print_endline ("|"^ (infos 19) ^"|\027[103m"^ (prop 19)^"\027[0m|                                                              |\027[45m"^ (prop 31)^"\027[0m|"^ (infos 31) ^"|");
  print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
  print_endline ("|Finance|\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Analyse|                   " ^ name_player players.(0) ^ " a " ^ string_of_int (money_player players.(0)) ^ "€");
  print_endline ("|"^ (infos 18) ^"|\027[103m"^ (prop 18)^"\027[0m|                                                              |\027[45m"^ (prop 32)^"\027[0m|"^ (infos 32) ^"|");
  print_endline ("|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|                   " ^ name_player players.(1) ^ " a " ^ string_of_int (money_player players.(1)) ^ "€");
  print_endline "|  Email  |                                                              |  Email  |";
  print_endline ("|"^ (infos 17) ^ "|                                                              |"^ (infos 33) ^"|                   " ^ name_player players.(2) ^ " a " ^ string_of_int (money_player players.(2)) ^ "€");
  print_endline "|_________|                                                              |_________|";
  print_endline ("| Socio |\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Algèbre|                   " ^ name_player players.(3) ^ " a " ^ string_of_int (money_player players.(3)) ^ "€");
  print_endline ("|"^ (infos 16) ^"|\027[103m"^ (prop 16)^"\027[0m|                                                              |\027[45m"^ (prop 34)^"\027[0m|"^ (infos 34) ^"|");
  print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
  print_endline "|  Bibli  |                                                              |  Bibli  |";
  print_endline ("|"^ (infos 15) ^ (prop 15)^"|                                                              |"^ (prop 35) ^ (infos 35) ^"|");
  print_endline "|_________|                                                              |_________|";
  print_endline "| Droit |\027[44m^\027[0m|                                                              |Vie étudi|";
  print_endline ("|"^ (infos 14) ^"|\027[44m"^ (prop 14)^"\027[0m|                                                              |"^ (infos 36) ^"|");
  print_endline "|_______|\027[44m_\027[0m|                                                              |_________|";
  print_endline "|Géograp|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| Algo  |";
  print_endline ("|"^ (infos 13) ^"|\027[44m"^ (prop 13)^"\027[0m|                                                              |\027[100m"^ (prop 37)^"\027[0m|"^ (infos 37) ^"|");
  print_endline "|_______|\027[44m_\027[0m|                                                              |\027[100m_\027[0m|_______|";
  print_endline "|         |                                                              |         |";
  print_endline ("|"^ (infos 12) ^ (prop 12)^"|                                                              |"^ (infos 38) ^"|");
  print_endline "|_________|                                                              |_________|";
  print_endline "|Histoir|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| OCaml |";
  print_endline ("|"^ (infos 11) ^"|\027[44m"^ (prop 11)^"\027[0m|                                                              |\027[100m"^ (prop 39)^"\027[0m|"^ (infos 39) ^"|");
  print_endline "|_______|\027[44m_\027[0m|______________________________________________________________|\027[100m_\027[0m|_______|";
  print_endline ("|  |Triche|\027[102m%__"^ (prop 9)^"__\027[0m|\027[102m%__"^ (prop 8)^"__\027[0m|  Vie |\027[102m%__"^ (prop 6)^"__\027[0m|  "^ (prop 5)^"   |Examen|\027[41m&__"^ (prop 3)^"__\027[0m|      |\027[41m&__"^ (prop 1)^"__\027[0m| Maison  |");
  print_endline ("|  |"^ (infos_j 10) ^"|Anglai|Italie|étudia|Allema| BNF  |      |Philo |Email |Litter|         |");
  print_endline ("|  |______|"^ (infos 9) ^"|"^ (infos 8) ^"|"^ (infos 7) ^"|"^ (infos 6) ^"|"^ (infos 5) ^"|"^ (infos 4) ^"|"^ (infos 3) ^"|"^ (infos 2) ^"|"^ (infos 1) ^"|"^ (infos 0) ^"|");
  print_endline ("|_"^ (infos 10) ^"|______|______|______|______|______|______|______|______|______|_________|");;


let init_board () = [|
  House;
  Square.create_cours Lettres 60 "Littérature";
  Email;
  Square.create_cours Lettres 60 "Philosophie";
  create_tax 200 "Examens";
  create_library "Bibliothèque";
  Square.create_cours Langues 100 "Allemand";
  StLife;
  Square.create_cours Langues 100 "Italien";
  Square.create_cours Langues 120 "Anglais";
  HouseCheating;
  Square.create_cours Hggsp 140 "Histoire";
  create_restaurant "Crous";
  Square.create_cours Hggsp 140 "Géographie";
  Square.create_cours Hggsp 160 "Droit";
  create_library "Bibliothèque";
  Square.create_cours Economie 180 "Sociologie";
  Email;
  Square.create_cours Economie 180 "Finances";
  Square.create_cours Economie 200 "Marketing";
  Holiday;
  Square.create_cours SVT 220 "Géologie";
  StLife;
  Square.create_cours SVT 220 "Biologie";
  Square.create_cours SVT 240 "Chimie";
  create_library "Bibliothèque";
  Square.create_cours Physique 260 "Optique";
  Square.create_cours Physique 260 "Electronique";
  create_restaurant "Barge";
  Square.create_cours Physique 280 "Mécanique";
  Cheating;
  Square.create_cours Math 300 "Probabilités";
  Square.create_cours Math 300 "Analyse";
  Email;
  Square.create_cours Math 320 "Algèbre";
  create_library "Bibliothèque";
  StLife;
  Square.create_cours Info 350 "Algorithmie";
  create_tax 100 "Frais de scolarité";
  Square.create_cours Info 400 "OCaml";
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
  board.(k) <- square;