type board = Square.square array
open Square
open Player

let get_players (square : square) : string =
    let list_players = square.players in
        let name_list = (List.map (fun p -> p.name) list_players) in
            match name_list with
                | [p1] -> "  @" ^ p1 ^ "   "
                | [p1; p2] -> " @" ^ p1 ^ " @" ^ p2 ^ " "
                | [p1; p2; p3] -> "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3
                | [p1; p2; p3; _] -> "@" ^ p1 ^ "@" ^ p2 ^ "@" ^ p3 ^ "@"
                | _ -> ""



(* return infos for kth square in board *)
let get_infos (board : square array) (k : int) : string =
  let square = Array.get board k in
  let nb_players = List.length square.players in

  (* Fonction pour déterminer le padding précis en fonction de l'index `k` *)
  let padding_size =
    match k with
    | 1 | 3 | 4 | 5 | 6 | 8 | 9 | 21 | 23 | 24 | 25 | 26 | 27 | 28 | 29 -> 6
    | 11 | 13 | 14 | 16 | 18 | 19 | 31 | 32 | 34 | 35 | 37 | 38 | 39 -> 7
    | _ -> 9  (* Par défaut, le padding est de 9 pour les autres cas *)
  in

  (* Si aucun joueur, on affiche le prix avec le bon padding, sinon les joueurs *)
  match nb_players with
  | 0 ->
      let price_str = string_of_int (get_price square.square_type) ^ "k" in
      let padding = padding_size - String.length price_str in  (* Calcul du padding total *)
      let left_padding = padding / 2 in
      let right_padding = padding - left_padding in
      String.make left_padding ' ' ^ price_str ^ String.make right_padding ' '
  | _ -> get_players square


let display (board : board) =
    board.(0).players |> List.iter (fun player -> print_endline (Player.name_player player));

    print_endline "____________________________________________________________________________________";
    print_endline "|Vacances |Géolog|      | Bio  |Chimie|Bibli |Optiqu| Elec |      | Meca |Suspicion|";
    print_endline ("|"^ (get_infos board 20) ^"| 25k  |      |      |      |      |      |      |      |      |         |");
    print_endline "|         |______|      |______|______|      |______|______|      |______|De Triche|";
    print_endline "|_________|\027[42m*_____\027[0m|______|\027[42m*_____\027[0m|\027[42m*_____\027[0m|______|\027[46m°_____\027[0m|\027[46m°_____\027[0m|______|\027[46m°_____\027[0m|_________|";
    print_endline "|Marketi|\027[103m$\027[0m|                                                              |\027[45m+\027[0m| Proba |";
    print_endline ("|"^ (get_infos board 19) ^"|\027[103m \027[0m|                                                              |\027[45m \027[0m|  25k  |");
    print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
    print_endline "|Finance|\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Analyse|";
    print_endline ("|"^ (get_infos board 18) ^"|\027[103m \027[0m|                                                              |\027[45m \027[0m|       |");
    print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
    print_endline "|         |                                                              |         |";
    print_endline ("|"^ (get_infos board 17) ^"|                                                              |         |");
    print_endline "|_________|                                                              |_________|";
    print_endline "| Socio |\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Algèbre|";
    print_endline ("|"^ (get_infos board 16) ^"|\027[103m \027[0m|                                                              |\027[45m \027[0m| @E @L |");
    print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
    print_endline "|  Bibli  |                                                              |  Bibli  |";
    print_endline ("|"^ (get_infos board 15) ^"|                                                              |         |");
    print_endline "|_________|                                                              |_________|";
    print_endline "| Droit |\027[44m^\027[0m|                                                              |         |";
    print_endline ("|"^ (get_infos board 14) ^"|\027[44m \027[0m|                                                              |         |");
    print_endline "|_______|\027[44m_\027[0m|                                                              |_________|";
    print_endline "|Géograp|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| Algo  |";
    print_endline ("|"^ (get_infos board 13) ^"|\027[44m \027[0m|                                                              |\027[100m \027[0m|       |");
    print_endline "|_______|\027[44m_\027[0m|                                                              |\027[100m_\027[0m|_______|";
    print_endline "|         |                                                              |         |";
    print_endline ("|"^ (get_infos board 12) ^"|                                                              |         |");
    print_endline "|_________|                                                              |_________|";
    print_endline "|Histoir|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| OCaml |";
    print_endline ("|"^ (get_infos board 11) ^"|\027[44m \027[0m|                                                              |\027[100m \027[0m|       |");
    print_endline "|_______|\027[44m_\027[0m|______________________________________________________________|\027[100m_\027[0m|_______|";
    print_endline "|  |Triche|\027[102m%_____\027[0m|\027[102m%_____\027[0m|      |\027[102m%_____\027[0m| BNF  |Examen|\027[41m&_____\027[0m|      |\027[41m&_____\027[0m| Maison  |";
    print_endline "|  |      |Anglai|Italie|      |Allema|      |      |Philo |      |Litter|         |";
    print_endline ("|  |______|"^ (get_infos board 9) ^"|"^ (get_infos board 8) ^"|"^ (get_infos board 7) ^"|"^ (get_infos board 6) ^"|"^ (get_infos board 5) ^"|"^ (get_infos board 4) ^"|"^ (get_infos board 3) ^"|"^ (get_infos board 2) ^"|"^ (get_infos board 1) ^"|"^ (get_infos board 0) ^"|");
    print_endline "|_________|______|______|______|______|______|______|______|______|______|_________|";;




