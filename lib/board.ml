type board = Square.square array
open Square
open Player
let display (board : board) =
    board.(0).players |> List.iter (fun player -> print_endline (Player.name_player player));

    print_endline "____________________________________________________________________________________";
    print_endline "|Vacances |Géolog|      | Bio  |Chimie|Bibli |Optiqu| Elec |      | Meca |Suspicion|";
    print_endline "|         | 25k  |      |      |      |      |      |      |      |      |         |";
    print_endline "|         |______|      |______|______|      |______|______|      |______|De Triche|";
    print_endline "|_________|\027[42m*_____\027[0m|______|\027[42m*_____\027[0m|\027[42m*_____\027[0m|______|\027[46m°_____\027[0m|\027[46m°_____\027[0m|______|\027[46m°_____\027[0m|_________|";
    print_endline "|Marketi|\027[103m$\027[0m|                                                              |\027[45m+\027[0m| Proba |";
    print_endline "|       |\027[103m \027[0m|                                                              |\027[45m \027[0m|  25k  |";
    print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
    print_endline "|Finance|\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Analyse|";
    print_endline "|       |\027[103m \027[0m|                                                              |\027[45m \027[0m|       |";
    print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
    print_endline "|         |                                                              |         |";
    print_endline "|         |                                                              |         |";
    print_endline "|_________|                                                              |_________|";
    print_endline "| Socio |\027[103m$\027[0m|                                                              |\027[45m+\027[0m|Algèbre|";
    print_endline "|       |\027[103m \027[0m|                                                              |\027[45m \027[0m| @E @L |";
    print_endline "|_______|\027[103m_\027[0m|                                                              |\027[45m_\027[0m|_______|";
    print_endline "|  Bibli  |                                                              |  Bibli  |";
    print_endline "|         |                                                              |         |";
    print_endline "|_________|                                                              |_________|";
    print_endline "| Droit |\027[44m^\027[0m|                                                              |         |";
    print_endline "|       |\027[44m \027[0m|                                                              |         |";
    print_endline "|_______|\027[44m_\027[0m|                                                              |_________|";
    print_endline "|Géograp|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| Algo  |";
    print_endline "|       |\027[44m \027[0m|                                                              |\027[100m \027[0m|       |";
    print_endline "|_______|\027[44m_\027[0m|                                                              |\027[100m_\027[0m|_______|";
    print_endline "|         |                                                              |         |";
    print_endline "|         |                                                              |         |";
    print_endline "|_________|                                                              |_________|";
    print_endline "|Histoir|\027[44m^\027[0m|                                                              |\027[100m#\027[0m| OCaml |";
    print_endline "|       |\027[44m \027[0m|                                                              |\027[100m \027[0m|       |";
    print_endline "|_______|\027[44m_\027[0m|______________________________________________________________|\027[100m_\027[0m|_______|";
    print_endline "|  |Triche|\027[102m%_____\027[0m|\027[102m%_____\027[0m|      |\027[102m%_____\027[0m| BNF  |      |\027[41m&_____\027[0m|      |\027[41m&_____\027[0m| Maison  |";
    print_endline "|  |      |Anglai|Italie|      |Allema|      |      |Philo |      |Litter|         |";
    print_endline "|  |______|      |      |      |      |      |      |      |      |      |         |";
    print_endline "|_________|______|______|______|______|______|______|______|______|______|_________|";;

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
let get_infos board k : string =
    let square = (Array.get board k) in
        let nb_players = (List.length square.players) in
            match nb_players with
                | 0 -> "  " ^ string_of_int (get_price square.square_type) ^ "k  "
                | _ -> get_players square


