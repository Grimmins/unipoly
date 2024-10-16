type board = Square.square array
open Square
open Player
let show_board (board : board) () =
    board.(0).players |> List.iter (fun player -> print_endline (Player.name_player player));

    print_endline "____________________________________________________________________________________";
    print_endline "|Vacances |Géolog|      | Bio  |Chimie|Bibli |Optiqu| Elec |      | Meca |Suspicion|";
    print_endline "|         | 25k  |      |      |      |      |      |      |      |      |         |";
    print_endline "|         |______|      |______|______|      |______|______|      |______|De Triche|";
    print_endline "|_________|*_____|______|*_____|*_____|______|°_____|°_____|______|°_____|_________|";
    print_endline "|Marketi|$|                                                              |+| Proba |";
    print_endline "|       | |                                                              | |  25k  |";
    print_endline "|_______|_|                                                              |_|_______|";
    print_endline "|Finance|$|                                                              |+|Analyse|";
    print_endline "|       | |                                                              | |       |";
    print_endline "|_______|_|                                                              |_|_______|";
    print_endline "|         |                                                              |         |";
    print_endline "|         |                                                              |         |";
    print_endline "|_________|                                                              |_________|";
    print_endline "| Socio |$|                                                              |+|Algèbre|";
    print_endline "|       | |                                                              | | @E @L |";
    print_endline "|_______|_|                                                              |_|_______|";
    print_endline "|  Bibli  |                                                              |  Bibli  |";
    print_endline "|         |                                                              |         |";
    print_endline "|_________|                                                              |_________|";
    print_endline "| Droit |^|                                                              |         |";
    print_endline "|       | |                                                              |         |";
    print_endline "|_______|_|                                                              |_________|";
    print_endline "|Géograp|^|                                                              |#| Algo  |";
    print_endline "|       | |                                                              | |       |";
    print_endline "|_______|_|                                                              |_|_______|";
    print_endline "|         |                                                              |         |";
    print_endline "|         |                                                              |         |";
    print_endline "|_________|                                                              |_________|";
    print_endline "|Histoir|^|                                                              |#| OCaml |";
    print_endline "|       | |                                                              | |       |";
    print_endline "|_______|_|______________________________________________________________|_|_______|";
    print_endline "|  |Triche|%_____|%_____|      |%_____| BNF  |      |&_____|      |&_____| Maison  |";
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


