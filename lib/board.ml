
type board = Square.square array

  let display (board : board) = 
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
    print_endline "|       | |                                                              | |  @E   |";
    print_endline "|_______|_|                                                              |_|_______|";
    print_endline "|         |                                                              |         |";
    print_endline "|         |                                                              |         |";
    print_endline "|_________|                                                              |_________|";
    print_endline "| Socio |$|                                                              |+|Algèbre|";
    print_endline "|       | |                                                              | |       |";
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



  let init_board () = [|
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