open Player
open Board
open Error
open Square
open Card
open Deck

type play =
  | Roll
  | Move of int
  | Buy of square_buyable
  | PayJail
  | PlayCard of card
  | BuyDiploma of square list
  | Change of square_buyable * square_buyable

type timeline =
  | Start
  | EndTurn
  | HandleSquare of square
  | HandleJail
  | AskDiploma
  | AskChange

type game_state = {
  board : Board.board;
  players : Player.player array;
  current_index_player : int;
  has_to_replay : bool;
  timeline : timeline;
}

type outcome = Next of game_state | Error of error | Endgame of player

(** [find index p] permet de trouver l'index d'un élément dans une liste.
    @param p la fonction de recherche
    @return l'index de l'élément trouvé ou None
*)
let find_index p =
  let rec aux i = function
    | [] -> None
    | a :: l -> if p a then Some i else aux (i + 1) l
  in
  aux 0

(** [get_current_player game_state] renvoie le joueur courant.
    @param game_state l'état du jeu
    @return le joueur courant
*)
let get_current_player game_state =
  game_state.players.(game_state.current_index_player)

(** [update_current_player game_state player] met à jour le joueur courant.
    @param game_state l'état du jeu
    @param player le joueur à mettre à jour
    @return unit
*)
let update_current_player game_state player =
  game_state.players.(game_state.current_index_player) <- player

(** [has_to_replay game_state] renvoie si le joueur doit rejouer.
    @param game_state l'état du jeu
    @return true si le joueur doit rejouer, false sinon
*)
let has_to_replay game_state = game_state.has_to_replay

(** [end_turn game_state] termine le tour du joueur.
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
let end_turn game_state =
  {
    game_state with
    current_index_player =
      (game_state.current_index_player + 1) mod Array.length game_state.players;
    has_to_replay = false;
    timeline = Start;
  }

(** [get_timeline game_state] renvoie la timeline du jeu.
    @param game_state l'état du jeu
    @return la timeline du jeu
*)
let get_timeline game_state = game_state.timeline

(** [roll_dices ()] lance les dés.
    @return un couple de dés
*)
let roll_dices () =
  let d1, d2 = (Random.int 6 + 1, Random.int 6 + 1) in
  print_endline "";
  print_endline
    ("Résultat des dés : " ^ string_of_int d1 ^ " , " ^ string_of_int d2);
  if d1 = d2 then print_endline "Double !";
  (d1, d2)

(** [handle_index_player player game_state f] permet de manipuler un joueur en fonction de son index.
    @param player le joueur
    @param game_state l'état du jeu
    @param f la fonction de manipulation
    @return le résultat de la fonction de manipulation
*)
let handle_index_player player game_state f =
  match find_index_player player game_state.players with
  | Some index -> f index
  | None -> Error InvalidPlayer

(** [handle_eliminate_player game_state] permet de gérer l'élimination d'un joueur.
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
let handle_eliminate_player game_state =
  print_endline
    "Vous n'avez pas assez d'argent pour effectuer cette action. Vous avez \
     perdu.";
  (* TODO : Enlever toutes les propriétés du joueur *)
  let cur_player = get_current_player game_state in
  let player_index =
    Option.get (find_index_player cur_player game_state.players)
  in
  remove_all_properties_board player_index game_state.board;
  Player.eliminate_player cur_player |> fun player ->
  update_current_player game_state player;
  let players_not_eliminated =
    List.filter
      (fun p -> not (Player.is_eliminated p))
      (Array.to_list game_state.players)
  in
  if List.length players_not_eliminated = 1 then
    Endgame (List.hd players_not_eliminated)
  else Next { game_state with timeline = EndTurn }

(** [calculate_course_fee cours owner_index game_state] calcule le prix d'une matière en fonction de son propriétaire.
    @param cours la matière
    @param owner_index l'index du propriétaire
    @param game_state l'état du jeu
    @return le prix de la matière
*)
let calculate_course_fee cours owner_index game_state =
  get_adjusted_course_landing_price_board cours owner_index game_state.board

(** [calculate_restaurant_fee owner_index game_state] calcule le prix d'un restaurant en fonction de son propriétaire.
    @param owner_index l'index du propriétaire
    @param game_state l'état du jeu
    @return le prix du restaurant
*)
let calculate_restaurant_fee owner_index game_state =
  let d1, d2 = roll_dices () in
  let multiplier =
    if count_restaurants_owned_board owner_index game_state.board >= 2 then 10
    else 4
  in
  let fee = (d1 + d2) * multiplier in
  print_endline ("Vous payez " ^ string_of_int fee ^ "k de consommation.");
  fee

(** [calculate_library_fee owner_index game_state] calcule le prix d'une bibliothèque en fonction de son propriétaire.
    @param owner_index l'index du propriétaire
    @param game_state l'état du jeu
    @return le prix de la bibliothèque
*)
let calculate_library_fee owner_index game_state =
  let fee =
    match count_librairies_owned_board owner_index game_state.board with
    | 1 -> 25
    | 2 -> 50
    | 3 -> 100
    | 4 -> 200
    | _ -> 0
  in
  print_endline ("Vous louez une salle pour " ^ string_of_int fee ^ "k.");
  fee

(** [calculate_fee square_buyable owner_index game_state] calcule le prix d'une propriété en fonction de son propriétaire.
    @param square_buyable la propriété
    @param owner_index l'index du propriétaire
    @param game_state l'état du jeu
    @return le prix de la propriété
*)
let calculate_fee square_buyable owner_index game_state =
  match get_type_square square_buyable with
  | Cours cours -> calculate_course_fee cours owner_index game_state
  | Restaurant _ -> calculate_restaurant_fee owner_index game_state
  | Library _ -> calculate_library_fee owner_index game_state

(** [handle_payment game_state player owner amount_to_pay] permet de gérer un paiement entre deux joueurs.
    @param game_state l'état du jeu
    @param player le joueur payant
    @param owner le joueur recevant le paiement
    @param amount_to_pay le montant à payer
    @return le nouvel état du jeu
*)
let handle_payment game_state player owner amount_to_pay =
  if money_player player < amount_to_pay then handle_eliminate_player game_state
  else
    let player = change_money player (-amount_to_pay) in
    let owner = change_money owner amount_to_pay in
    update_current_player game_state player;
    game_state.players.(Option.get (find_index_player owner game_state.players)) <-
      owner;
    print_endline
      (name_player player ^ " a payé "
      ^ string_of_int amount_to_pay
      ^ "k à " ^ name_player owner);
    Next { game_state with timeline = AskDiploma }

(** [pay_owner game_state player square_buyable] permet de payer le propriétaire d'une propriété.
    @param game_state l'état du jeu
    @param player le joueur payant
    @param square_buyable la propriété
    @return le nouvel état du jeu
*)
let pay_owner game_state player square_buyable =
  match get_owner square_buyable game_state.players with
  | Some owner when owner != player -> (
      match find_index_player owner game_state.players with
      | Some owner_index ->
          let amount_to_pay =
            calculate_fee square_buyable owner_index game_state
          in
          handle_payment game_state player owner amount_to_pay
      | None -> Error InvalidPlayer)
  | _ -> Next { game_state with timeline = AskDiploma }

(** [goto game_state player i] permet de déplacer un joueur à une position donnée.
    @param game_state l'état du jeu
    @param player le joueur
    @param i la position
    @return le nouvel état du jeu
*)
let rec goto game_state player i =
  update_current_player game_state (change_pos player i);
  act (get_current_player game_state) (Move 0) game_state

(** [handle_roll player game_state] permet de gérer le lancer de dés.
    @param player le joueur
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_roll player game_state =
  roll_dices () |> fun (n, m) ->
  if Player.is_in_jail player then
    if n = m then (
      print_endline "Vous sortez de prison";
      toogle_to_jail player false |> fun player ->
      update_current_player game_state player;
      act player Roll game_state)
    else
      Player.add_turn_jail (get_current_player game_state) |> fun player ->
      if Player.get_turn_jail (get_current_player game_state) >= 3 then (
        print_endline "Tour 3 : vous êtes obligé de payer pour sortir de prison";
        act player PayJail game_state)
      else (
        update_current_player game_state player;
        Next { game_state with timeline = HandleJail })
  else act player (Move (n + m)) { game_state with has_to_replay = n = m }

(** [handle_email player game_state] permet de gérer un email.
    @param player le joueur
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_email player game_state =
  let card, _ = Deck.draw_card (init_email_deck ()) in
  print_endline "Vous recevez un email de l'administration : ";
  print_endline (Card.get_name card);
  print_endline (Card.get_description card);
  act player (PlayCard card) game_state

(** [handle_stlife player game_state] permet de gérer un événement Vie étudiante.
    @param player le joueur
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_stlife player game_state =
  let card, _ = Deck.draw_card (init_stlife_deck ()) in
  print_endline "Vous recevez un message de l'association Vie étudiante : ";
  print_endline (Card.get_name card);
  print_endline (Card.get_description card);
  act player (PlayCard card) game_state

(** [handle_buyable_square player square game_state] permet de gérer une propriété achetable.
    @param player le joueur
    @param square la propriété
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_buyable_square player square game_state =
  match get_type_square square with
  | Restaurant r ->
      let name_restaurant = get_name_restaurant r in
      if name_restaurant = "Barge" then
        print_endline
          "Vous arrivez à la Barge (si elle appartient déjà à quelqu'un, vous \
           dépensez en fonction de votre chance aux dés)"
      else if name_restaurant = "Crous" then
        print_endline
          "Vous arrivez au Crous (s'il appartient déjà à quelqu'un, vous \
           dépensez en fonction de votre chance aux dés)"
      else
        print_endline
          ("Vous arrivez au " ^ name_restaurant
         ^ "(vous dépensez en fonction de votre chance aux dés)");
      if get_owner square game_state.players != None then
        pay_owner game_state player square
      else
        Next
          {
            game_state with
            timeline =
              HandleSquare
                (Board.get_square (pos_player player) game_state.board);
          }
  | Library _ ->
      print_endline "Vous arrivez à une Bibliothèque Universitaire.";
      if get_owner square game_state.players != None then
        pay_owner game_state player square
      else
        Next
          {
            game_state with
            timeline =
              HandleSquare
                (Board.get_square (pos_player player) game_state.board);
          }
  | _ ->
      if get_owner square game_state.players != None then
        pay_owner game_state player square
      else
        Next
          {
            game_state with
            timeline =
              HandleSquare
                (Board.get_square (pos_player player) game_state.board);
          }

(** [handle_cheating game_state] permet de gérer la triche.
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_cheating game_state =
  print_endline "Vous avez triché ! Vous êtes envoyé en prison !";
  update_current_player game_state
    (Player.toogle_to_jail (get_current_player game_state) true);
  goto game_state (get_current_player game_state) 10

(** [handle_tax player tax_square game_state] permet de gérer une taxe.
    @param player le joueur
    @param tax_square la case taxe
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_tax player tax_square game_state =
  let tax_amount = get_tax_amount (Tax tax_square) in
  if money_player player < tax_amount then handle_eliminate_player game_state
  else
    let update_player = change_money player (-tax_amount) in
    update_current_player game_state update_player;
    print_endline
      (name_player player ^ " a payé une taxe de " ^ string_of_int tax_amount
     ^ "k.");
    Next { game_state with timeline = AskDiploma }

(** [handle_square player game_state] permet de gérer une case.
    @param player le joueur
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_square player game_state =
  match
    Board.get_square
      (pos_player (get_current_player game_state))
      game_state.board
  with
  | Email -> handle_email player game_state
  | StLife -> handle_stlife player game_state
  | Buyable square -> handle_buyable_square player square game_state
  | Cheating -> handle_cheating game_state
  | Tax tax_square -> handle_tax player tax_square game_state
  | _ ->
      Next
        {
          game_state with
          timeline =
            HandleSquare (Board.get_square (pos_player player) game_state.board);
        }

(** [handle_move player n game_state] permet de gérer un déplacement.
    @param player le joueur
    @param n le nombre de cases à avancer
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_move player n game_state =
  let old_position = pos_player player in
  let new_position = (old_position + n) mod 40 in
  let player, stop_on_maison =
    if new_position = 0 then (
      print_endline
        "Vous vous arrêtez quelques jours à la maison, vos parents se montrent \
         plus généreux, recevez 400 k";
      (change_money player 400, true))
    else (player, false)
  in
  let player =
    if old_position > new_position && not stop_on_maison then (
      print_endline "Vous êtes de passage chez vous, recevez 200 k";
      change_money player 200)
    else player
  in
  change_pos player new_position |> fun player ->
  handle_index_player player game_state (fun index ->
      game_state.players.(index) <- player;
      display game_state.board game_state.players
        game_state.current_index_player;
      handle_square player game_state)

(** [handle_buy player square_buyable game_state] permet de gérer l'achat d'une propriété.
    @param player le joueur
    @param square_buyable la propriété
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_buy player square_buyable game_state =
  if price_buyable (get_type_square square_buyable) > money_player player then
    handle_eliminate_player game_state
  else
    change_money player (-price_buyable (get_type_square square_buyable))
    |> fun player ->
    change_owner square_buyable (Some game_state.current_index_player)
    |> fun square ->
    game_state.players.(game_state.current_index_player) <- player;
    Board.change_square (pos_player player) square game_state.board;
    print_endline
      (name_player player ^ " a acheté "
      ^ string_of_int (price_buyable (get_type_square square_buyable))
      ^ "k " ^ name_square square);
    Next { game_state with timeline = AskDiploma }

(** [handle_pay_jail player game_state] permet de gérer le paiement pour sortir de prison.
    @param player le joueur
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_pay_jail player game_state =
  if money_player player < 500 then handle_eliminate_player game_state
  else
    change_money player (-500) |> fun player ->
    update_current_player game_state (toogle_to_jail player false);
    Next { game_state with timeline = Start }

(** [handle_change square1 square2 game_state] permet de gérer un échange de propriétés.
    @param square1 la première propriété
    @param square2 la seconde propriété
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_change square1 square2 game_state =
  let player_index = square1.proprietaire_index in
  let player_index2 = square2.proprietaire_index in
  let index_square =
    get_index_from_square_buyable_board square1 game_state.board
  in
  let index_square2 =
    get_index_from_square_buyable_board square2 game_state.board
  in
  change_square (Option.get index_square)
    (change_owner square1 player_index2)
    game_state.board;
  change_square (Option.get index_square2)
    (change_owner square2 player_index)
    game_state.board;
  Next { game_state with timeline = EndTurn }

(** [handle_play_card player card game_state] permet de gérer l'utilisation d'une carte.
    @param player le joueur
    @param card la carte
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and handle_play_card player card game_state =
  let updated_player = Card.apply_card_effect player card in
  update_current_player game_state (fst updated_player);
  match snd updated_player with
  | false -> Next { game_state with timeline = AskDiploma }
  | true -> act (get_current_player game_state) (Move 0) game_state

(** [handle_buy_diploma list_square game_state player] permet de gérer l'achat d'un diplôme.
    @param list_square la liste des matières
    @param game_state l'état du jeu
    @param player le joueur
    @return le nouvel état du jeu
*)
and handle_buy_diploma list_square game_state player =
  let rec buy_diploma list_square game_state_1 player =
    match list_square with
    | [] -> Next { game_state_1 with timeline = EndTurn }
    | square :: list ->
        if
          owns_all_courses_in_ufr_board
            (get_ufr (get_cours_from_square square))
            game_state_1.current_index_player game_state_1.board
        then
          let diploma_price =
            get_upgrade_price (get_cours_from_square square)
          in
          if money_player player < diploma_price then
            handle_eliminate_player game_state_1
          else
            match square with
            | Buyable square_buyable -> (
                match get_type_square square_buyable with
                | Cours existing_cours
                  when get_name_cours existing_cours
                       = get_name_cours (get_cours_from_square square) -> (
                    let updated_square_buyable = update_degre square_buyable in
                    let player = change_money player (-diploma_price) in
                    update_current_player game_state_1 player;
                    get_index_from_square_board square game_state_1.board
                    |> fun index ->
                    match index with
                    | None -> Error InvalidSquare
                    | Some index ->
                        Board.change_square index
                          (Buyable updated_square_buyable) game_state_1.board;
                        print_endline
                          (name_player player ^ " a acheté un diplôme pour "
                          ^ get_name_cours existing_cours);
                        buy_diploma list game_state_1 player)
                | _ -> Error InvalidAction)
            | _ -> Error InvalidSquare
        else Error InvalidAction
  in
  buy_diploma list_square game_state player

(** [act player play game_state] permet de gérer une action.
    @param player le joueur
    @param play l'action
    @param game_state l'état du jeu
    @return le nouvel état du jeu
*)
and act player play game_state =
  if Player.is_eliminated (get_current_player game_state) then
    Error InvalidPlayer
  else
    match play with
    | Roll -> handle_roll player game_state
    | Move n -> handle_move player n game_state
    | Buy square_buyable -> handle_buy player square_buyable game_state
    | PayJail -> handle_pay_jail player game_state
    | Change (square1, square2) -> handle_change square1 square2 game_state
    | PlayCard card -> handle_play_card player card game_state
    | BuyDiploma list_square -> handle_buy_diploma list_square game_state player

(** [create_game board players] permet de créer une partie.
    @param board le plateau
    @param players les joueurs
    @return la partie
*)
let create_game board players =
  {
    board;
    players;
    current_index_player = 0;
    has_to_replay = false;
    timeline = Start;
  }

(* demande d'achat d'une propriété *)
let ask_buy square_buyable =
  print_endline
    ("Voulez-vous acheter "
    ^ name_square (Buyable square_buyable)
    ^ " pour "
    ^ string_of_int (price_buyable (get_type_square square_buyable))
    ^ "k ? (y/n)");
  let rec ask_buy () =
    match read_line () with
    | "y" -> true
    | "n" -> false
    | _ ->
        print_endline "Veuillez entrer y ou n";
        ask_buy ()
  in
  ask_buy ()

(** [ask_change game_state play] demande à l'utilisateur s'il veut échanger une propriété.
    @param game_state l'état du jeu
    @param play l'action
    @return unit
*)
let rec ask_change game_state endturn turn =
  print_endline "";
  print_endline
    "Voulez-vous échanger une propriété avec un autre joueur ? Tapez 'y' pour \
     échanger ou Enter pour passer :";
  match read_line () with
  | "y" -> handle_exchange game_state endturn turn
  | _ -> endturn game_state

and handle_exchange game_state endturn turn =
  let properties_owned =
    get_properties_owned_by_player_board game_state.current_index_player
      game_state.board
  in
  if List.length properties_owned = 0 then (
    print_endline "Vous n'avez pas de propriété à échanger.";
    endturn game_state)
  else (
    print_endline "Voici les joueurs :";
    let active_players = Array.of_list (Array.to_list game_state.players |> List.filter (fun player -> not (is_eliminated player))) in
    Array.iteri
      (fun i player ->
        print_endline (string_of_int i ^ " : " ^ name_player player))
      active_players;
    print_endline
      "Entrez le numéro du joueur avec lequel vous souhaitez échanger ou taper \
       enter pour passer :";
    match read_line () with
    | "" -> endturn game_state
    | s -> handle_player_selection s game_state endturn turn properties_owned active_players)

and handle_player_selection s game_state endturn turn properties_owned list_players =
  let index = int_of_string_opt s in
  match index with
  | None ->
      print_endline "Numéro de joueur invalide. Fin de l'échange.";
      endturn game_state
  | Some index ->
      if
        index >= 0
        && index < Array.length list_players
        && index != game_state.current_index_player
      then
        let properties =
          get_properties_owned_by_player_board index game_state.board
        in
        if List.length properties = 0 then (
          print_endline "Ce joueur n'a pas de propriété à échanger.";
          endturn game_state)
        else (
          print_endline "Voici les propriétés de ce joueur :";
          List.iter
            (fun square ->
              print_endline
                (string_of_int
                   (Option.get (find_index (fun s -> s = square) properties))
                ^ " : "
                ^ name_square (create_buyable (get_type_square square))))
            properties;
          print_endline "";
          print_endline
            "Entrez le numéro de la propriété que vous souhaitez échanger ou \
             taper enter pour passer :";
          match read_line () with
          | "" -> endturn game_state
          | s ->
              handle_property_selection s game_state endturn turn properties
                properties_owned)
      else (
        print_endline "Numéro de joueur invalide. Fin de l'échange.";
        endturn game_state)

and handle_property_selection s game_state endturn turn properties
    properties_owned =
  let index = int_of_string_opt s in
  match index with
  | None ->
      print_endline "Numéro de propriété invalide. Fin de l'échange.";
      endturn game_state
  | Some index ->
      if index >= 0 && index < List.length properties then (
        print_endline "Voici vos propriétés :";
        List.iter
          (fun square ->
            print_endline
              (string_of_int
                 (Option.get
                    (find_index (fun s -> s = square) properties_owned))
              ^ " : "
              ^ name_square (create_buyable (get_type_square square))))
          properties_owned;
        print_endline "";
        print_endline
          "Entrez le numéro de la propriété que vous souhaitez donner en \
           échange ou taper enter pour annuler l'échange :";
        match read_line () with
        | "" -> endturn game_state
        | s ->
            handle_player_property_selection s game_state endturn turn
              properties properties_owned index)
      else (
        print_endline "Numéro de propriété invalide. Fin de l'échange.";
        endturn game_state)

and handle_player_property_selection s game_state endturn turn properties
    properties_owned index =
  let index_player = int_of_string_opt s in
  match index_player with
  | None ->
      print_endline "Numéro de propriété invalide. Fin de l'échange.";
      endturn game_state
  | Some index_player ->
      if index_player >= 0 && index_player < List.length properties_owned then
        turn
          (Change
             (List.nth properties index, List.nth properties_owned index_player))
          game_state
      else (
        print_endline "Numéro de propriété invalide. Fin de l'échange.";
        endturn game_state)

(** [get_eligible_courses player_index board] renvoie les matières pour lesquelles un joueur peut acheter un diplôme.
    @param player_index l'index du joueur
    @param board le plateau
    @return la liste des matières
*)
let get_eligible_courses player_index board =
  let courses = get_courses_owned_by_player_board player_index board in
  let eligible_courses_with_D =
    List.filter
      (fun square ->
        owns_all_courses_in_ufr_board
          (get_ufr (get_cours_from_square square))
          player_index board)
      courses
  in
  List.filter
    (fun square -> Option.get (get_degre_square square) != Some Doctorat)
    eligible_courses_with_D

(** [display_eligible_courses eligible_courses] affiche les matières pour lesquelles un joueur peut acheter un diplôme.
    @param eligible_courses la liste des matières
    @return unit
*)
let display_eligible_courses eligible_courses =
  print_endline
    "Vous avez la possibilité d'acheter un diplôme pour une de vos matières :";
  List.iter
    (fun square ->
      print_endline
        (string_of_int
           (Option.get
              (find_index
                 (fun c ->
                   get_cours_from_square c = get_cours_from_square square)
                 eligible_courses))
        ^ " : "
        ^ get_name_cours (get_cours_from_square square)))
    eligible_courses;
  print_endline
    "Entrez le(s) numéro(s) de la matière pour lequels vous souhaitez acheter \
     un diplôme ou taper enter pour passer (séparer les numéros avec des \
     espaces. Ex : 0 2 3"

(** [parse_user_input input eligible_courses] permet de parser l'entrée de l'utilisateur.
    @param input l'entrée de l'utilisateur
    @param eligible_courses la liste des matières
    @return la liste des matières sélectionnées
*)
let parse_user_input input eligible_courses =
  let indices =
    try
      let indices = String.split_on_char ' ' input in
      List.map int_of_string indices
    with Failure _ ->
      print_endline "Entrée non valide. Fin de l'achat.";
      [] (* Si la conversion échoue, retourne une liste vide *)
  in
  let indices =
    List.filter (fun i -> i >= 0 && i < List.length eligible_courses) indices
  in
  List.map (fun i -> List.nth eligible_courses i) indices

(** [ask_for_diploma_purchase game_state endturn turn] demande à l'utilisateur s'il veut acheter un diplôme.
    @param game_state l'état du jeu
    @param endturn la fonction de fin de tour
    @param turn la fonction de tour
    @return unit
*)
let ask_for_diploma_purchase game_state endturn turn =
  let player_index = game_state.current_index_player in
  let eligible_courses = get_eligible_courses player_index game_state.board in

  if List.length eligible_courses = 0 then ask_change game_state endturn turn
  else (
    display_eligible_courses eligible_courses;
    match read_line () with
    | "" -> ask_change game_state endturn turn
    | s ->
        let courses_to_buy = parse_user_input s eligible_courses in
        turn (BuyDiploma courses_to_buy) game_state)

(** [play game_state] is the main function of the game. It displays the board, handles the turn and the end of the turn.
    @param game_state the current state of the game
    @return unit
*)
let rec play (game_state : game_state) =
  (* End the turn *)
  let endturn game_state =
    print_endline "";
    print_endline
      "Appuyez sur Entrée pour terminer votre tour ou tapez 'end' pour \
       terminer le jeu : ";
    match read_line () with
    | "end" -> exit 0
    | _ -> (
        match has_to_replay game_state with
        | true -> play game_state
        | false -> play (end_turn game_state))
  in

  (* Handle errors *)
  let handle_error error =
    match error with
    | InvalidPlayer ->
        print_endline "Erreur : joueur introuvable. arrêt du jeu."
    | NoOwner ->
        print_endline "Erreur : propriétaire introuvable. arrêt du jeu."
    | InvalidBoard -> print_endline "Erreur : plateau corrompu. arrêt du jeu."
    | InvalidMove -> print_endline "Erreur : mouvement invalide. arrêt du jeu."
    | InvalidSquare -> print_endline "Erreur : case invalide. arrêt du jeu."
    | InvalidAction -> print_endline "Erreur : action impossible. arrêt du jeu."
  in

  (* Handle jail *)
  let rec ask_jail game_state turn =
    print_endline "";
    print_endline
      "Vous êtes en prison. Vous avez 3 tours pour sortir. Vous pouvez payer \
       500k pour sortir immédiatement.";
    print_endline
      ("Vous avez actuellement "
      ^ string_of_int (Player.get_turn_jail (get_current_player game_state))
      ^ " tours en prison.");
    let has_alibi = can_use_alibi (get_current_player game_state) in
    if has_alibi then
      print_endline
        "Vous pouvez également utiliser votre carte Alibi pour sortir de \
         prison.";
    print_endline
      (if has_alibi then
         "Voulez-vous payer pour sortir de prison ? (y/n/a) \n\
          (a = utiliser la carte Alibi)"
       else "Voulez-vous payer pour sortir de prison ? (y/n) ");
    match read_line () with
    | "y" -> turn PayJail game_state
    | "n" -> endturn game_state
    | "a" when has_alibi ->
        let player = get_current_player game_state in
        let updated_player = Player.use_alibi_card player in
        update_current_player game_state
          (Player.toogle_to_jail updated_player false);
        print_endline
          "Vous avez utilisé votre carte Alibi pour sortir de prison.";
        turn Roll game_state
    | _ -> ask_jail game_state turn
  in

  (* Handle the turn *)
  let rec turn play game_state =
    if Player.is_eliminated (get_current_player game_state) then (
      print_endline
        (name_player (get_current_player game_state) ^ " a été éliminé.");
      endturn game_state)
    else
      act (get_current_player game_state) play game_state |> function
      | outcome -> (
          match outcome with
          | Error error -> handle_error error
          | Endgame player ->
              print_endline (name_player player ^ " a gagné la partie !");
              exit 0
          | Next game_state -> (
              match get_timeline game_state with
              | Start -> turn Roll game_state
              | HandleSquare square -> (
                  match square with
                  | Square.Buyable square_buyable -> (
                      match
                        Square.get_owner square_buyable game_state.players
                      with
                      | None -> (
                          match ask_buy square_buyable with
                          | true -> turn (Buy square_buyable) game_state
                          | false -> endturn game_state)
                      | Some _owner ->
                          print_endline
                            "Erreur : propriétaire déjà existant. arrêt du jeu."
                      )
                  | _ -> endturn game_state)
              | HandleJail -> ask_jail game_state turn
              | AskDiploma -> ask_for_diploma_purchase game_state endturn turn
              | AskChange -> ask_change game_state endturn turn
              | EndTurn -> endturn game_state))
  in
  turn Roll game_state
