open Player
open Board
open Error
open Square
open Card
open Deck
open Property

type play = 
  | Roll
  | Move of int
  | Buy of square_buyable
  | PayJail
  | PlayCard of card
  | BuyDiploma of square list

type timeline  =
  | Start
  | EndTurn
  | HandleSquare of square
  | HandleJail
  | AskDiploma

type game_state = {
  board : Board.board;
  players : Player.player array;
  current_index_player : int;
  has_to_replay : bool;
  timeline : timeline;
}

  type outcome =
  | Next of game_state
  | Error of error
(*| | Endgame of player option*)

let find_index p =
  let rec aux i = function
    [] -> None
    | a::l -> if p a then Some i else aux (i+1) l in
  aux 0

let get_current_player game_state = game_state.players.(game_state.current_index_player)

let update_current_player game_state player = game_state.players.(game_state.current_index_player) <- player

let has_to_replay game_state = game_state.has_to_replay

let end_turn game_state = {game_state with current_index_player = (game_state.current_index_player + 1) mod (Array.length game_state.players); has_to_replay = false; timeline= Start}

let get_timeline game_state = game_state.timeline

let roll_dices () =
  let (d1, d2) = (Random.int 6 + 1,
  Random.int 6 + 1) in 
  print_endline "";
  print_endline ("Résultat des dés : " ^ string_of_int d1 ^ " , " ^ string_of_int d2);
  if d1 = d2 then print_endline "Double !";
  (d1, d2)

(* Handle the int option when finding the index of player *)
let handle_index_player player game_state f  = 
  match find_index_player player game_state.players with
  | Some index -> f index
  | None -> Error (InvalidPlayer)

(* paiement au propriétaire *)
let pay_owner game_state player square_buyable =
  match get_owner square_buyable game_state.players with
  | Some owner when owner != player ->  (* Si le propriétaire n'est pas le joueur lui-même *)
      (match find_index_player owner game_state.players with
      | Some owner_index ->
          let amount_to_pay =
            match get_type_square square_buyable with
            | Cours cours ->
                get_adjusted_course_landing_price cours owner_index game_state.board
            | Restaurant _ ->
                let (d1, d2) = roll_dices () in
                let multiplier =
                  if count_restaurants_owned owner_index game_state.board >= 2 then 10 else 4
                in
                let fee = (d1 + d2) * multiplier in
                print_endline ("Vous payez " ^ string_of_int fee ^ "€ de consommation.");
                fee
            | Library _ ->
                let fee =
                  match count_librairies_owned owner_index game_state.board with
                  | 1 -> 25
                  | 2 -> 50
                  | 3 -> 100
                  | 4 -> 200
                  | _ -> 0
                in
                print_endline ("Vous louez une salle pour " ^ string_of_int fee ^ "€.");
                fee
          in
          if money_player player < amount_to_pay then Error (NotEnoughMoney)
          else
            let player = change_money player (-amount_to_pay) in
            let owner = change_money owner amount_to_pay in
            update_current_player game_state player;
            game_state.players.(Option.get (find_index_player owner game_state.players)) <- owner;
            print_endline (name_player player ^ " a payé " ^ string_of_int amount_to_pay ^ "€ à " ^ name_player owner);
            Next { game_state with timeline = AskDiploma }
      | None -> Error (InvalidPlayer))
  | _ ->  (* Aucun propriétaire ou le propriétaire est le joueur lui-même *)
      Next { game_state with timeline = AskDiploma }


let rec act player play game_state = 
  let goto game_state player i =

    update_current_player game_state (change_pos player i);
    act (get_current_player game_state) (Move 0) game_state in


  match play with

  (* lancer de dés *)
  | Roll -> roll_dices () |> fun (n,m) ->
      (* Le joueur est en prison *)
      ( if Player.is_in_jail player then

          (if n = m then (print_endline "Vous sortez de prison";
          toogle_to_jail player false |> fun player -> (
          update_current_player game_state player;
          act player Roll game_state))
          else (
            Player.add_turn_jail (get_current_player game_state) |> fun player ->
            if (Player.get_turn_jail (get_current_player game_state) >= 3) then
              ((print_endline "Tour 3 : vous êtes obligé de payer pour sortir de prison";
                  act player PayJail game_state))
            else (
            update_current_player game_state player;
            Next {game_state with timeline = HandleJail})))

      (* Le joueur n'est pas en prison *)
      else act player (Move (n + m)) {game_state with has_to_replay = (n = m)};
      )

  (* déplacement du joueur *)
  | Move n ->
  (* calcul position *)
  let old_position = pos_player player in
      let new_position = (old_position + n) mod 40 in
    (*traiter si le joueur passe par la case départ (autrement dit si la position passe de x < 40 à x > 0  ET/OU s'arrête sur la case *)
      let player, stop_on_maison =
              if new_position = 0 then (
              print_endline "Vous vous arrêtez quelques jours à la maison, vos parents se montrent plus généreux, recevez 400 €";
              (change_money player 400, true)
              )
              else (player, false)
          in
          let player =
                  if old_position > new_position && not stop_on_maison then (print_endline "Vous êtes de passage chez vous, recevez 200 €";
                  (change_money player 200)
                  )
                  else player
              in
  change_pos player new_position |> fun player ->
      (* change player into current_player and players *)
      handle_index_player player game_state (fun index ->

        game_state.players.(index) <- player;
        display game_state.board game_state.players game_state.current_index_player;

        match Board.get_square (pos_player (get_current_player game_state)) game_state.board with

          | Email -> let (card, _) = Deck.draw_card (init_email_deck ()) in
          print_endline ("Vous recevez un email de l'administration : ");
          print_endline (Card.get_name card);
          print_endline (Card.get_description card);
          act player (PlayCard card) game_state

          | StLife -> let (card, _) = Deck.draw_card (init_stlife_deck ()) in
          print_endline ("Vous recevez un message de l'association Vie étudiante : ");
          print_endline (Card.get_name card);
          print_endline (Card.get_description card);
          act player (PlayCard card) game_state


          | Buyable square ->
            (match get_type_square square with
              | Restaurant r ->
              let name_restaurant = get_name_restaurant r in
                   if name_restaurant = "Barge" then
                     print_endline "Vous arrivez à la Barge (si elle appartient déjà à quelqu'un, vous dépensez en fonction de votre chance aux dés)"
                   else if name_restaurant = "Crous" then
                     print_endline "Vous arrivez au Crous (s'il appartient déjà à quelqu'un, vous dépensez en fonction de votre chance aux dés)"
                   else
                     print_endline ("Vous arrivez au " ^ name_restaurant ^ "(vous dépensez en fonction de votre chance aux dés)");
                     (if get_owner square game_state.players != None then pay_owner game_state player square
                     else Next {game_state with timeline = HandleSquare (Board.get_square (pos_player player) game_state.board)})
              | Library _ ->
                   print_endline "Vous arrivez à une Bibliothèque Universitaire.";
                   (if get_owner square game_state.players != None then pay_owner game_state player square
                   else Next {game_state with timeline = HandleSquare (Board.get_square (pos_player player) game_state.board)})
              | _ -> (if get_owner square game_state.players != None then pay_owner game_state player square
                else Next {game_state with timeline = HandleSquare (Board.get_square (pos_player player) game_state.board)}))

          | Cheating -> (print_endline "Vous avez triché ! Vous êtes envoyé en prison !";
          update_current_player game_state (Player.toogle_to_jail (get_current_player game_state) true);
          goto game_state (get_current_player game_state) 10;)

          | Tax tax_square ->
            let tax_amount = get_tax_amount (Tax tax_square) in
            if money_player player < tax_amount then
                Error (NotEnoughMoney)
            else
                let update_player = change_money player (-tax_amount) in
                update_current_player game_state update_player;
                print_endline (name_player player ^ " a payé une taxe de " ^ string_of_int tax_amount ^ "€.");
                Next { game_state with timeline = AskDiploma }
          | _ ->  Next {game_state with timeline = HandleSquare (Board.get_square (pos_player player) game_state.board)}
            )

  (* achat d'une propriété *)
   | Buy square_buyable ->
      if price_buyable (get_type_square square_buyable) > money_player player then Error (NotEnoughMoney)
      else
        change_money player (- price_buyable (get_type_square square_buyable)) |> fun player ->
        change_owner square_buyable (Some (game_state.current_index_player)) |> fun square ->

            game_state.players.(game_state.current_index_player) <- player;
            Board.change_square (pos_player player) square game_state.board ;
            (* TODO : pas dans la même fonction*)
            print_endline (name_player player ^ " a acheté " ^ string_of_int (price_buyable (get_type_square square_buyable)) ^ "€ " ^ name_square square);
            Next {game_state with timeline = AskDiploma}

  | PayJail -> (
    (* TODO : Change 500 with constant *)
    if money_player player < 500 then Error (NotEnoughMoney)
    else (change_money player (- 500) |> fun player ->
      update_current_player game_state (toogle_to_jail player false);
      Next {game_state with timeline = Start}))

  | PlayCard card ->
  let updated_player = Card.apply_card_effect player card in
  update_current_player game_state updated_player;
  Next {game_state with timeline = AskDiploma}

  | BuyDiploma list_square ->
      let player = get_current_player game_state in
      let player_index = game_state.current_index_player in
      
      
      let rec buy_diploma list_square = match list_square with
      | [] -> Next {game_state with timeline = EndTurn}
      | square :: list -> if owns_all_courses_in_ufr (get_ufr (get_cours_from_square square)) player_index game_state.board then
        let diploma_price = get_upgrade_price (get_cours_from_square square)
        in
        if money_player player < diploma_price then Error NotEnoughMoney
        else
          match square with
          | Buyable square_buyable ->
              (match get_type_square square_buyable with
              | Cours existing_cours when (get_name_cours existing_cours) = (get_name_cours (get_cours_from_square square)) ->
                  let updated_square_buyable = update_degre square_buyable in
                  let player = change_money player (-diploma_price) in
                  update_current_player game_state player;
                  ((Square.get_index_from_square square game_state.board ) |> fun index -> match index with
                  | None ->  Error InvalidSquare;
                  | Some index -> 
                    Board.change_square index (Buyable updated_square_buyable) game_state.board;
                   print_endline (name_player player ^ " a acheté un diplôme pour " ^ (get_name_cours existing_cours));
                    buy_diploma list)
                  
              | _ -> Error InvalidAction)
          | _ -> Error InvalidSquare
      else
        Error InvalidAction
       in buy_diploma list_square


let create_game board players = 
  { board;
   players;
   current_index_player = 0;
    has_to_replay = false;
    timeline = Start;
  }

(* demande d'achat d'une propriété *)
let ask_buy square_buyable =
  (print_endline ("Voulez-vous acheter " ^ name_square (Buyable (square_buyable)) ^ " pour " ^ string_of_int (price_buyable (get_type_square square_buyable)) ^ "€ ? (y/n)");
  let rec ask_buy () =
    match read_line () with
    | "y" -> true
    | "n" -> false
    | _ -> print_endline "Veuillez entrer y ou n"; ask_buy () in ask_buy ())

(* demande uograde propriété *)
let ask_for_diploma_purchase game_state endturn turn =

  let player_index = game_state.current_index_player in
  let courses = get_courses_owned_by_player player_index game_state.board in
  let eligible_courses = List.filter (fun square -> owns_all_courses_in_ufr (get_ufr (get_cours_from_square square)) player_index game_state.board) courses in

  if List.length eligible_courses = 0 then
    endturn game_state
  else 
    (print_endline "Vous avez la possibilité d'acheter un diplôme pour une de vos matières :";
    List.iter (fun square -> print_endline (string_of_int (Option.get (find_index (fun c -> (get_cours_from_square c) = (get_cours_from_square square)) eligible_courses)) ^ " : " ^ get_name_cours (get_cours_from_square square))) eligible_courses;
    print_endline "Entrez le(s) numéro(s) de la matière pour lequels vous souhaitez acheter un diplôme ou taper enter pour passer :";
    match read_line () with
    | "" -> endturn game_state
    | s -> let indices = String.split_on_char ' ' s in
      let indices = List.map (fun s -> int_of_string s) indices in
      let indices = List.filter (fun i -> i >= 0 && i < List.length eligible_courses) indices in
      let courses_to_buy = List.map (fun i -> List.nth eligible_courses i) indices in
      turn (BuyDiploma courses_to_buy) game_state)
    
    


(** [play game_state] is the main function of the game. It displays the board, handles the turn and the end of the turn.
    @param game_state the current state of the game
    @return unit
*)

let rec play (game_state : game_state) =
    (* End the turn *)
      let endturn game_state =
        (print_endline "";
          print_endline "Appuyez sur Entrée pour terminer votre tour, ou tapez 'end' pour terminer le jeu : ";
          match read_line () with
            | "end" -> exit 0
            | _ -> match has_to_replay game_state with
              | true -> play game_state
              | false -> play (end_turn game_state))
   in
  (* Handle the turn *)
  let rec turn play game_state  =
  if Player.is_eliminated (get_current_player game_state) then
    (print_endline (name_player (get_current_player game_state) ^ " a été éliminé.");
    endturn game_state)
  else
  (act (get_current_player game_state) play game_state |> function outcome -> match outcome with

  | Error error -> (
      match error with
        | NotEnoughMoney -> (print_endline "Vous n'avez pas assez d'argent pour effectuer cette action. Vous avez perdu.";
          (* TODO : Enlever toutes les propriétés du joueur *)
           Player.eliminate_player (get_current_player game_state) |> fun player -> (
            update_current_player game_state player;
            endturn game_state)
                 ) 

        | InvalidPlayer -> print_endline "Erreur : joueur introuvable. arrêt du jeu.";
        | NoOwner -> print_endline "Erreur : propriétaire introuvable. arrêt du jeu.";
        | InvalidBoard -> print_endline "Erreur : plateau corrompu. arrêt du jeu.";
        | InvalidMove -> print_endline "Erreur : mouvement invalide. arrêt du jeu.";
        | InvalidSquare -> print_endline "Erreur : case invalide. arrêt du jeu.";
        | InvalidAction -> print_endline "Erreur : action impossible. arrêt du jeu.";

  )

  (* Next turn *)
  | Next game_state -> match get_timeline game_state with
    | Start -> turn Roll game_state
    | HandleSquare square -> (match square with

      | Square.Buyable square_buyable ->

        (match Square.get_owner square_buyable game_state.players with
          | None -> ( match (ask_buy square_buyable) with
              | true -> turn (Buy square_buyable) game_state
              | false -> endturn game_state)

          | Some _owner -> print_endline "Erreur : propriétaire déjà existant. arrêt du jeu.";)

      | _ -> endturn game_state)

    | HandleJail -> let rec ask_jail () = (
        print_endline "";
        print_endline "Vous êtes en prison. Vous avez 3 tours pour sortir. Vous pouvez payer 500€ pour sortir immédiatement.";
        print_endline ("Vous avez actuellement " ^ string_of_int (Player.get_turn_jail (get_current_player game_state)) ^ " tours en prison.");
        let has_alibi = can_use_alibi (get_current_player game_state) in
        if has_alibi then
                    print_endline "Vous pouvez également utiliser votre carte Alibi pour sortir de prison.";
        print_endline
                    (if has_alibi
                     then "Voulez-vous payer pour sortir de prison ? (y/n/a) \n(a = utiliser la carte Alibi)"
                     else "Voulez-vous payer pour sortir de prison ? (y/n) ");
        match read_line () with
          | "y" -> turn (PayJail) game_state
          | "n" -> (

            endturn game_state)
          | "a" when has_alibi ->
                let player = get_current_player game_state in
                let updated_player = Player.use_alibi_card player in
                update_current_player game_state (Player.toogle_to_jail updated_player false);
                print_endline "Vous avez utilisé votre carte Alibi pour sortir de prison.";
                turn Roll game_state
          | _ -> ask_jail ())
        in ask_jail ()
    | AskDiploma -> ask_for_diploma_purchase game_state endturn turn
    | EndTurn -> endturn game_state)
      in turn Roll game_state
