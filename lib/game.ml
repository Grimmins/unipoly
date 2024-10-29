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

type timeline  =
  | Start
  | EndTurn
  | HandleSquare of square
  | HandleJail

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
  (match get_owner square_buyable game_state.players with
   | Some owner ->
     let owner_index = find_index_player owner game_state.players in
     match owner_index with
     | Some owner_index ->
       let amount_to_pay =
         match get_type_square square_buyable with
         | Restaurant _ ->
           let (d1, d2) = roll_dices () in
           let multiplier =
             let num_restaurants = count_restaurants_owned owner_index game_state.board in
             if num_restaurants >= 2 then 10 else 4
           in
           let fee = (d1 + d2) * multiplier in
           print_endline ("Vous payez " ^ string_of_int fee ^ "€ de consommation.");
           fee
         | Library _ ->
           let num_libraries = count_librairies_owned owner_index game_state.board in
           let fee = match num_libraries with
             | 1 -> 25
             | 2 -> 50
             | 3 -> 100
             | 4 -> 200
             | _ -> 0
           in
           print_endline ("Vous louez une salle pour " ^ string_of_int fee ^ "€.");
           fee
         | _ ->
           price_buyable (get_type_square square_buyable)
       in
       if money_player player < amount_to_pay then Error (NotEnoughMoney)
       else
         change_money player (-amount_to_pay) |> fun player ->
         change_money owner amount_to_pay |> fun owner ->
         update_current_player game_state player;
         game_state.players.(find_index_player owner game_state.players |> Option.get) <- owner;
         print_endline (name_player player ^ " a payé " ^ name_player owner ^ " " ^ string_of_int amount_to_pay ^ "€");
         Next {game_state with timeline = EndTurn}
     | None -> Error (InvalidPlayer) (* Ajout de gestion du cas où l'index n'est pas trouvé *)
   | None -> Error (NoOwner))



let rec act player play game_state = 
  let goto game_state player i =

    update_current_player game_state (change_pos player i);
    act (get_current_player game_state) (Move 0) game_state in


  match play with

  (* lancer de dés *)
  | Roll -> roll_dices () |> fun (n,m) ->
      (* Le joueur est en prison *)
      ( if Player.is_in_jail player then

          (if n = m then (print_endline "Vous sortez de prison"; act player Roll game_state)
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
        display game_state.board game_state.players;

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
                     print_endline "Vous arrivez à la Barge (vous dépensez en fonction de votre chance aux dés)"
                   else if name_restaurant = "Crous" then
                     print_endline "Vous arrivez au Crous (vous dépensez en fonction de votre chance aux dés)"
                   else
                     print_endline ("Vous arrivez au " ^ name_restaurant ^ "(vous dépensez en fonction de votre chance aux dés)");
                     pay_owner game_state player square
              | Library _ ->
                   print_endline "Vous arrivez à une Bibliothèque Universitaire.";
                   pay_owner game_state player square
              | _ -> (if get_owner square game_state.players != None then pay_owner game_state player square
                else Next {game_state with timeline = HandleSquare (Board.get_square (pos_player player) game_state.board)}))

          | Cheating -> (print_endline "Vous avez triché ! Vous êtes envoyé en prison !";
          update_current_player game_state (Player.toogle_to_jail (get_current_player game_state) true);
          goto game_state (get_current_player game_state) 10;)


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
            Next {game_state with timeline = EndTurn}

  | PayJail -> (
    (* TODO : Change 500 with constant *)
    if money_player player < 500 then Error (NotEnoughMoney)
    else (change_money player (- 500) |> fun player ->
      update_current_player game_state (toogle_to_jail player false);
      Next {game_state with timeline = Start}))

  | PlayCard card ->
  let updated_player = Card.apply_card_effect player card in
  update_current_player game_state updated_player;
  Next {game_state with timeline = EndTurn}


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

  (Board.display game_state.board game_state.players;

  (* Handle the turn *)
  let rec turn play game_state  =
  (act (get_current_player game_state) play game_state |> function outcome -> match outcome with

  (* TODO :  Error handling *)
  | Error _error -> print_endline "Erreur";

  (* Next turn *)
  | Next game_state -> match get_timeline game_state with
    | Start -> turn Roll game_state
    | HandleSquare square -> (match square with

      | Square.Buyable square_buyable ->

        (match Square.get_owner square_buyable game_state.players with
          | None -> ( match (ask_buy square_buyable) with
              | true -> turn (Buy square_buyable) game_state
              | false -> endturn game_state)

          (* TODO :  Error handling *)
          | Some _owner -> print_endline "Erreur")

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
    | EndTurn -> endturn game_state)


      in turn Roll game_state)
