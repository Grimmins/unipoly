open Player
open Board
open Error
open Square

type play = 
  | Roll
  | Move of int
  | Buy of square_buyable
  | PayJail

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
      if money_player player < price_buyable (get_type_square square_buyable) then Error (NotEnoughMoney)
      else 
        change_money player (- (price_buyable (get_type_square square_buyable))) |> fun player -> 
        change_money owner (price_buyable (get_type_square square_buyable)) |> fun owner -> 
          update_current_player game_state player;
        game_state.players.(find_index_player owner game_state.players |> Option.get) <- owner;
        print_endline (name_player player ^ " a payé " ^ name_player owner ^ " " ^ string_of_int (price_buyable (get_type_square square_buyable)) ^ "€");
        Next {game_state with timeline = EndTurn}
    | None -> Error (NoOwner))

let rec act player play game_state = 
  let goto game_state player i = 
    (* TODO : Gérer passage case départ *)
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
  | Move n -> change_pos player ((pos_player player + n) mod 40) |> fun player ->
      (* change player into current_player and players *)
      handle_index_player player game_state (fun index ->

        game_state.players.(index) <- player; 
        display game_state.board game_state.players;

        match Board.get_square (pos_player (get_current_player game_state)) game_state.board with 

          | Buyable square -> (if get_owner square game_state.players != None then pay_owner game_state player square 
            else Next {game_state with timeline = HandleSquare (Board.get_square (pos_player player) game_state.board)})

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
    
    | HandleJail -> let rec ask_jail () = (print_endline "";
        print_endline "Vous êtes en prison. Vous avez 3 tours pour sortir. Vous pouvez payer 500€ pour sortir immédiatement.";
        print_endline ("Vous avez actuellement " ^ string_of_int (Player.get_turn_jail (get_current_player game_state)) ^ " tours en prison.");
        print_endline "Voulez-vous payer pour sortir de prison ? (y/n) ";
        match read_line () with
          | "y" -> turn (PayJail) game_state
          | "n" -> (
            
            endturn game_state)
          | _ -> ask_jail ())
        in ask_jail ()
    | EndTurn -> endturn game_state) 
    
    
      in turn Roll game_state)
    