open Unipoly
open Game

let board = Board.init_board ()
let game_state = create_game board [|Player.create_player "A"; Player.create_player "E"; Player.create_player "L"; Player.create_player "M"|]


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
        | _ -> match game_state.has_to_replay with
          | true -> play game_state
          | false -> play {game_state with current_index_player = (game_state.current_index_player + 1) mod (Array.length game_state.players); has_to_replay = false; timeline= Start}) 
      in

  (Board.display game_state.board game_state.players;

  (* Handle the turn *)
  let rec turn play game_state  =
  (Game.act game_state.players.(game_state.current_index_player) play game_state |> function outcome -> match outcome with

  (* Error handling *)
  | Error _error -> print_endline "Erreur";

  (* Next turn *)
  | Next game_state -> match game_state.timeline with
    | Start -> turn Roll game_state
    | HandleSquare square -> (match square with 

      | Square.Buyable square_buyable -> 
        
        (match Square.get_owner square_buyable with
          | None -> ( match (Game.ask_buy square_buyable) with
              | true -> turn (Buy square_buyable) game_state
              | false -> endturn game_state)

          | Some owner -> 
            print_endline ("Cette propriété appartient à " ^ Player.name_player owner);
            turn (PayOwner square_buyable) game_state)
      
      | Square.Cheating -> 
        print_endline "Vous avez triché ! Vous êtes envoyé en prison !";
        game_state.players.(game_state.current_index_player) <- Player.toogle_to_jail game_state.players.(game_state.current_index_player) true;
        turn (Goto 10) game_state;

      | _ -> endturn game_state)
    
    | HandleJail -> let rec ask_jail () = (print_endline "";
        print_endline "Voulez-vous payer pour sortir de prison ? (y/n) ";
        match read_line () with
          | "y" -> turn (PayJail) game_state
          | "n" -> endturn game_state
          | _ -> ask_jail ())
        in ask_jail ()
    | EndTurn -> endturn game_state) 
    
    
     in turn Roll game_state)
    

let () = 

(* Initialisation of the random generator *)
 Random.self_init (); 

(* Start the game *)
 play game_state