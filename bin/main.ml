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
    print_endline "";
      print_endline "Appuyez sur Entrée pour terminer votre tour, ou tapez 'end' pour terminer le jeu : ";
      match read_line () with
        | "end" -> exit 0
        | _ -> play game_state in

  (Board.display game_state.board game_state.players;

  (* Handle the turn *)
  let rec turn play  =
  (Game.act game_state.players.(game_state.current_index_player) play game_state |> function outcome -> match outcome with

  (* Error handling *)
  | Error _error -> print_endline "Erreur";

  (* Handle the square *)
  | HandleSquare square -> (match square with 

    | Square.Buyable square_buyable -> 
      
      (match Square.get_owner square_buyable with
        | None -> ( match (Game.ask_buy square_buyable) with
            | true -> turn (Buy square_buyable)
            | false -> endturn game_state)

        | Some owner -> 
          print_endline ("Cette propriété appartient à " ^ Player.name_player owner);
          turn (PayOwner square_buyable))

    | _ -> endturn game_state)

  (* Next turn *)
  | Next game_state ->  endturn game_state) in turn Roll)
    

let () = 

(* Initialisation of the random generator *)
 Random.self_init (); 

(* Start the game *)
 play game_state