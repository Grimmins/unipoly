open Unipoly
open Game

let board = Board.init_board ()
let game_state = create_game board [|Player.create_player "A"; Player.create_player "E"; Player.create_player "L"; Player.create_player "M"|]

(* Main game loop *)
let rec play (game_state : game_state) =
  Board.display game_state.board game_state.players;
  Game.act game_state.players.(game_state.current_index_player) Roll game_state |> function outcome -> match outcome with
  | Error _error -> print_endline "Erreur";
  | Next game_state ->  
    print_endline "";
    print_endline "Appuyez sur Entrée pour terminer votre tour, ou tapez 'end' pour terminer le jeu : ";
     match read_line () with
      | "end" -> exit 0
      | _ -> play game_state
  

let () = 
 Random.self_init (); (* Initialisation of the random generator *)
 play game_state