open Unipoly 

let board = Board.init_board ()
let game_state = Game.create_game board [|Player.create_player "Joueur 1"; Player.create_player "Joueur 2"|]

(* Main game loop *)
let rec play board = 
  Board.display board;
  Game.act game_state.current_player Roll game_state |> function outcome -> match outcome with
  | Next game_state ->  
    print_endline "";
    print_endline "Appuyez sur Entrée pour terminer votre tour, ou tapez 'end' pour terminer le jeu : ";
     match read_line () with
      | "end" -> exit 0
      | _ -> play game_state.board

let () = play board