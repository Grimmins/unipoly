open Unipoly
open Game

let board = Board.init_board ()
let game_state = create_game board [|Player.create_player "A"; Player.create_player "E"; Player.create_player "L"; Player.create_player "M"|]


let () = 

(* Initialisation of the random generator *)
 Random.self_init (); 

(* Start the game *)
 Game.play game_state