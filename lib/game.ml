open Player
open Board
open Error

type play = 
  | Roll
  | Move of int

type game_state = 
  { board : board
  ; players : player array
  ; current_player : player
  }

  type outcome = 
| Next of game_state
  | Error of error
(*| | Endgame of player option*)

let roll_dices () = 
  let (d1, d2) = (Random.int 6 + 1,
  Random.int 6 + 1) in 
  print_endline ("Résultat des dés : " ^ string_of_int d1 ^ " , " ^ string_of_int d2);
  (d1, d2)

let rec act player play game_state = 
  match play with
  | Roll -> roll_dices () |> fun (n,m) -> act player (Move (n + m)) game_state  (* TODO : Rajouter condition prison *)
  | Move n -> change_pos player (pos_player player + n) |> fun player -> 
      (* change player into current_player and players *)
      match Array.find_index (fun p -> name_player p = name_player player) game_state.players with
        | Some index -> game_state.players.(index) <- player; Next {game_state with current_player = player}
        | None -> Error (InvalidPlayer)
      
  (* | Buy -> buy player game_state *)
  (* | End -> end_turn player game_state *)
  (* | Pay -> pay player game_state *)
  (* | Endgame -> endgame player game_state *)
  (* | Error -> Error (Error.create_error "Erreur") *)

let create_game board players = 
  { board
  ; players
  ; current_player = players.(0)
  }