open Player
open Board
open Error
open Square

type play = 
  | Roll
  | Move of int
  | Buy of square_buyable

type game_state = 
  { board : board;
   players : player array;
   current_index_player : int
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
  
(* Handle the int option when finding the index of player *)
let handle_index_player player game_state f  = 
  match find_index_player player game_state.players with
  | Some index -> f index
  | None -> Error (InvalidPlayer)

let end_turn game_state = 
    Next {game_state with current_index_player = (game_state.current_index_player + 1) mod Array.length game_state.players}

let rec act player play game_state = 
  match play with

  (* lancer de dés *)
  | Roll -> roll_dices () |> fun (n,m) -> act player (Move (n + m)) game_state  (* TODO : Rajouter condition prison *)

  (* déplacement du joueur *)
  | Move n -> change_pos player ((pos_player player + n) mod 40) |> fun player ->
      (* change player into current_player and players *)
      handle_index_player player game_state (fun index ->

        game_state.players.(index) <- player; 
        (let handle_square_result player game_state (square : square) =
          match square with
          | Buyable square_buyable -> act player (Buy square_buyable) game_state
          | _ -> end_turn game_state
           in handle_square_result player game_state game_state.board.(pos_player player))
        )

  (* achat d'une propriété *)
   | Buy square_buyable -> 
      if price_buyable square_buyable.type_square > money_player player then Error (NotEnoughMoney)
      else 
        change_money player (- price_buyable square_buyable.type_square) |> fun player -> 
        change_owner square_buyable (Some player) |> fun square -> 
              
            game_state.players.(game_state.current_index_player) <- player;
            game_state.board.(pos_player player) <- square;
            (* TODO : pas dans la même fonction*)
            print_endline (name_player player ^ " a acheté " ^ string_of_int (price_buyable square_buyable.type_square) ^ "€ " ^ name_square square);
            end_turn game_state
      

  (* | End -> end_turn player game_state *)
  (* | Pay -> pay player game_state *)
  (* | Endgame -> endgame player game_state *)
  (* | Error -> Error (Error.create_error "Erreur") *)


let create_game board players = 
  { board
  ; players
  ; current_index_player = 0
  }