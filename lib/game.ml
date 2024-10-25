open Player
open Board
open Error
open Square

type play = 
  | Roll
  | Move of int
  | Buy of square_buyable
  | PayOwner of square_buyable
  | Goto of int
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



let rec act player play game_state = 
  match play with

  (* lancer de dés *)
  | Roll -> roll_dices () |> fun (n,m) -> 
      (* Le joueur est en prison *)
      ( if Player.is_in_jail player then

          (if n = m then (print_endline "Vous sortez de prison"; act player Roll game_state)
          else Next {game_state with timeline = HandleJail})
      
      (* Le joueur n'est pas en prison *)
      else act player (Move (n + m)) {game_state with has_to_replay = (n = m)};
      )

  (* déplacement du joueur *)
  | Move n -> change_pos player ((pos_player player + n) mod 40) |> fun player ->
      (* change player into current_player and players *)
      handle_index_player player game_state (fun index ->

        game_state.players.(index) <- player; 
        display game_state.board game_state.players;

        Next {game_state with timeline = HandleSquare game_state.board.(pos_player player)})

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
            Next {game_state with timeline = EndTurn}
      
  (* paiement au propriétaire *)
  | PayOwner square_buyable -> 
    (match get_owner square_buyable with
    | Some owner -> 
      if money_player player < price_buyable square_buyable.type_square then Error (NotEnoughMoney)
      else 
        change_money player (- (price_buyable square_buyable.type_square)) |> fun player -> 
        change_money owner (price_buyable square_buyable.type_square) |> fun owner -> 
        game_state.players.(game_state.current_index_player) <- player;
        game_state.players.(find_index_player owner game_state.players |> Option.get) <- owner;
        print_endline (name_player player ^ " a payé " ^ name_player owner ^ " " ^ string_of_int (price_buyable square_buyable.type_square) ^ "€");
        Next {game_state with timeline = EndTurn}
    | None -> Error (NoOwner))

  | Goto i -> (
    (* TODO : Gérer passage case départ *)
    game_state.players.(game_state.current_index_player) <- change_pos player i;
    Next {game_state with timeline = HandleSquare game_state.board.(i)}
  )

  | PayJail -> (
    (* TODO : Change 500 with constant *)
    if money_player player < 500 then Error (NotEnoughMoney)
    else (change_money player (- 500) |> fun player -> 
      game_state.players.(game_state.current_index_player) <- toogle_to_jail player false;
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
  (print_endline ("Voulez-vous acheter " ^ name_square (Buyable (square_buyable)) ^ " pour " ^ string_of_int (price_buyable square_buyable.type_square) ^ "€ ? (y/n)");
  let rec ask_buy () = 
    match read_line () with
    | "y" -> true
    | "n" -> false
    | _ -> print_endline "Veuillez entrer y ou n"; ask_buy () in ask_buy ())