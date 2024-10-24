open Player
open Board
open Error
open Square
open Card
open Deck

type play =
  | Roll
  | Move of int
  | Buy of square_buyable
  | PayOwner of square_buyable

type game_state =
  { board : board;
    players : player array;
    current_index_player : int
  }

type outcome =
  | Next of game_state
  | Error of error

let roll_dices () =
  let (d1, d2) = (Random.int 6 + 1,
  Random.int 6 + 1) in
  print_endline "";
  print_endline ("Résultat des dés : " ^ string_of_int d1 ^ " , " ^ string_of_int d2);
  (d1, d2)


(* Handle the int option when finding the index of player *)
let handle_index_player player game_state f  =
  match find_index_player player game_state.players with
  | Some index -> f index
  | None -> Error (InvalidPlayer)

let end_turn game_state =
  Next {game_state with current_index_player = (game_state.current_index_player + 1) mod Array.length game_state.players}

let apply_card_effect_and_continue (card : Card.card) (player : player) (game_state : game_state) =
  let updated_player = apply_card_effect player card in
  handle_index_player updated_player game_state (fun index ->
    game_state.players.(index) <- updated_player;
    print_endline ("Vous avez pioché une carte : " ^ Card.get_description card);
    end_turn game_state)

(* demande d'achat d'une propriété *)
let ask_buy square_buyable =
  print_endline ("Voulez-vous acheter " ^ name_square (Buyable (square_buyable)) ^ " pour " ^ string_of_int (price_buyable square_buyable.type_square) ^ "€ ? (y/n)");
  let rec ask_buy () =
    match read_line () with
    | "y" -> true
    | "n" -> false
    | _ -> print_endline "Veuillez entrer y ou n"; ask_buy ()
  in ask_buy ()

let rec act player play game_state =
  (* Fonction locale pour gérer une case *)
  let handle_square player game_state =
    let current_square = game_state.board.(pos_player player) in
    match current_square with
    | Email ->
        let (card, _) = draw_card (init_email_deck ()) in
        apply_card_effect_and_continue card player game_state
    | StLife ->
        let (card, _) = draw_card (init_stlife_deck ()) in
        apply_card_effect_and_continue card player game_state
    | Buyable buyable_square ->
        if ask_buy buyable_square then
          act player (Buy buyable_square) game_state
        else
          end_turn game_state
    | _ -> end_turn game_state
  in

  (* Corps principal de act *)
  match play with
  | Roll -> roll_dices () |> fun (n, m) -> act player (Move (n + m)) game_state
  | Move n ->
      change_pos player ((pos_player player + n) mod 40) |> fun player ->
      handle_index_player player game_state (fun index ->
        game_state.players.(index) <- player;
        display game_state.board game_state.players;
        handle_square player game_state
      )
  | Buy square_buyable ->
      if price_buyable square_buyable.type_square > money_player player then Error (NotEnoughMoney)
      else
        change_money player (- price_buyable square_buyable.type_square) |> fun player ->
        change_owner square_buyable (Some player) |> fun square ->

            game_state.players.(game_state.current_index_player) <- player;
            game_state.board.(pos_player player) <- square;
            print_endline (name_player player ^ " a acheté " ^ string_of_int (price_buyable square_buyable.type_square) ^ "€ " ^ name_square square);
            end_turn game_state
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
        end_turn game_state
    | None -> Error (NoOwner))


(* Création du jeu *)
let create_game board players =
  { board
  ; players
  ; current_index_player = 0
  }
