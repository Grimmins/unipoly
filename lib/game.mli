type game_state

type play = 
  | Roll
  | Move of int
  | Buy of Square.square_buyable
  | PayJail
  | PlayCard of Card.card

type timeline =
  | Start
  | EndTurn
  | HandleSquare of Square.square
  | HandleJail

type outcome =
  | Next of game_state
  | Error of Error.error

(** [act] handles the player's action.
    @param player the current player
    @param play the action to do
    @param game_state the current state of the game
    @return outcome
*)
val act : Player.player -> play -> game_state -> outcome

(** [create_game] creates the game state.
    @param board the board of the game
    @param players the players of the game
    @return game_state
*)
val create_game : Board.board -> Player.player array -> game_state

(** [ask_buy] asks the player if he wants to buy a square.
    @param square_buyable the square to buy
    @return bool
*)
val ask_buy : Square.square_buyable -> bool

(** [get_current_player] gets the current player.
    @param game_state the current state of the game
    @return Player.player
*)
val get_current_player : game_state -> Player.player


(** [update_current_player] updates the current player.
    @param game_state the current state of the game
    @param player the new player
*)
val update_current_player : game_state -> Player.player -> unit

(** [has_to_replay] checks if the player has to replay.
    @param game_state the current state of the game
    @return bool
*)
val has_to_replay : game_state -> bool

(** [end_turn] ends the turn of the player.
    @param game_state the current state of the game
    @return game_state
*)
val end_turn : game_state -> game_state

(** [get_timeline] gets the timeline of the game.
    @param game_state the current state of the game
    @return timeline
*)
val get_timeline : game_state -> timeline

(** [play] is the main function of the game. It displays the board, handles the turn and the end of the turn.
    @param game_state the current state of the game
    @return unit
*)
val play : game_state -> unit