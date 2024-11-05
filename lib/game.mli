type game_state

type play = 
  | Roll
  | Move of int
  | Buy of Square.square_buyable
  | PayJail
  | PlayCard of Card.card
  | BuyDiploma of Square.square list
  | Change of Square.square_buyable * Square.square_buyable

type timeline =
  | Start
  | EndTurn
  | HandleSquare of Square.square
  | HandleJail
  | AskDiploma
  | AskChange

type outcome =
  | Next of game_state
  | Error of Error.error
  | Endgame of Player.player

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

(** [play] is the main function of the game. It displays the board, handles the turn and the end of the turn.
    @param game_state the current state of the game
    @return unit
*)
val play : game_state -> unit

(** [get_current_player game_state] returns the current player.
    @param game_state the current state of the game
    @return player
*)
val get_current_player : game_state -> Player.player

(** [get_timeline game_state] returns the timeline of the game.
    @param game_state the current state of the game
    @return timeline
*)
val get_timeline : game_state -> timeline