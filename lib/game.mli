type game_state

type play = 
  | Roll
  | Move of int
  | Buy of Square.square_buyable
  | PayJail

type timeline = 
  | Start
  | EndTurn
  | HandleSquare of Square.square
  | HandleJail


 
type outcome =
  | Next of game_state
  | Error of Error.error

(** [act player play game_state] is the main function of the game. It handles the player's turn.
    @param player the current player
    @param play the action to do
    @param game_state the current state of the game
    @return outcome
*)
val act : Player.player -> play -> game_state -> outcome

(** [create_game board players] is the main function of the game. It creates the game state.
    @param board the board of the game
    @param players the players of the game
    @return game_state
*)
val create_game : Board.board -> Player.player array -> game_state

(** [ask_buy square_buyable] is the main function of the game. It asks the player if he wants to buy a square.
    @param square_buyable the square to buy
    @return bool
*)
val ask_buy : Square.square_buyable -> bool

val get_current_player : game_state -> Player.player

val update_current_player : game_state -> Player.player -> unit

val has_to_replay : game_state -> bool

val end_turn : game_state -> game_state

val get_timeline : game_state -> timeline

val play : game_state -> unit