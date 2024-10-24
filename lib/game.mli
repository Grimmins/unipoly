

type play = 
  | Roll
  | Move of int
  | Buy of Square.square_buyable
  | PayOwner of Square.square_buyable

type timeline = 
| Start
| EndTurn
| HandleSquare of Square.square

type game_state = {
  board : Board.board;
  players : Player.player array;
  current_index_player : int;
  has_to_replay : bool;
  timeline : timeline;
}
 
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