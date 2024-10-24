type game_state = {
  board : Board.board;
  players : Player.player array;
  current_index_player : int;
}

type play = 
  | Roll
  | Move of int
  | Buy of Square.square_buyable
  | PayOwner of Square.square_buyable
  
 
type outcome = 
  | HandleSquare of Square.square
  | Next of game_state
  | Error of Error.error

val act : Player.player -> play -> game_state -> outcome
val create_game : Board.board -> Player.player array -> game_state
val end_turn : game_state -> outcome
val ask_buy : Square.square_buyable -> bool