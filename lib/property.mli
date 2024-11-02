val count_properties_owned :
  (Square.square_buyable_type -> bool) -> int -> Square.square array -> int

val count_restaurants_owned : int -> Board.board -> int

val count_librairies_owned : int -> Board.board -> int

val count_cours_owned : int -> Board.board -> int

val is_restaurant : Square.square_buyable_type -> bool

val is_library : Square.square_buyable_type -> bool

val is_cours : Square.square_buyable_type -> bool