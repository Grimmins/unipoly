type board

(** [display] displays the board.
    @param board the board to display
    @param players the players of the game
    @param current_player the current player
    @return unit
*)
val display :  board -> Player.player array -> int -> unit

(** [init_board] initializes the board.
    @return board
*)
val init_board : unit-> board

(** [index_square] returns the index of the square in the board.
    @param square the square
    @param board the board
    @return index
*)
val index_square : Square.square -> Square.square array -> int option

(** [get_square] returns the square at the given index.
    @param index the index of the square
    @param board the board
    @return square
*)
val get_square : int -> board -> Square.square

(** [change_square] changes the square at the given index.
    @param index the index of the square
    @param square the new square
    @param board the board
    @return unit
*)
val change_square : int -> Square.square -> board -> unit

(* functions to use other functions with the board *)

val get_adjusted_course_landing_price_board : Square.cours -> int -> board -> int

val count_restaurants_owned_board : int -> board -> int

val count_librairies_owned_board : int -> board -> int

val get_index_from_square_buyable_board : Square.square_buyable -> board -> int option

val owns_all_courses_in_ufr_board : Square.ufr -> int -> board-> bool

val get_index_from_square_board : Square.square -> board -> int option

val get_properties_owned_by_player_board : int -> board -> Square.square_buyable list

val get_courses_owned_by_player_board : int -> board -> Square.square list

val remove_all_properties_board : int -> board -> unit