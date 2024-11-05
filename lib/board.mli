type board

(* fonction d'affichage *)
val display :  board -> Player.player array -> int -> unit

(* fonction de création *)
val init_board : unit-> board

val index_square : Square.square -> Square.square array -> int option

val get_square : int -> board -> Square.square

val change_square : int -> Square.square -> board -> unit

val get_adjusted_course_landing_price_board : Square.cours -> int -> board -> int

val count_restaurants_owned_board : int -> board -> int

val count_librairies_owned_board : int -> board -> int

val get_index_from_square_buyable_board : Square.square_buyable -> board -> int option

val owns_all_courses_in_ufr_board : Square.ufr -> int -> board-> bool

val get_index_from_square_board : Square.square -> board -> int option

val get_properties_owned_by_player_board : int -> board -> Square.square_buyable list

val get_courses_owned_by_player_board : int -> board -> Square.square list

val remove_all_properties_board : int -> board -> unit