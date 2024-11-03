val count_restaurants_owned : int -> Board.board -> int

val count_librairies_owned : int -> Board.board -> int

val count_cours_owned : int -> Board.board -> int

val owns_all_courses_in_ufr : Square.ufr -> int -> Board.board -> bool

val get_courses_owned_by_player : int -> Board.board -> Square.square list

val get_properties_owned_by_player : int -> Board.board -> Square.square_buyable list

val get_landing_price : Square.cours -> int

val get_adjusted_course_landing_price : Square.cours -> int -> Board.board -> int

val get_next_degree_price : Square.cours -> int

