val count_restaurants_owned : int -> Square.square array -> int

val count_librairies_owned : int -> Square.square array -> int

val count_cours_owned : int -> Square.square array -> int

val owns_all_courses_in_ufr : Square.ufr -> int -> Square.square array -> bool

val get_courses_owned_by_player : int -> Square.square array -> Square.square list

val get_properties_owned_by_player : int -> Square.square array -> Square.square_buyable list

val get_landing_price : Square.cours -> int

val get_adjusted_course_landing_price : Square.cours -> int -> Square.square array -> int

val get_next_degree_price : Square.cours -> int

val remove_all_properties : int -> Square.square array -> unit


