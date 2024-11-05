(** [count_restaurants_owned] returns the number of restaurants owned by the player.
    @param player the player
    @param board the board
    @return number of restaurants owned
*)
val count_restaurants_owned : int -> Square.square array -> int

(** [count_librairies_owned] returns the number of librairies owned by the player.
    @param player the player
    @param board the board
    @return number of librairies owned
*)
val count_librairies_owned : int -> Square.square array -> int

(** [count_cours_owned] returns the number of courses owned by the player.
    @param player the player
    @param board the board
    @return number of courses owned
*)
val count_cours_owned : int -> Square.square array -> int

(** [owns_all_courses_in_ufr] returns true if the player owns all the courses in the ufr, false otherwise.
    @param ufr the ufr
    @param player the player
    @param board the board
    @return bool
*)
val owns_all_courses_in_ufr : Square.ufr -> int -> Square.square array -> bool

(** [get_courses_owned_by_player] returns the courses owned by the player.
    @param player the player
    @param board the board
    @return list of courses
*)
val get_courses_owned_by_player : int -> Square.square array -> Square.square list

(** [get_properties_owned_by_player] returns the properties owned by the player.
    @param player the player
    @param board the board
    @return list of properties
*)
val get_properties_owned_by_player : int -> Square.square array -> Square.square_buyable list

(** [get_landing_price] returns the landing price of the course.
    @param cours the course
    @return landing price
*)
val get_landing_price : Square.cours -> int

(** [get_adjusted_course_landing_price] returns the adjusted landing price of the course.
    @param cours the course
    @param n the number of courses owned
    @param board the board
    @return adjusted landing price
*)
val get_adjusted_course_landing_price : Square.cours -> int -> Square.square array -> int

(** [get_price] returns the price of the property.
    @param board the board
    @param index the index of the property
    @return price
*)
val get_price : Square.square array -> int -> Square.square -> int

(** [get_next_degree_price] returns the price of the next degree.
    @param cours the course
    @return price
*)
val get_next_degree_price : Square.cours -> int

(** [remove_all_properties] removes all the properties owned by the player.
    @param player the player
    @param board the board
*)
val remove_all_properties : int -> Square.square array -> unit

(** [is_restaurant] returns true if the square is a restaurant, false otherwise.
    @param square the square
    @return is restaurant
*)
val is_restaurant : Square.square_buyable_type -> bool

(** [is_library] returns true if the square is a library, false otherwise.
    @param square the square
    @return is library
*)
val is_library : Square.square_buyable_type -> bool

(** [is_cours] returns true if the square is a course, false otherwise.
    @param square the square
    @return is course
*)
val is_cours : Square.square_buyable_type -> bool
