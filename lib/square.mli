type ufr = Math | Info | Physique | SVT | Economie | Lettres | Langues | Hggsp
type degre_type = Licence | Master | Doctorat
type degre = degre_type option

type cours

type library

type tax

type restaurant

type square_buyable_type =
| Cours of cours
| Library of library
| Restaurant of restaurant

type square_buyable = {
    type_square : square_buyable_type;
    proprietaire_index : int option;
    }

type square =
House
| Email
| StLife
| Buyable of square_buyable
| Holiday
| Cheating
| HouseCheating
| Tax of tax

(** [create_cours] creates a course.
    @param ufr the ufr of the course
    @param degre the degree of the course
    @param name the name of the course
    @param landing_price the landing price of the course
    @param licence_price the price of the licence
    @param master_price the price of the master
    @param doctorat_price the price of the doctorat
    @param upgrade_price the price of the upgrade
    @return course
*)
val create_cours : ufr -> int -> string -> int -> int -> int -> int -> int -> square

(** [create_buyable] creates a buyable square.
    @param type_square the type of the square
    @return square
*)
val create_buyable : square_buyable_type -> square

(** [price_buyable] returns the price of the buyable square.
    @param square the buyable square
    @return price
*)
val price_buyable : square_buyable_type -> int

(** [get_owner] returns the owner of the buyable square.
    @param square the buyable square
    @param players the players of the game
    @return owner
*)
val get_owner : square_buyable -> Player.player array -> Player.player option

(** [change_owner] changes the owner of the buyable square.
    @param square the buyable square
    @param owner the new owner
    @return square
*)
val change_owner : square_buyable -> int option -> square

(** [name_square] returns the name of the square.
    @param square the square
    @return name
*)
val name_square : square -> string

(** [get_tax_amount] returns the amount of the tax.
    @param square the tax square
    @return amount
*)
val get_tax_amount : square -> int

(** [get_square_buyable] returns the buyable square.
    @param square the square
    @return buyable square
*)
val get_square_buyable : square -> square_buyable option

(** [get_cours] returns the course.
    @param square the buyable square
    @return course
*)
val get_cours : square_buyable -> cours option

(** [get_type_square] returns the type of the buyable square.
    @param square the buyable square
    @return type
*)
val get_type_square : square_buyable -> square_buyable_type

(** [get_name_restaurant] returns the name of the restaurant.
    @param restaurant the restaurant
    @return name
*)
val get_name_restaurant : restaurant -> string

(** [create_tax] creates a tax square.
    @param amount the amount of the tax
    @param name the name of the tax
    @return square
*)
val create_tax : int -> string -> square

(** [create_restaurant] creates a restaurant square.
    @param name the name of the restaurant
    @return square
*)
val create_restaurant : string -> square

(** [create_library] creates a library square.
    @param name the name of the library
    @return square
*)
val create_library : string -> square

(** [get_ufr] returns the ufr of the course.
    @param cours the course
    @return ufr
*)
val get_ufr : cours -> ufr

(** [get_degre] returns the degree of the course.
    @param cours the course
    @return degree
*)
val get_degre : cours -> degre

(** [get_name_cours] returns the name of the course.
    @param cours the course
    @return name
*)
val get_name_cours : cours -> string

(** [get_landing_price] returns the landing price of the course.
    @param cours the course
    @return landing price
*)
val get_landing_price : cours -> int

(** [get_licence_price] returns the price of the licence.
    @param cours the course
    @return price
*)
val get_licence_price : cours -> int

(** [get_master_price] returns the price of the master.
    @param cours the course
    @return price
*)
val get_master_price : cours -> int

(** [get_doctorat_price] returns the price of the doctorat.
    @param cours the course
    @return price
*)
val get_doctorat_price : cours -> int

(** [get_upgrade_price] returns the price of the upgrade.
    @param cours the course
    @return price
*)
val get_upgrade_price : cours -> int

(** [update_degre] updates the degree of the square.
    @param square the square
    @return square
*)
val update_degre : square_buyable -> square_buyable

(** [get_cours_from_square] returns the course from the square.
    @param square the square
    @return course
*)
val get_cours_from_square : square -> cours

(** [get_index_from_square] returns the index of the square in the array.
    @param square the square
    @param board the board
    @return index
*)
val get_index_from_square : square -> square array -> int option

(** [get_index_from_square_buyable] returns the index of the square in the array.
    @param square the square
    @param board the board
    @return index
*)
val get_index_from_square_buyable : square_buyable -> square array -> int option

(** [get_properties_owned_by_player] returns the properties owned by the player.
    @param player the player
    @param board the board
    @return list of properties
*)
val get_degre_square : square -> degre option