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

type square_buyable

type square =
House
| Email
| StLife
| Buyable of square_buyable
| Holiday
| Cheating
| HouseCheating
| Tax of tax

val create_cours : ufr -> int -> string -> square

val create_buyable : square_buyable_type -> square

val price_buyable : square_buyable_type -> int

val get_price : square -> int

val get_owner : square_buyable -> Player.player array -> Player.player option

val change_owner : square_buyable -> int option -> square

val name_square : square -> string

val get_type_square : square_buyable -> square_buyable_type

val create_tax : int -> string -> square

val create_restaurant : string -> square

val create_library : string -> square
