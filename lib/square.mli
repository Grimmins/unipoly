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

val create_cours : ufr -> int -> string -> int -> int -> int -> int -> int -> square

val create_buyable : square_buyable_type -> square

val price_buyable : square_buyable_type -> int

val get_price : square -> int

val get_owner : square_buyable -> Player.player array -> Player.player option

val change_owner : square_buyable -> int option -> square

val name_square : square -> string

val get_tax_amount : square -> int

val get_type_square : square_buyable -> square_buyable_type

val get_name_restaurant : restaurant -> string

val create_tax : int -> string -> square

val create_restaurant : string -> square

val create_library : string -> square

val get_ufr : cours -> ufr

val get_degre : cours -> degre

val get_name_cours : cours -> string

val get_landing_price : cours -> int

val get_licence_price : cours -> int

val get_master_price : cours -> int

val get_doctorat_price : cours -> int

val get_upgrade_price : cours -> int


