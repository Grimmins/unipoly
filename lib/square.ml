  type ufr = Math | Info | Physique | SVT | Economie | Lettres | Langues | Hggsp
  type degre_type = Licence | Master | Doctorat
  type degre = degre_type option

  type cours = {
  ufr : ufr;
  price : int;
  degre : degre;
  name : string;
  }

  type library = {
  name : string;
  }

  type tax = {
    price : int;
    name : string;
  }

  type restaurant = {
    name : string;
  }

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

  let create_cours ufr price name = Buyable {type_square = Cours { ufr; price; degre = None; name }; proprietaire_index = None}

  let create_buyable = function 
    | Library l -> Buyable {type_square = Library l; proprietaire_index = None}
    | Restaurant r -> Buyable {type_square = Restaurant r; proprietaire_index = None}
    | Cours c -> Buyable {type_square = Cours c; proprietaire_index = None}

  let price_buyable = function
  | Cours c -> c.price
  | Library _ -> 200
  | Restaurant _ -> 150

  let get_price = function
    | Tax c -> c.price
    | Buyable b -> 
      price_buyable b.type_square
    | _ -> 0

  let change_owner square_buyable player_index =
    match square_buyable.type_square with
    | Library l -> Buyable {type_square = Library l; proprietaire_index = player_index}
    | Restaurant r -> Buyable {type_square = Restaurant r; proprietaire_index = player_index}
    | Cours c -> Buyable {type_square = Cours c; proprietaire_index = player_index}

  let get_owner square_buyable players =
    square_buyable.proprietaire_index |> fun index ->
      if index = None then None
    else Some players.(Option.get index)

  let name_square = function
  | House -> "Maison"
  | Email -> "Email"
  | StLife -> "StLife"
  | Buyable b -> 
    (match b.type_square with
    | Cours c -> c.name
    | Library l -> l.name
    | Restaurant r -> r.name)
  | Holiday -> "Holiday"
  | Cheating -> "Cheating"
  | HouseCheating -> "HouseCheating"
  | Tax t -> t.name

let get_type_square square_buyable =
  square_buyable.type_square

let create_tax price name = Tax {price; name}

let create_library name = create_buyable (Library {name})

let create_restaurant name = create_buyable (Restaurant {name})

let get_name_restaurant (r : restaurant) = r.name

let _get_ufr c = c.ufr

let _get_degre c = c.degre

let _get_name c = c.name