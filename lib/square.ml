open Player

  type ufr = Math | Info | Physique | SVT | Economie | Lettres | Langues | Hggsp
  type degre = None | Licence | Master | Doctorat

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
    proprietaire : player option;
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

  let create_cours ufr price name = Buyable {type_square = Cours { ufr; price; degre = None; name }; proprietaire = None}

  let create_buyable = function 
    | Library l -> Buyable {type_square = Library l; proprietaire = None}
    | Restaurant r -> Buyable {type_square = Restaurant r; proprietaire = None}
    | Cours c -> Buyable {type_square = Cours c; proprietaire = None}

  let price_buyable = function
  | Cours c -> c.price
  | Library _ -> 200
  | Restaurant _ -> 150

  let get_price = function
    | Tax c -> c.price
    | Buyable b -> 
      price_buyable b.type_square
    | _ -> 0

  let change_owner square_buyable player =
    match square_buyable.type_square with
    | Library l -> Buyable {type_square = Library l; proprietaire = player}
    | Restaurant r -> Buyable {type_square = Restaurant r; proprietaire = player}
    | Cours c -> Buyable {type_square = Cours c; proprietaire = player}

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



