  type ufr = Math | Info | Physique | SVT | Economie | Lettres | Langues | Hggsp
  type degre_type = Licence | Master | Doctorat
  type degre = degre_type option

  type cours = {
  ufr : ufr;
  price : int;
  landing_price: int;
  licence_price: int;
  master_price: int;
  doctorat_price: int;
  upgrade_price: int;
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

let next_degre = function
  | None -> Some Licence
  | Some Licence -> Some Master
  | Some Master -> Some Doctorat
  | Some Doctorat -> Some Doctorat

let update_degre square_buyable =
  match square_buyable.type_square with
  | Cours cours ->
      let new_degre = next_degre cours.degre in
      let updated_cours = { cours with degre = new_degre } in
      { square_buyable with type_square = Cours updated_cours }
  | _ -> square_buyable


  type square =
   House
  | Email
  | StLife
  | Buyable of square_buyable
  | Holiday
  | Cheating
  | HouseCheating
  | Tax of tax

  let create_cours ufr price name landing_price licence_price master_price doctorat_price upgrade_price = Buyable {type_square = Cours { ufr; price; landing_price; licence_price; master_price; doctorat_price; upgrade_price; degre = None; name }; proprietaire_index = None}

  let create_buyable = function
    | Library l -> Buyable {type_square = Library l; proprietaire_index = None}
    | Restaurant r -> Buyable {type_square = Restaurant r; proprietaire_index = None}
    | Cours c -> Buyable {type_square = Cours c; proprietaire_index = None}

  let price_buyable = function
  | Cours c -> c.price
  | Library _ -> 200
  | Restaurant _ -> 150

  let get_tax_amount = function
    | Tax t -> t.price
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

let get_ufr (c : cours) = c.ufr

let get_degre (c : cours) = c.degre

let get_degre_square = function
| Buyable {type_square = Cours c; _} -> Some c.degre
| _ -> None

let get_name_cours (c : cours) = c.name

let get_landing_price (c : cours) = c.landing_price

let get_licence_price (c : cours) = c.licence_price

let get_master_price (c : cours) = c.master_price

let get_doctorat_price (c : cours) = c.doctorat_price

let get_upgrade_price (c : cours) = c.upgrade_price

let get_square_buyable = function
  | Buyable b -> Some b
  | _ -> None

let get_cours = function
  | {type_square = Cours c; _} -> Some c
  | _ -> None

let get_index_from_square (square : square) (board : square array) = 
  let rec aux i = function
    | [] -> None
    | h::t -> if h == square then Some i else aux (i+1) t
  in aux 0 (Array.to_list board)

let get_index_from_square_buyable (square_buyable : square_buyable) (board : square array) = 
  let rec aux i = function
    | [] -> None
    | h::t -> match h with
      | Buyable b when b == square_buyable -> Some i
      | _ -> aux (i+1) t
  in aux 0 (Array.to_list board)

let get_cours_from_square square = (Option.get (get_cours (Option.get (get_square_buyable square)))) 