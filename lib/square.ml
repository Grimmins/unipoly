
  type ufr = Math | Info | Physique | SVT | Economie | Lettres | Langues | Hggsp
  type degre = None | Licence | Master | Doctorat

  type cours = {
  ufr : ufr;
  price : int;
  (* proprietaire : Joueur.joueur; *)
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

  type square_type =
   House
  | Email
  | StLife
  | Library of library 
  | Cours of cours
  | Holiday
  | Cheating
  | HouseCheating
  | Tax of tax
  | Restaurant of restaurant

type square = {
    square_type: square_type;
    players: Player.player list;
}

  let create_square square_type = { square_type ; players = [] }

  let create_cours ufr price name = { square_type = Cours { ufr; price; degre = None; name }; players = [] }

  let get_price = function
    | House -> 100
    | Library _ -> 200
    | Cours c -> c.price
    | Holiday -> 50
    | Cheating -> 100
    | HouseCheating -> 200
    | Tax c -> c.price
    | Restaurant _ -> 150
    | _ -> 0

