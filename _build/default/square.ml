
module Square = struct 

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

  type cost = {
    price : int;
    name : string;
  }

  type restaurant = {
    name : string;
  }

  type square =
    House 
  | Library of library 
  | Cours of cours
  | Holiday
  | Cheating
  | HouseCheating
  | Cost of cost
  | Restaurant of restaurant


end
