

module Board = struct
  open Square

  let board : square list = [
    House;
    Library {name = "Bibliotheque"};
    Cours {ufr = Math; price = 100; degre = Licence; name = "Mathematiques"};
    Cours {ufr = Info; price = 100; degre = Licence; name = "Informatique"};
    Cours {ufr = Physique; price = 100; degre = Licence; name = "Physique"};
    Cours {ufr = SVT; price = 100; degre = Licence; name = "SVT"};
    Cours {ufr = Economie; price = 100; degre = Licence; name = "Economie"};
    Cours {ufr = Lettres; price = 100; degre = Licence; name = "Lettres"};
    Cours {ufr = Langues; price = 100; degre = Licence; name = "Langues"};
    Cours {ufr = Hggsp; price = 100; degre = Licence; name = "HGGSP"};
    Holiday;
    Cheating;
    HouseCheating;
    Cost {price = 50; name = "Cantine"};
    Restaurant {name = "Restaurant"};
  ]

end