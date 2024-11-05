# Monopoly Université

Ce projet est une version du jeu Monopoly adaptée pour les étudiants universitaires. Il est développé en OCaml avec l'utilisation de Dune comme système de construction.

## Installation

1. Assurez-vous d'avoir OCaml et Dune installés sur votre système.
2. Pour réaliser les tests au point 7. téléchargez les librairies alcotest et bisect_ppx avec `opam install alcotest bisect_ppx`
3. Clonez ce dépôt sur votre machine.
4. Naviguez jusqu'au répertoire du projet.
5. Exécutez la commande `make build` pour construire le projet.
6. Exécutez la commande `make run` pour lancer le jeu.
7. Exécutez la commande `make tests` pour réaliser les tests et obtenir les rapports de couverture.

## Fonctionnalités

- Plateau de jeu représentant un campus universitaire.
- Cartes Chance et Caisse de Communauté adaptées à la vie étudiante.
- Possibilité d'acheter des cours (équivalent des rues).
- Mécanismes de jeu fidèles à la version classique du Monopoly.