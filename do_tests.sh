#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[1;34m'
NC='\033[0m'

# clear files existants
echo -e "${YELLOW}Nettoyage des anciens fichiers de couverture...${NC}"
find . -name '*.coverage' | xargs rm -f

# execute tests avec bisect
echo -e "${BLUE}Exécution des tests avec couverture...${NC}"
dune runtest --instrument-with bisect_ppx --force

# check test :OK:
if [ $? -eq 0 ]; then
  echo -e "${GREEN}Les tests ont réussi !${NC}"
else
  echo -e "${RED}Certains tests ont échoué. Veuillez vérifier.${NC}"
  exit 1
fi

# ask quel rapport
echo -e "${YELLOW}Quel type de rapport souhaitez-vous ?${NC}"
echo "1) Rapport HTML détaillé"
echo "2) Résumé rapide dans le terminal"
echo "3) Les deux"
echo -n "Entrez votre choix (1, 2 ou 3) : "
read -r choix

# generate rapport
case $choix in
  1)
    echo -e "${BLUE}Génération du rapport HTML...${NC}"
    bisect-ppx-report html
    echo -e "${GREEN}Rapport HTML généré dans le dossier _coverage.${NC}"
    ;;
  2)
    echo -e "${BLUE}Génération d'un résumé rapide...${NC}"
    bisect-ppx-report summary
    ;;
  3)
    echo -e "${BLUE}Génération du rapport HTML et du résumé...${NC}"
    bisect-ppx-report html
    bisect-ppx-report summary
    echo -e "${GREEN}Rapport HTML généré dans le dossier _coverage.${NC}"
    ;;
  *)
    echo -e "${RED}Choix non valide. Aucun rapport généré.${NC}"
    exit 1
    ;;
esac

echo -e "${GREEN}Tests terminés avec succès !${NC}"
