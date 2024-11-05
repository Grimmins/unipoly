build:
	dune build

run:
	dune exec bin/main.exe

# Dépendances de Bisect_ppx pour la couverture de code
bisect_deps = --instrument-with bisect_ppx

# Règle pour exécuter les tests avec couverture de code et rapport
tests:
	@echo "\033[1;33mNettoyage des anciens fichiers de couverture...\033[0m"
	@find . -name '*.coverage' | xargs rm -f
	@echo "\033[1;34mExécution des tests avec couverture...\033[0m"
	@dune runtest $(bisect_deps) --force

	@if [ $$? -eq 0 ]; then \
		echo "\033[0;32mLes tests ont réussi !\033[0m"; \
	else \
		echo "\033[0;31mCertains tests ont échoué. Veuillez vérifier.\033[0m"; \
		exit 1; \
	fi

	@echo "\033[1;33mQuel type de rapport souhaitez-vous ?\033[0m"
	@echo "1) Rapport HTML détaillé"
	@echo "2) Résumé rapide dans le terminal"
	@echo "3) Les deux"
	@echo "Entrez votre choix (1, 2 ou 3) : "
	@read choix; \
	case $$choix in \
		1) \
			echo "\033[1;34mGénération du rapport HTML...\033[0m"; \
			bisect-ppx-report html; \
			echo "\033[0;32mRapport HTML généré dans le dossier _coverage.\033[0m"; \
			;; \
		2) \
			echo "\033[1;34mGénération d'un résumé rapide...\033[0m"; \
			bisect-ppx-report summary; \
			;; \
		3) \
			echo "\033[1;34mGénération du rapport HTML et du résumé...\033[0m"; \
			bisect-ppx-report html; \
			bisect-ppx-report summary; \
			echo "\033[0;32mRapport HTML généré dans le dossier _coverage.\033[0m"; \
			;; \
		*) \
			echo "\033[0;31mChoix non valide. Aucun rapport généré.\033[0m"; \
			exit 1; \
			;; \
	esac

	@echo "\033[0;32mTests terminés avec succès !\033[0m"
