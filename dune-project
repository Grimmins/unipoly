(lang dune 3.16)

(name unipoly)

(generate_opam_files true)

(source (uri git+https://moule.informatique.univ-paris-diderot.fr/koltes/unipoly))
(documentation https://moule.informatique.univ-paris-diderot.fr/koltes/unipoly)
(homepage https://moule.informatique.univ-paris-diderot.fr/koltes/unipoly)
(bug_reports https://moule.informatique.univ-paris-diderot.fr/koltes/unipoly/-/issues)

(authors "Eva Herson" "Lùkas Koltès")

(maintainers "Eva Herson" "Lùkas Koltès")

(license LICENSE)

(package
 (name unipoly)
 (synopsis "Monopoly dans le terminal")
 (description "Implémentation d'un Monopoly version universitaire dans le terminal")
 (depends
    ocaml
    dune
    (alcotest :with-test)
    (bisect_ppx :with-test))
 (tags
  (monopoly university version)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
