(lang dune 3.0)

(name tw)

(generate_opam_files true)

(source
 (github username/tw))

(authors "Your Name")

(maintainers "Your Name")

(license MIT)

(package
 (name tw)
 (synopsis "A DSL for generating CSS that matches Tailwind CSS classes")
 (description "tw is an OCaml DSL that compiles to CSS, providing Tailwind CSS-like utility classes")
 (depends
  ocaml
  dune
  dune-build-info
  cmdliner
  fmt
  re
  (alcotest :with-test)
  (astring :with-test)
  (crunch :with-test)))