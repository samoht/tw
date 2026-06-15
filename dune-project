(lang dune 3.21)

(using mdx 0.2)

(name tw)

(implicit_transitive_deps false)

(generate_opam_files true)

(source
 (github samoht/tw))

(authors "Thomas Gazagnaire")

(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(license ISC)

(package
 (name tw)
 (synopsis "Type-safe Tailwind CSS v4 in OCaml")
 (description
  "tw is a type-safe OCaml implementation of Tailwind CSS v4. It compiles to CSS through a strongly-typed variable system that mirrors Tailwind's pipeline, producing byte-for-byte identical output. Supports all core utilities and the official forms and typography plugins.")
 (depends
  (ocaml (>= 5.2))
  dune
  cascade
  cmdliner
  fmt
  re
  uutf
  (brr (>= 0.0.6))
  js_of_ocaml
  (alcotest :with-test)
  (alcotest-js :with-test)
  (astring :with-test)
  (crunch :with-test)
  (alcobar :with-test)
  (mdx :with-test)))
