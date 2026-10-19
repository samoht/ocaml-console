(lang dune 3.21)
(using mdx 0.4)

(name console)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (github samoht/ocaml-console))

(package
 (name console)
 (synopsis "Terminal styling and layout widgets")
 (tags (command-line-utilities format))
 (description
  "Type-safe terminal styling (colors, bold, italic) and layout widgets
   (tables, trees, panels) for OCaml CLI applications. Inspired by
   Python's Rich and Go's Charm/lipgloss.")
 (depends
  (ocaml (>= 5.4))
  (fmt (>= 0.9))
  logs
  matrix
  uucp
  (uutf (>= 1.0))
  (alcotest (and :with-test (>= 1.7.0)))
  (astring :with-test)
  (mdx :with-test)
  (alcobar :with-test)
  (re :with-test)))

(package
 (name console-eio)
 (synopsis "Eio backend for console")
 (tags (command-line-utilities format eio))
 (description
  "Helpers that animate Console.Display bars from an Eio fiber. Split out
   so the console library stays scheduler-agnostic.")
 (depends
  (ocaml (>= 5.4))
  console
  (eio (>= 1.4))
  fmt
  logs
  cstruct
  optint
  (alcotest (and :with-test (>= 1.7.0)))
  (astring :with-test)
  (mdx :with-test)
  (eio_main :with-test)))
