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
   Python's Rich and Go's Charm/lipgloss. The console.tty library decides
   colour and UTF-8 for the standard outputs from the terminal and the
   environment, with Unix alone.")
 (depends
  (ocaml (>= 5.4))
  (fmt (>= 0.9))
  (uucp (>= 18.0.0))
  (uuseg (>= 18.0.0))
  (jsont (>= 0.3.0))
  (bytesrw (>= 0.1.0))
  ; The tests' terminal emulator, from the released matrix. It lacks fixes
  ; some tests need, which are skipped until console moves to matrix 0.2.0.
  (matrix (and :with-test (>= 0.1.0) (< 0.2.0)))
  (alcotest (and :with-test (>= 1.7.0)))
  (astring :with-test)
  (mdx :with-test)
  (alcobar (and :with-test (>= 0.3.2)))
  (re :with-test)))

(package
 (name console-eio)
 (synopsis "Eio backend for console")
 (tags (command-line-utilities format eio))
 (description
  "Runs a Console.Display on the terminal with Eio: finds whether the
   output is a terminal and its size, redraws the progress rows, prints the
   program's output and Logs messages above them and gives the terminal
   back however the program ends. Also asks questions on the terminal,
   passwords included.")
 (depends
  (ocaml (>= 5.4))
  (console (= :version))
  (eio (>= 1.4))
  (mtime (>= 2.0.0))
  fmt
  logs
  (alcotest (and :with-test (>= 1.7.0)))
  (astring :with-test)
  (mdx :with-test)
  (cmdliner :with-test)
  (cstruct :with-test)
  (eio_main :with-test)
  (alcobar (and :with-test (>= 0.3.2)))
  (jsont :with-test)
  (bytesrw :with-test)))
