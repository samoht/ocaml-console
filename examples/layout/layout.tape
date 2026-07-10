# VHS tape for the side-by-side progress-bar layout demo.
#   Render from the repo root:  vhs ocaml-tty/examples/layout/layout.tape
# Warm the build first so no compile output leaks into the recording:
#   opam exec -- dune build ocaml-tty/examples/layout/main.exe

Output ocaml-tty/examples/layout/layout.gif

Set Shell "bash"
Set FontSize 18
Set Width 1000
Set Height 460
Set Theme "Builtin Dark"
Set Padding 20

Type "opam exec -- dune exec ocaml-tty/examples/layout/main.exe"
Enter
Sleep 10s
