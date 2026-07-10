# VHS tape for the Matrix ASCII rain demo.
#   Render from the repo root:  vhs ocaml-tty/examples/matrix/matrix.tape
# Warm the build first so no compile output leaks into the recording:
#   opam exec -- dune build ocaml-tty/examples/matrix/main.exe

Output ocaml-tty/examples/matrix/matrix.gif

Set Shell "bash"
Set FontSize 18
Set Width 1000
Set Height 560
Set Theme "Builtin Dark"
Set Padding 20

Type "opam exec -- dune exec ocaml-tty/examples/matrix/main.exe"
Enter
Sleep 9s
