(* Minimal test - compile and run directly: ocamlfind ocamlopt -package unix
   -linkpkg -o /tmp/test_progress minimal_progress.ml /tmp/test_progress *)

let () =
  Fmt.pr "Terminal test - should update in place:@.";
  for i = 1 to 10 do
    Fmt.pr "\r[%2d/10] Display...%!" i;
    Unix.sleepf 0.3
  done;
  Fmt.pr "\n";
  Fmt.pr "Done - if you saw 10 separate lines, \\r is broken@.";
  Fmt.pr "If you saw updates in place, \\r works correctly@."
