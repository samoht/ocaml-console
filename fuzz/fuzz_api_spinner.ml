open Alcobar
open Console

let test s t =
  List.iter
    (fun spinner ->
      ignore (Fmt.str "%a" Spinner.pp spinner);
      let frames = Spinner.frames spinner in
      let f = Anim.frame (Spinner.anim spinner) ~elapsed:t in
      check (Array.exists (String.equal f) frames);
      if (not (Float.is_finite t)) || t <= 0. then check (f = frames.(0)))
    [ Spinner.v [| s; "x" |]; Spinner.ascii; Spinner.braille ]

let suite =
  ( "api.spinner",
    [ test_case "predefined spinner sampling" [ bytes; float ] test ] )
