open Alcobar
open Console

let test s t =
  List.iter
    (fun frames ->
      let spinner = Spinner.v frames in
      ignore (Fmt.str "%a" Spinner.pp spinner);
      let f = Spinner.frame spinner ~elapsed:t in
      check (Array.exists (String.equal f) frames);
      if (not (Float.is_finite t)) || t <= 0. then check (f = frames.(0)))
    [ [| s; "x" |]; [| "|"; "/"; "-"; "\\" |] ]

let suite =
  ( "api.spinner",
    [ test_case "predefined spinner sampling" [ bytes; float ] test ] )
