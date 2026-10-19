open Alcobar
open Console

let test s =
  let spinner = Spinner.v [| s |] in
  let t =
    Theme.v ~accent:Color.red
      ~palette:[ Color.blue; Color.green ]
      ~border:Border.double ~guide:Guide.ascii ~spinner ()
  in
  check (Color.equal (Theme.accent t) Color.red);
  check (Theme.palette t = [ Color.blue; Color.green ]);
  check (Border.chars (Theme.border t) = Border.chars Border.double);
  check (Guide.branch (Theme.guide t) = Guide.branch Guide.ascii);
  check (Spinner.frame (Theme.spinner t) ~elapsed:0. = s);
  check (Theme.palette (Theme.v ~accent:Color.red ()) = [ Color.red ]);
  List.iter
    (fun t ->
      ignore (Fmt.str "%a" Theme.pp t);
      ignore (Theme.accent t);
      ignore (Theme.palette t);
      ignore (Theme.border t);
      ignore (Theme.guide t);
      ignore (Theme.spinner t))
    [ t; Theme.unicode; Theme.dos; Theme.matrix; Theme.rainbow ]

let suite =
  ("api.theme", [ test_case "theme overrides and defaults" [ bytes ] test ])
