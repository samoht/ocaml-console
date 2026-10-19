open Alcobar
open Console

let test s n =
  let width = 8 + (n land 31) in
  let p =
    Panel.v ~title:(Span.text s) ~subtitle:(Span.text s) ~padding:1 ~width
      (Span.text s)
  in
  check (Panel.content_width p = width - 4);
  check (Panel.content_width ~margin:width p <= width - 4);
  check
    (Panel.content_width ~margin:(width - 1) p
    <= Panel.content_width ~margin:width p);
  ignore (Panel.to_string ~styled:true p);
  ignore (Fmt.str "%a" Panel.pp p);
  check
    (try
       ignore (Panel.v ~padding:(-1) (Span.text s));
       false
     with Invalid_argument _ -> true);
  check
    (try
       ignore (Panel.v ~width:0 (Span.text s));
       false
     with Invalid_argument _ -> true)

let suite =
  ( "api.panel",
    [ test_case "panel content room and rendering" [ bytes; int ] test ] )
