open Alcobar
open Console

let test s n =
  let s = Console_text.sanitize ~keep_newlines:false s in
  let w = 2 + (n land 63) in
  let lines = Span.wrap w (Span.styled Style.bold s) in
  check (lines <> []);
  List.iter (fun line -> check (Span.width line <= w)) lines;
  check
    (try
       ignore (Span.wrap 0 (Span.text s));
       false
     with Invalid_argument _ -> true)

let suite =
  ( "api.span_bounds",
    [ test_case "wrapped spans fit width" [ bytes; int ] test ] )
