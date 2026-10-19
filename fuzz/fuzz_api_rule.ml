open Alcobar
open Console

let test s n =
  let b = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer b in
  let width = 8 + (n land 63) in
  Format.pp_set_margin ppf width;
  if Width.string_width (Console_text.sanitize ~keep_newlines:false s) <> 1 then
    check
      (try
         ignore (Rule.v ~glyph:s ());
         false
       with Invalid_argument _ -> true)
  else
    List.iter
      (fun align ->
        Rule.pp ppf
          (Rule.v ~glyph:s ~align ~style:Style.bold ~label:(Span.text s) ());
        Format.pp_print_flush ppf ())
      [ `Left; `Center; `Right ]

let suite =
  ( "api.rule",
    [ test_case "rule accepts arbitrary glyphs and labels" [ bytes; int ] test ]
  )
