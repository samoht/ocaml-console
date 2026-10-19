open Alcobar
open Console

let test s uri n =
  let w = 2 + (n land 63) in
  let span = Span.(empty ++ styled Style.bold s ++ text uri) in
  ignore (Span.to_string ~styled:true span);
  ignore (Fmt.str "%a" Span.pp span);
  ignore (Span.to_string (Span.with_style Style.italic span));
  check
    (Span.to_string span = Console_text.sanitize s ^ Console_text.sanitize uri);
  let lines = Span.lines span in
  check (lines <> []);
  check
    (List.length lines
    = List.length (String.split_on_char '\n' (Span.to_string span)));
  let joined = Span.concat ~sep:(Span.text "\n") lines in
  check (Span.to_string joined = Span.to_string span);
  let invalid =
    uri = ""
    || String.exists (fun c -> Char.code c <= 32 || Char.code c >= 127) uri
  in
  if invalid then
    check
      (try
         ignore (Span.link ~uri s);
         false
       with Invalid_argument _ -> true)
  else
    check
      (Span.to_string (Span.link ~style:Style.bold ~uri s)
      = Span.to_string (Span.text s));
  if Span.width span <= w then
    check (Span.to_string (Span.truncate w span) = Span.to_string span);
  let truncated = Span.truncate w span in
  check (Span.width truncated <= w);
  check (Span.to_string (Span.truncate w truncated) = Span.to_string truncated);
  check (Span.to_string (Span.truncate 0 span) = "")

let suite =
  ( "api.span",
    [ test_case "span conversion and fitting laws" [ bytes; bytes; int ] test ]
  )
