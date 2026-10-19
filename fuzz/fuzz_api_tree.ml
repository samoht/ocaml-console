open Alcobar
open Console

let test s =
  let t =
    Tree.v ~guide:Guide.ascii (Span.text s)
      [ Tree.v (Span.styled Style.bold s) [] ]
  in
  ignore (Tree.to_string ~styled:true t);
  ignore (Fmt.str "%a" Tree.pp t);
  let safe = Console_text.sanitize ~keep_newlines:false s in
  let t =
    Tree.v ~guide:Guide.ascii (Span.text safe) [ Tree.v (Span.text safe) [] ]
  in
  let out = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer out in
  Format.pp_set_margin ppf 40;
  Tree.pp ppf t;
  Format.pp_print_flush ppf ();
  List.iter
    (fun line -> check (Width.string_width line <= 40))
    (String.split_on_char '\n' (Buffer.contents out))

let suite =
  ( "api.tree",
    [ test_case "tree printers accept arbitrary labels" [ bytes ] test ] )
