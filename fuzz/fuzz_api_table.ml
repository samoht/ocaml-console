open Alcobar
open Console

let test s n =
  let lo = n mod 16 and hi = n / 16 mod 32 in
  let valid = lo >= 0 && hi >= 0 && lo <= hi in
  if not valid then
    check
      (try
         ignore (Table.column ~min_width:lo ~max_width:hi s);
         false
       with Invalid_argument _ -> true)
  else begin
    let col =
      Table.column ~min_width:lo ~max_width:hi ~align:`Right ~overflow:`Truncate
        ~style:Style.bold s
    in
    let t = Table.of_rows [ col ] [ [ Span.text s ] ] in
    ignore (Table.to_string ~styled:true ~width:32 t);
    ignore (Fmt.str "%a" Table.pp t);
    List.iter
      (fun row ->
        check
          (try
             ignore (Table.of_rows [ col ] [ row ]);
             false
           with Invalid_argument _ -> true))
      [ []; [ Span.empty; Span.empty ] ]
  end

let suite =
  ( "api.table",
    [ test_case "table column and row contracts" [ bytes; int ] test ] )
