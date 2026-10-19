open Alcobar
open Console

let test s n =
  let gutter = n mod 5 in
  let block = Fmt.const Fmt.string s in
  if gutter < 0 then begin
    check
      (try
         ignore (Fmt.str "%a" (Layout.hcat ~gutter [ block ]) ());
         false
       with Invalid_argument _ -> true);
    check
      (try
         ignore (Fmt.str "%a" (Layout.vcat ~gutter [ block ]) ());
         false
       with Invalid_argument _ -> true)
  end
  else begin
    List.iter
      (fun align ->
        ignore (Fmt.str "%a" (Layout.hcat ~align ~gutter [ block; block ]) ()))
      [ `Top; `Bottom ];
    ignore (Fmt.str "%a" (Layout.vcat ~gutter [ block; block ]) ())
  end

let suite =
  ( "api.layout",
    [ test_case "layout arbitrary blocks and gutters" [ bytes; int ] test ] )
