open Alcobar
open Console

let test s n =
  let module L = Display.Line in
  let w = n mod 64 in
  if w < 0 then
    check
      (try
         ignore (L.bar w);
         false
       with Invalid_argument _ -> true)
  else begin
    ignore
      (L.v
         [
           L.spinner;
           L.label;
           L.bar w;
           L.percent;
           L.count;
           L.rate;
           L.elapsed;
           L.text s;
           L.accent s;
           L.spacer;
         ]);
    ignore L.default;
    List.iter
      (fun pieces ->
        check
          (try
             ignore (L.v pieces);
             false
           with Invalid_argument _ -> true))
      [ [ L.label; L.label ]; [ L.spacer; L.spacer ] ]
  end

let suite =
  ("api.display_line", [ test_case "row layout validation" [ bytes; int ] test ])
