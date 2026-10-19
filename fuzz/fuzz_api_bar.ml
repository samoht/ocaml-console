open Alcobar
open Console

let test n a b scale =
  let width = n mod 65 in
  let brackets = n land 1 = 0 in
  let count = n mod 100 in
  let check_width thunk valid =
    if valid then
      check (Span.width (thunk ()) = width + if brackets then 2 else 0)
    else
      check
        (try
           ignore (thunk ());
           false
         with Invalid_argument _ -> true)
  in
  check_width (fun () -> Bar.v ~brackets ~width a) (width >= 0);
  check_width (fun () -> Bar.spread ~brackets ~width ~scale a b) (width >= 0);
  check_width
    (fun () ->
      Bar.stacked ~brackets ~width [ (count, Style.bold); (0, Style.none) ])
    (width >= 0 && count >= 0)

let suite =
  ( "api.bar",
    [
      test_case "bar variants conserve width" [ int; float; float; float ] test;
    ] )
