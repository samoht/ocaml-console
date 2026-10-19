open Alcobar
open Console

let test n a =
  let width = n mod 65 in
  let brackets = n land 1 = 0 in
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
  check_width (fun () -> Bar.v ~brackets ~width a) (width >= 0)

let suite = ("api.bar", [ test_case "bars conserve width" [ int; float ] test ])
