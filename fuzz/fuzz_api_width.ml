open Alcobar

let test s n =
  let w = 2 + (n land 63) in
  ignore (Width.shorten w s);
  ignore (Width.elide w s);
  ignore (Width.wrap ~indent:(n land 3) w s);
  ignore (Width.atoms s);
  let sanitized = Console_text.sanitize ~keep_newlines:false s in
  let atoms = Width.atoms sanitized in
  check (List.for_all (fun (_, w) -> w >= 0) atoms);
  check (String.concat "" (List.map fst atoms) = sanitized);
  check
    (List.fold_left (fun acc (_, w) -> acc + w) 0 atoms
    = Width.string_width sanitized);
  List.iter
    (fun f ->
      let result = f w sanitized in
      check (Width.string_width result <= w);
      check (f w result = result))
    [ Width.shorten; Width.elide ];
  List.iter
    (fun f ->
      check
        (Width.string_width (f w sanitized)
        = max w (Width.string_width sanitized)))
    [ Width.pad_left; Width.pad_right ];
  ignore (Width.fold ~split:true w sanitized)

let suite =
  ( "api.width",
    [ test_case "width atom agreement and fitting laws" [ bytes; int ] test ] )
