open Alcobar

let test s keep =
  let sanitized = Console_text.sanitize ~keep_newlines:keep s in
  let strip = Console_text.strip ~keep_newlines:keep s in
  let styled = Console_text.sanitize_styles ~keep_newlines:keep s in
  check
    (String.is_valid_utf_8 sanitized
    && String.is_valid_utf_8 strip
    && String.is_valid_utf_8 styled);
  check (Console_text.sanitize ~keep_newlines:keep sanitized = sanitized);
  check (Console_text.strip ~keep_newlines:keep strip = strip);
  check (Console_text.strip ~keep_newlines:keep styled = strip);
  if not keep then
    check
      ((not (String.contains sanitized '\n'))
      && not (String.contains strip '\n'))

let suite =
  ("api.console_text", [ test_case "text sanitation laws" [ bytes; bool ] test ])
