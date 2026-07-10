let test_dimensions_are_usable () =
  let width, height = Console_eio.Terminal.dimensions () in
  Alcotest.(check bool) "width is usable" true (width > 10);
  Alcotest.(check bool) "height is usable" true (height > 2)

let test_accessors_match_dimensions () =
  let width, height = Console_eio.Terminal.dimensions () in
  Alcotest.(check int) "width" width (Console_eio.Terminal.width ());
  Alcotest.(check int) "height" height (Console_eio.Terminal.height ())

let test_is_tty_matches_stdout () =
  Alcotest.(check bool)
    "stdout tty state" (Unix.isatty Unix.stdout)
    (Console_eio.Terminal.is_tty ())

let suite =
  ( "terminal",
    [
      Alcotest.test_case "dimensions are usable" `Quick
        test_dimensions_are_usable;
      Alcotest.test_case "accessors match dimensions" `Quick
        test_accessors_match_dimensions;
      Alcotest.test_case "is_tty matches stdout" `Quick
        test_is_tty_matches_stdout;
    ] )
