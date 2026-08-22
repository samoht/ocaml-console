module type Surface = sig end

let exposed name _module = Alcotest.(check string) name name name

let test_public_surface () =
  exposed "Color" (module Console.Color : Surface);
  exposed "Style" (module Console.Style : Surface);
  exposed "Width" (module Console.Width : Surface);
  exposed "Guide" (module Console.Guide : Surface);
  exposed "Rule" (module Console.Rule : Surface);
  exposed "Table" (module Console.Table : Surface);
  exposed "Display" (module Console.Display : Surface)

(* The library's terminal-geometry contract says that malformed UTF-8 renders as
   a replacement glyph and that terminal controls in content cannot move the
   cursor. Every widget applies that to what it lays out. A caller printing a
   string of its own -- a subprocess's stderr, a peer's name off the network --
   needs the same rule over a plain string, and the only way to reach it was to
   wrap the string in a Span and unwrap the result, so callers write private
   ANSI filters instead and each one draws its own line between a control
   character and a printable one. *)
let test_sanitize () =
  Alcotest.(check string)
    "a tab, a newline and an escape all become spaces" "a b c d"
    (Console.sanitize ~keep_newlines:false "a\tb\nc\027d");
  Alcotest.(check string)
    "a newline is a row boundary, so it is kept by default" "a b\nc d"
    (Console.sanitize "a\tb\nc\027d");
  Alcotest.(check string)
    "a malformed sequence becomes the replacement glyph" "a\u{FFFD}b"
    (Console.sanitize "a\255b");
  Alcotest.(check string)
    "a C1 control, which UTF-8 hides behind two bytes, becomes a space" "a b"
    (Console.sanitize "a\u{009B}b");
  Alcotest.(check string)
    "DEL becomes a space" "a b"
    (Console.sanitize "a\127b");
  Alcotest.(check string)
    "well-formed text, wide glyphs included, is returned unchanged"
    "\u{4E16}\u{754C} ok"
    (Console.sanitize "\u{4E16}\u{754C} ok")

let suite =
  ( "console",
    [
      Alcotest.test_case "public surface" `Quick test_public_surface;
      Alcotest.test_case "sanitize" `Quick test_sanitize;
    ] )
