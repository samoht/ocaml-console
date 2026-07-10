module type Surface = sig end

let exposed name _module = Alcotest.(check string) name name name

let test_public_surface () =
  exposed "Color" (module Console.Color : Surface);
  exposed "Style" (module Console.Style : Surface);
  exposed "Width" (module Console.Width : Surface);
  exposed "Table" (module Console.Table : Surface);
  exposed "Display" (module Console.Display : Surface);
  let ctx =
    Console.Display.ctx ~ppf:Format.std_formatter
      ~now:(fun () -> 0.)
      ~dimensions:(fun () -> (80, 24))
      ~is_tty:false ()
  in
  Alcotest.(check int)
    "display dimensions" 80
    (fst (Console.Display.dimensions ctx));
  Alcotest.(check bool) "tty flag" false (Console.Display.is_tty ctx)

let suite =
  ("console", [ Alcotest.test_case "public surface" `Quick test_public_surface ])
