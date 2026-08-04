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

let suite =
  ("console", [ Alcotest.test_case "public surface" `Quick test_public_surface ])
