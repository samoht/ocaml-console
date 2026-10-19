(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let frames = [| "a"; "b"; "c" |]

let test_empty () =
  Alcotest.check_raises "empty frames rejected"
    (Invalid_argument "Console.Spinner.v: no frames") (fun () ->
      ignore (Spinner.v [||]))

let test_frames_are_copied () =
  let given = Array.copy frames in
  let spinner = Spinner.v given in
  given.(0) <- "z";
  Alcotest.(check string)
    "the caller's array is not shared" "a"
    (Spinner.frame spinner ~elapsed:0.)

let test_frame () =
  let t = Spinner.v frames in
  let at elapsed = Spinner.frame ~fps:2. t ~elapsed in
  Alcotest.(check (list string))
    "two frames a second, cycling"
    [ "a"; "a"; "b"; "c"; "a" ]
    [ at 0.; at 0.25; at 0.5; at 1.; at 1.5 ];
  Alcotest.(check string) "negative time" "a" (at (-1.));
  Alcotest.(check string) "infinite time" "a" (at Float.infinity);
  Alcotest.(check string) "nan" "a" (at Float.nan)

let test_fps () =
  List.iter
    (fun fps ->
      Alcotest.check_raises (Fmt.str "fps %g" fps)
        (Invalid_argument "Console.Spinner.frame: fps must be positive")
        (fun () -> ignore (Spinner.frame ~fps (Spinner.v frames) ~elapsed:0.)))
    [ 0.; -1.; Float.nan; Float.infinity ]

let cycle t n = List.init n (fun i -> Spinner.frame t ~elapsed:(float i /. 10.))

let test_predefined () =
  Alcotest.(check (list string))
    "braille"
    [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏"; "⠋" ]
    (cycle Spinner.braille 11);
  Alcotest.(check (list string))
    "ascii"
    [ "|"; "/"; "-"; "\\"; "|" ]
    (cycle Spinner.ascii 5)

let test_static () =
  List.iter
    (fun elapsed ->
      Alcotest.(check string)
        "one frame" "*"
        (Spinner.frame Spinner.static ~elapsed))
    [ 0.; 0.1; 0.25; 1.; 3600. ]

let test_frames_print () =
  Alcotest.(check string)
    "control characters are spaces" "a b"
    (Spinner.frame (Spinner.v [| "a\027b" |]) ~elapsed:0.)

let suite =
  ( "spinner",
    [
      Alcotest.test_case "static does not move" `Quick test_static;
      Alcotest.test_case "frames print" `Quick test_frames_print;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "frames are copied" `Quick test_frames_are_copied;
      Alcotest.test_case "frame" `Quick test_frame;
      Alcotest.test_case "fps" `Quick test_fps;
      Alcotest.test_case "predefined" `Quick test_predefined;
    ] )
