(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let frames = [| "a"; "b"; "c" |]

let test_v_empty () =
  Alcotest.check_raises "empty frames rejected"
    (Invalid_argument "Console.Spinner.v: no frames") (fun () ->
      ignore (Spinner.v [||]))

let test_frames_roundtrip () =
  Alcotest.(check (array string))
    "frames returns what v was given" frames
    (Spinner.frames (Spinner.v frames))

let test_frames_are_immutable () =
  let source = [| "a"; "b" |] in
  let spinner = Spinner.v source in
  source.(0) <- "changed";
  Alcotest.(check string)
    "constructor copies its argument" "a"
    (Spinner.frames spinner).(0);
  let returned = Spinner.frames spinner in
  returned.(0) <- "changed again";
  Alcotest.(check string)
    "accessor returns a copy" "a"
    (Spinner.frames spinner).(0)

(* The animation shows frame [floor (elapsed *. fps)], wrapping; fps 2 keeps
   the products exact so the boundaries are unambiguous. *)
let test_anim_advances_with_fps () =
  let a = Spinner.anim ~fps:2. (Spinner.v frames) in
  let c msg expected elapsed =
    Alcotest.(check string) msg expected (Anim.frame a ~elapsed)
  in
  c "elapsed 0" "a" 0.;
  c "negative elapsed shows first" "a" (-1.);
  c "0.5s -> 1" "b" 0.5;
  c "1.0s -> 2" "c" 1.0;
  c "1.5s -> 3 wraps" "a" 1.5;
  c "infinity shows first" "a" Float.infinity;
  c "nan shows first" "a" Float.nan;
  Alcotest.(check string)
    "default fps at 0" "a"
    (Anim.frame (Spinner.anim (Spinner.v frames)) ~elapsed:0.)

let test_fps_range () =
  let rejected fps =
    Alcotest.check_raises "non-positive fps rejected"
      (Invalid_argument "Console.Spinner.anim: fps must be positive") (fun () ->
        ignore (Spinner.anim ~fps (Spinner.v frames)))
  in
  rejected 0.;
  rejected (-1.);
  rejected Float.infinity;
  rejected Float.nan

let test_presets () =
  Alcotest.(check int)
    "braille has ten frames" 10
    (Array.length (Spinner.frames Spinner.braille));
  Alcotest.(check int)
    "ascii has four frames" 4
    (Array.length (Spinner.frames Spinner.ascii))

let suite =
  ( "spinner",
    [
      Alcotest.test_case "v rejects empty" `Quick test_v_empty;
      Alcotest.test_case "frames roundtrip" `Quick test_frames_roundtrip;
      Alcotest.test_case "frames are immutable" `Quick test_frames_are_immutable;
      Alcotest.test_case "anim advances with fps" `Quick
        test_anim_advances_with_fps;
      Alcotest.test_case "fps range" `Quick test_fps_range;
      Alcotest.test_case "presets" `Quick test_presets;
    ] )
