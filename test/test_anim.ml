(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* [const] ignores time; every frame is the same value. *)
let test_const () =
  let a = Anim.const 7 in
  Alcotest.(check int) "frame at 0" 7 (Anim.frame a ~elapsed:0.);
  Alcotest.(check int) "frame at 3.5" 7 (Anim.frame a ~elapsed:3.5)

(* [v]/[frame] sample the underlying function; negative elapsed clamps to 0. *)
let test_v_frame () =
  let a = Anim.v (fun ~elapsed -> int_of_float (elapsed *. 2.)) in
  Alcotest.(check int) "elapsed 0" 0 (Anim.frame a ~elapsed:0.);
  Alcotest.(check int) "elapsed 1.5 -> 3" 3 (Anim.frame a ~elapsed:1.5);
  Alcotest.(check int) "negative clamps to 0" 0 (Anim.frame a ~elapsed:(-5.))

(* [map] transforms every frame. *)
let test_map () =
  let a =
    Anim.map (fun n -> n * 10) (Anim.v (fun ~elapsed -> int_of_float elapsed))
  in
  Alcotest.(check int) "mapped frame" 20 (Anim.frame a ~elapsed:2.0)

(* Spinner and Bar animate by elapsed time. *)
let test_component_anims () =
  let s = Spinner.anim Spinner.ascii in
  Alcotest.(check string) "spinner at 0.3s" "\\" (Anim.frame s ~elapsed:0.3);
  let b = Bar.anim ~style:`Blocky ~width:10 () in
  Alcotest.(check bool)
    "bar moves" false
    (String.equal (Anim.frame b ~elapsed:0.) (Anim.frame b ~elapsed:0.5))

(* [all] samples every animation at the same elapsed time, in order. *)
let test_all () =
  let a = Anim.v (fun ~elapsed -> int_of_float elapsed) in
  let b = Anim.const 100 in
  let c = Anim.map (fun n -> n * 2) a in
  let all = Anim.all [ a; b; c ] in
  Alcotest.(check (list int))
    "samples each child at elapsed 3" [ 3; 100; 6 ]
    (Anim.frame all ~elapsed:3.0);
  Alcotest.(check (list int))
    "empty list" []
    (Anim.frame (Anim.all []) ~elapsed:1.)

let suite =
  ( "anim",
    [
      Alcotest.test_case "const" `Quick test_const;
      Alcotest.test_case "v and frame" `Quick test_v_frame;
      Alcotest.test_case "map" `Quick test_map;
      Alcotest.test_case "all" `Quick test_all;
      Alcotest.test_case "component anims" `Quick test_component_anims;
    ] )
