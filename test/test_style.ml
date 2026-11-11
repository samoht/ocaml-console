(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_none () =
  let ansi = View_harness.sgr Style.none in
  Alcotest.(check string) "none produces empty" "" ansi

let test_bold () =
  let ansi = View_harness.sgr Style.bold in
  Alcotest.(check string) "bold ANSI code" "\027[1m" ansi

let test_composition () =
  let style = Style.(bold + fg Color.red + underline) in
  Alcotest.(check bool)
    "composed style not none" false
    (Style.equal style Style.none)

let red = Color.rgb 255 0 0
let blue = Color.rgb 0 0 255

let test_colours () =
  let s = Style.(bold + fg red + bg blue) in
  Alcotest.(check bool) "foreground" true (Style.equal Style.(s + fg red) s);
  Alcotest.(check string)
    "View_harness.sgr" "\027[1;38;2;255;0;0;48;2;0;0;255m" (View_harness.sgr s);
  Alcotest.(check bool)
    "the right colour wins" true
    (Style.equal (Style.fg blue) Style.(fg red + fg blue))

(* [printed pp] is [pp] printed on a buffer in colour at [depth], or without
   colour if [depth] is not given. *)
let printed ?depth ?(margin = 78) pp =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  Option.iter
    (fun d ->
      Fmt.set_style_renderer ppf `Ansi_tty;
      Color.set_depth ppf d)
    depth;
  pp ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* [Style.styled] works with any printer, at the formatter's depth, and
   prints plain text on a formatter without colour. *)
let test_styled () =
  let red_int ppf = Style.(styled (fg red)) Fmt.int ppf 42 in
  Alcotest.(check string) "no colour" "42" (printed red_int);
  Alcotest.(check string)
    "16 colours" "\027[91m42\027[0m"
    (printed ~depth:`Ansi_16 red_int);
  Alcotest.(check string)
    "true colour" "\027[38;2;255;0;0m42\027[0m"
    (printed ~depth:`True_color red_int);
  Alcotest.(check string)
    "a style that sets nothing" "42"
    (printed ~depth:`Ansi_16 (fun ppf -> Style.styled Style.none Fmt.int ppf 42))

(* With [Style.styled], an inner style adds to the outer one, and the outer
   style comes back after it. *)
let test_styled_nested () =
  let inner = Style.(styled (fg Color.red)) Fmt.string in
  let outer ppf () =
    Fmt.string ppf "a";
    inner ppf "b";
    Fmt.string ppf "c"
  in
  Alcotest.(check string)
    "nested" "\027[1ma\027[1;31mb\027[0m\027[1mc\027[0m"
    (printed ~depth:`Ansi_16 (fun ppf -> Style.styled Style.bold outer ppf ()))

(* With [Style.styled], escape codes take no room on the line. *)
let test_styled_width () =
  let word = Style.(styled bold) Fmt.string in
  Alcotest.(check string)
    "one line" "\027[1mabcd\027[0m \027[1mabcd\027[0m"
    (printed ~depth:`Ansi_16 ~margin:10 (fun ppf ->
         Fmt.pf ppf "@[<hov>%a@ %a@]" word "abcd" word "abcd"))

let suite =
  ( "style",
    [
      Alcotest.test_case "colours" `Quick test_colours;
      Alcotest.test_case "none" `Quick test_none;
      Alcotest.test_case "bold" `Quick test_bold;
      Alcotest.test_case "composition" `Quick test_composition;
      Alcotest.test_case "styled" `Quick test_styled;
      Alcotest.test_case "styled nested" `Quick test_styled_nested;
      Alcotest.test_case "styled width" `Quick test_styled_width;
    ] )
