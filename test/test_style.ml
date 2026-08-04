(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_none () =
  let ansi = Style.to_ansi Style.none in
  Alcotest.(check string) "none produces empty" "" ansi

let test_bold () =
  let ansi = Style.to_ansi Style.bold in
  Alcotest.(check string) "bold ANSI code" "\027[1m" ansi

let test_composition () =
  let style = Style.(bold + fg Color.red + underline) in
  Alcotest.(check bool) "composed style not none" false (Style.is_none style)

let render renderer pp v =
  let buf = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf renderer;
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_styled_honours_formatter () =
  let pp = Style.styled Style.bold Fmt.string in
  Alcotest.(check string) "plain formatter" "hello" (render `None pp "hello");
  Alcotest.(check bool)
    "ANSI formatter" true
    (String.contains (render `Ansi_tty pp "hello") '\027')

let suite =
  ( "style",
    [
      Alcotest.test_case "none" `Quick test_none;
      Alcotest.test_case "bold" `Quick test_bold;
      Alcotest.test_case "composition" `Quick test_composition;
      Alcotest.test_case "formatter controls styling" `Quick
        test_styled_honours_formatter;
    ] )
