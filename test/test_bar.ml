(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* {1 Bars} *)

let plain = Bar.to_string
let ansi = Bar.to_string ~styled:true
let red = Style.fg Color.red
let dim = Style.faint
let check_plain msg want span = Alcotest.(check string) msg want (plain span)

let check_ansi msg want span =
  Alcotest.(check string)
    msg
    (Span.to_string ~styled:true (Span.concat want))
    (ansi span)

(* bar.mli, v: a cell is a heavy line, filled or track, and a half filled
   cell the left half of one. *)
let test_v_halves () =
  check_plain "empty" "━" (Bar.v ~width:1 0.);
  check_plain "half" "╸" (Bar.v ~width:1 0.5);
  check_plain "full" "━" (Bar.v ~width:1 1.)

(* bar.mli, v: the fill rounds to the nearest half cell, and a track after a
   whole cell starts with the right half of one, which leaves a gap. *)
let test_v_rounding () =
  check_plain "half of three cells" "━╸━" (Bar.v ~width:3 0.5);
  check_plain "half of four cells" "━━╺━" (Bar.v ~width:4 0.5);
  check_plain "a quarter of one cell is half of it" "╸" (Bar.v ~width:1 0.25);
  check_plain "below a quarter is none" "━━" (Bar.v ~width:2 0.12);
  check_plain "19.8 halves is 20" "━━━━━━━━━━" (Bar.v ~width:10 0.99)

(* bar.mli, v: the fraction is clamped to 0..1 and NaN is 0. *)
let test_v_clamped () =
  check_plain "negative" "━━━" (Bar.v ~width:3 (-1.));
  check_plain "over one" "━━━" (Bar.v ~width:3 2.);
  check_plain "infinity" "━━━" (Bar.v ~width:3 Float.infinity);
  check_plain "nan" "━━━" (Bar.v ~width:3 Float.nan);
  check_plain "zero width" "" (Bar.v ~width:0 0.5)

(* bar.mli, spans: the fill carries [style], the track and the brackets
   [track], and a part with no cell adds no style. *)
let test_v_styles () =
  check_ansi "fill and track"
    [ Span.styled red "━╸"; Span.styled dim "━" ]
    (Bar.v ~style:red ~track:dim ~width:3 0.5);
  check_ansi "a gap before the track"
    [ Span.styled red "━"; Span.styled dim "╺━" ]
    (Bar.v ~style:red ~track:dim ~width:3 0.34);
  check_ansi "full bar, no track"
    [ Span.styled red "━━" ]
    (Bar.v ~style:red ~track:dim ~width:2 1.);
  check_ansi "empty bar, no fill"
    [ Span.styled dim "━━" ]
    (Bar.v ~style:red ~track:dim ~width:2 0.);
  check_plain "brackets" "▕━╸━▏" (Bar.v ~brackets:true ~width:3 0.5);
  check_ansi "brackets in the track style"
    [
      Span.styled dim "▕";
      Span.styled red "━╸";
      Span.styled dim "━";
      Span.styled dim "▏";
    ]
    (Bar.v ~style:red ~track:dim ~brackets:true ~width:3 0.5)

(* bar.mli: on a formatter that is not UTF-8, whole cells of [#] on a track
   of [-]. *)
let test_ascii () =
  let ascii bar =
    let buf = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_utf_8 ppf false;
    Bar.pp ppf bar;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Alcotest.(check string) "half" "##--" (ascii (Bar.v ~width:4 0.5));
  Alcotest.(check string)
    "brackets" "[#--]"
    (ascii (Bar.v ~brackets:true ~width:3 0.4))

let test_span_invalid () =
  Alcotest.check_raises "v width"
    (Invalid_argument "Console.Bar.v: negative width") (fun () ->
      ignore (Bar.v ~width:(-1) 0.5));
  ()

let suite =
  ( "bar",
    [
      Alcotest.test_case "v halves" `Quick test_v_halves;
      Alcotest.test_case "v rounding" `Quick test_v_rounding;
      Alcotest.test_case "v clamped" `Quick test_v_clamped;
      Alcotest.test_case "v styles" `Quick test_v_styles;
      Alcotest.test_case "ASCII" `Quick test_ascii;
      Alcotest.test_case "span invalid" `Quick test_span_invalid;
    ] )
