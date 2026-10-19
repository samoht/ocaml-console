(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* {1 Bars as spans} *)

let plain = Span.to_string
let ansi = Span.to_string ~styled:true
let red = Style.fg Color.red
let dim = Style.faint
let check_plain msg want span = Alcotest.(check string) msg want (plain span)

let check_ansi msg want span =
  Alcotest.(check string) msg (ansi (Span.concat want)) (ansi span)

(* bar.mli, v: a fraction [k/8] of one cell is the glyph of [k] eighths, the
   empty cell the track and the full one a block. *)
let test_v_eighths () =
  List.iteri
    (fun k glyph ->
      check_plain (Fmt.str "%d eighths" k) glyph
        (Bar.v ~width:1 (float_of_int k /. 8.)))
    [ "░"; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" ]

(* bar.mli, v: the fill rounds to the nearest eighth of a cell, never down. *)
let test_v_rounding () =
  check_plain "half of three cells" "█▌░" (Bar.v ~width:3 0.5);
  check_plain "half of four cells" "██░░" (Bar.v ~width:4 0.5);
  check_plain "2.4 eighths is 2" "▎" (Bar.v ~width:1 0.3);
  check_plain "2.56 eighths is 3" "▍" (Bar.v ~width:1 0.32);
  check_plain "79.2 eighths is 79" "█████████▉" (Bar.v ~width:10 0.99);
  check_plain "79.92 eighths is 80" "██████████" (Bar.v ~width:10 0.999)

(* bar.mli, v: the fraction is clamped to 0..1 and NaN is 0. *)
let test_v_clamped () =
  check_plain "negative" "░░░" (Bar.v ~width:3 (-1.));
  check_plain "over one" "███" (Bar.v ~width:3 2.);
  check_plain "infinity" "███" (Bar.v ~width:3 Float.infinity);
  check_plain "nan" "░░░" (Bar.v ~width:3 Float.nan);
  check_plain "zero width" "" (Bar.v ~width:0 0.5)

(* bar.mli, spans: the fill carries [style], the empty cells and the brackets
   [track], and a part with no cell adds no style. *)
let test_v_styles () =
  check_ansi "fill and track"
    [ Span.styled red "█▌"; Span.styled dim "░" ]
    (Bar.v ~style:red ~track:dim ~width:3 0.5);
  check_ansi "full bar, no track"
    [ Span.styled red "██" ]
    (Bar.v ~style:red ~track:dim ~width:2 1.);
  check_ansi "empty bar, no fill"
    [ Span.styled dim "░░" ]
    (Bar.v ~style:red ~track:dim ~width:2 0.);
  check_plain "brackets" "▕█▌░▏" (Bar.v ~brackets:true ~width:3 0.5);
  check_ansi "brackets in the track style"
    [
      Span.styled dim "▕";
      Span.styled red "█▌";
      Span.styled dim "░";
      Span.styled dim "▏";
    ]
    (Bar.v ~style:red ~track:dim ~brackets:true ~width:3 0.5)

let test_span_invalid () =
  Alcotest.check_raises "v width"
    (Invalid_argument "Console.Bar.v: negative width") (fun () ->
      ignore (Bar.v ~width:(-1) 0.5));
  ()

let suite =
  ( "bar",
    [
      Alcotest.test_case "v eighths" `Quick test_v_eighths;
      Alcotest.test_case "v rounding" `Quick test_v_rounding;
      Alcotest.test_case "v clamped" `Quick test_v_clamped;
      Alcotest.test_case "v styles" `Quick test_v_styles;
      Alcotest.test_case "span invalid" `Quick test_span_invalid;
    ] )
