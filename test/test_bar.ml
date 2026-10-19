(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* Count non-overlapping occurrences of [sub] in [s]. ANSI escapes never contain
   the block glyphs, so counting them measures the visible fill directly. *)
let count sub s =
  let n = String.length sub and m = String.length s in
  let c = ref 0 and i = ref 0 in
  while !i + n <= m do
    if String.sub s !i n = sub then begin
      incr c;
      i := !i + n
    end
    else incr i
  done;
  !c

let full = count "█"
let track = count "░"

(* Blocky fills [pct]% of the [width] cells with full blocks and the rest with a
   shaded track. *)
let test_blocky_fill () =
  let s = Bar.render ~style:`Blocky ~width:10 ~pct:30 () in
  Alcotest.(check int) "three full blocks" 3 (full s);
  Alcotest.(check int) "seven track cells" 7 (track s)

let test_blocky_extremes () =
  let e = Bar.render ~style:`Blocky ~width:10 ~pct:0 () in
  Alcotest.(check int) "empty has no fill" 0 (full e);
  Alcotest.(check int) "empty is all track" 10 (track e);
  let f = Bar.render ~style:`Blocky ~width:10 ~pct:100 () in
  Alcotest.(check int) "full is all fill" 10 (full f);
  Alcotest.(check int) "full has no track" 0 (track f)

(* [pct] is clamped to 0..100: out-of-range values render as the endpoints. *)
let test_pct_clamped () =
  Alcotest.(check string)
    "negative clamps to 0"
    (Bar.render ~style:`Blocky ~width:8 ~pct:0 ())
    (Bar.render ~style:`Blocky ~width:8 ~pct:(-50) ());
  Alcotest.(check string)
    "over 100 clamps to 100"
    (Bar.render ~style:`Blocky ~width:8 ~pct:100 ())
    (Bar.render ~style:`Blocky ~width:8 ~pct:200 ())

(* Smooth uses whole eighth-cell fills: 50% of 10 cells is exactly 5 full cells
   (80 eighths * 50% = 40 = 5 cells, no partial). *)
let test_smooth_fill () =
  let s = Bar.render ~style:`Smooth ~width:10 ~pct:50 () in
  Alcotest.(check int) "five full cells" 5 (full s)

(* A single cell at 50% is a half-cell glyph (8 eighths * 50% = 4 = the "▌"). *)
let test_smooth_partial () =
  let s = Bar.render ~style:`Smooth ~width:1 ~pct:50 () in
  Alcotest.(check int) "one half-cell glyph" 1 (count "▌" s)

(* Style and colour resolve from the theme when not given, and an explicit style
   overrides the theme's. *)
let test_style_resolution () =
  Alcotest.(check int)
    "dos theme gives a blocky bar" 5
    (full (Bar.render ~theme:Theme.dos ~width:10 ~pct:50 ()));
  Alcotest.(check int)
    "explicit smooth overrides the theme" 5
    (full (Bar.render ~theme:Theme.dos ~style:`Smooth ~width:10 ~pct:50 ()))

(* The indeterminate bar sweeps a triangle wave up and back over two seconds. At
   width 100 the blocky fill count equals the percentage. *)
let test_at_triangle () =
  let pct elapsed = full (Bar.at ~style:`Blocky ~width:100 ~elapsed ()) in
  Alcotest.(check int) "t=0 empty" 0 (pct 0.);
  Alcotest.(check int) "t=0.5 half, rising" 50 (pct 0.5);
  Alcotest.(check int) "t=1 full at the peak" 100 (pct 1.0);
  Alcotest.(check int) "t=1.5 half, falling" 50 (pct 1.5);
  Alcotest.(check int) "t=2 back to empty" 0 (pct 2.0)

let test_invalid_width () =
  Alcotest.check_raises "render width"
    (Invalid_argument "Console.Bar.render: negative width") (fun () ->
      ignore (Bar.render ~width:(-1) ~pct:0 ()));
  Alcotest.check_raises "animation width"
    (Invalid_argument "Console.Bar.at: negative width") (fun () ->
      ignore (Bar.at ~width:(-1) ~elapsed:0. ()))

(* {1 Bars as spans} *)

let plain = Span.to_string
let ansi = Span.to_string ~styled:true
let red = Style.fg Color.red
let green = Style.fg Color.green
let blue = Style.fg Color.blue
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

(* bar.mli, cells: to the nearest cell, at least one for a positive value, at
   most the width. *)
let test_cells () =
  let check msg want x =
    Alcotest.(check int) msg want (Bar.cells ~width:16 ~scale:100. x)
  in
  check "zero" 0 0.;
  check "negative" 0 (-5.);
  check "nan" 0 Float.nan;
  check "tiny is one" 1 0.1;
  check "half" 8 50.;
  check "4.96 cells is 5" 5 31.;
  check "past the scale" 16 1000.;
  Alcotest.(check int) "zero scale" 0 (Bar.cells ~width:16 ~scale:0. 5.)

(* bar.mli, spread: solid to the median, shaded to the 95th percentile. *)
let test_spread () =
  let spread = Bar.spread ~width:10 in
  check_plain "two levels" "███▒▒▒▒░░░" (spread ~scale:10. 3. 7.);
  check_plain "tiny values" "█░░░░░░░░░" (spread ~scale:100. 0.1 0.2);
  check_plain "p95 past the scale" "█████▒▒▒▒▒" (spread ~scale:10. 5. 20.);
  check_plain "p95 below the median" "██████░░░░" (spread ~scale:10. 6. 2.);
  check_plain "zero scale" "░░░░░░░░░░" (spread ~scale:0. 6. 2.);
  check_plain "brackets" "▕██▒░▏"
    (Bar.spread ~brackets:true ~width:4 ~scale:4. 2. 3.);
  check_ansi "styles"
    [ Span.styled red "█"; Span.styled red "▒▒"; Span.styled dim "░" ]
    (Bar.spread ~style:red ~track:dim ~width:4 ~scale:4. 1. 3.)

(* bar.mli, stacked: the largest remainder method. Shares 1.4, 2.1 and 3.5 of
   seven cells floor to 1, 2 and 3; the one cell left goes to the largest
   remainder, 0.5, so the third part, not the first. *)
let test_stacked_largest_remainder () =
  let stacked = Bar.stacked ~track:dim in
  check_ansi "2:3:5 on seven cells"
    [ Span.styled red "█"; Span.styled green "██"; Span.styled blue "████" ]
    (stacked ~width:7 [ (2, red); (3, green); (5, blue) ]);
  check_ansi "a tie goes to the earlier part"
    [ Span.styled red "████"; Span.styled green "███"; Span.styled blue "███" ]
    (stacked ~width:10 [ (1, red); (1, green); (1, blue) ]);
  check_ansi "a part with no cell adds nothing"
    [ Span.styled red "██" ]
    (stacked ~width:2 [ (3, red); (1, green) ])

(* bar.mli, stacked: the cells add up to the width exactly. Six shares of
   twenty cells, 20/21 to 120/21, floor to fifteen; the five cells left go to
   the first five parts, whose remainders are the largest. *)
let test_stacked_sums_to_width () =
  let styles = List.init 6 (fun i -> Style.fg (Color.palette (i + 1))) in
  let parts = List.mapi (fun i s -> (i + 1, s)) styles in
  check_ansi "1:2:3:4:5:6 on twenty cells"
    (List.map2
       (fun s n ->
         Span.styled s (String.concat "" (List.init n (fun _ -> "█"))))
       styles [ 1; 2; 3; 4; 5; 5 ])
    (Bar.stacked ~width:20 parts);
  check_plain "1:1:1 on ten cells" "██████████"
    (Bar.stacked ~width:10 [ (1, red); (1, green); (1, blue) ])

let test_stacked_empty () =
  check_ansi "zero total"
    [ Span.styled dim "░░░░░" ]
    (Bar.stacked ~track:dim ~width:5 [ (0, red); (0, green) ]);
  check_ansi "no part"
    [ Span.styled dim "░░░░░" ]
    (Bar.stacked ~track:dim ~width:5 []);
  check_plain "brackets" "▕███▏"
    (Bar.stacked ~brackets:true ~width:3 [ (1, red) ])

let test_span_invalid () =
  Alcotest.check_raises "v width"
    (Invalid_argument "Console.Bar.v: negative width") (fun () ->
      ignore (Bar.v ~width:(-1) 0.5));
  Alcotest.check_raises "spread width"
    (Invalid_argument "Console.Bar.spread: negative width") (fun () ->
      ignore (Bar.spread ~width:(-1) ~scale:1. 0.5 0.5));
  Alcotest.check_raises "stacked width"
    (Invalid_argument "Console.Bar.stacked: negative width") (fun () ->
      ignore (Bar.stacked ~width:(-1) [ (1, red) ]));
  Alcotest.check_raises "stacked count"
    (Invalid_argument "Console.Bar.stacked: negative count") (fun () ->
      ignore (Bar.stacked ~width:4 [ (2, red); (-1, green) ]))

let suite =
  ( "bar",
    [
      Alcotest.test_case "v eighths" `Quick test_v_eighths;
      Alcotest.test_case "v rounding" `Quick test_v_rounding;
      Alcotest.test_case "v clamped" `Quick test_v_clamped;
      Alcotest.test_case "v styles" `Quick test_v_styles;
      Alcotest.test_case "cells" `Quick test_cells;
      Alcotest.test_case "spread" `Quick test_spread;
      Alcotest.test_case "stacked largest remainder" `Quick
        test_stacked_largest_remainder;
      Alcotest.test_case "stacked sums to width" `Quick
        test_stacked_sums_to_width;
      Alcotest.test_case "stacked empty" `Quick test_stacked_empty;
      Alcotest.test_case "span invalid" `Quick test_span_invalid;
      Alcotest.test_case "blocky fill" `Quick test_blocky_fill;
      Alcotest.test_case "blocky extremes" `Quick test_blocky_extremes;
      Alcotest.test_case "pct clamped" `Quick test_pct_clamped;
      Alcotest.test_case "smooth fill" `Quick test_smooth_fill;
      Alcotest.test_case "smooth partial" `Quick test_smooth_partial;
      Alcotest.test_case "style resolution" `Quick test_style_resolution;
      Alcotest.test_case "at triangle wave" `Quick test_at_triangle;
      Alcotest.test_case "invalid width" `Quick test_invalid_width;
    ] )
