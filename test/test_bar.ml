(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

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

let test_stacked_large_counts () =
  check_ansi "equal large counts tie in input order"
    [ Span.styled red "█" ]
    (Bar.stacked ~width:1 [ (max_int, red); (max_int, green); (2, blue) ])

let test_spread_zero () =
  check_plain "zero cells requested" "" (Bar.spread ~width:0 ~scale:1. 0.5 1.)

let test_stacked_product () =
  check_plain "the only part receives every cell" "██"
    (Bar.stacked ~width:2 [ (max_int, red) ])

let suite =
  ( "bar",
    [
      Alcotest.test_case "stacked keeps width with a large single count" `Quick
        test_stacked_product;
      Alcotest.test_case "spread respects zero width" `Quick test_spread_zero;
      Alcotest.test_case "stacked accepts large counts" `Quick
        test_stacked_large_counts;
      Alcotest.test_case "v eighths" `Quick test_v_eighths;
      Alcotest.test_case "v rounding" `Quick test_v_rounding;
      Alcotest.test_case "v clamped" `Quick test_v_clamped;
      Alcotest.test_case "v styles" `Quick test_v_styles;
      Alcotest.test_case "spread" `Quick test_spread;
      Alcotest.test_case "stacked largest remainder" `Quick
        test_stacked_largest_remainder;
      Alcotest.test_case "stacked sums to width" `Quick
        test_stacked_sums_to_width;
      Alcotest.test_case "stacked empty" `Quick test_stacked_empty;
      Alcotest.test_case "span invalid" `Quick test_span_invalid;
    ] )
