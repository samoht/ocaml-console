(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* {1 Bars} *)

let plain = View.to_string
let ansi = View.to_string ~styled:true
let red = Style.fg Color.red
let dim = Style.faint
let check_plain msg want span = Alcotest.(check string) msg want (plain span)

let check_ansi msg want span =
  Alcotest.(check string)
    msg
    (View.to_string ~styled:true (View.concat want))
    (ansi span)

(* bar.mli, v: a cell is a heavy line, filled or track, and a half filled
   cell the left half of one. *)
let test_v_halves () =
  check_plain "empty" "━" (Bar.horizontal ~width:1 0.);
  check_plain "half" "╸" (Bar.horizontal ~width:1 0.5);
  check_plain "full" "━" (Bar.horizontal ~width:1 1.)

(* bar.mli, v: the fill rounds to the nearest half cell, and a track after a
   whole cell starts with the right half of one, which leaves a gap. *)
let test_v_rounding () =
  check_plain "half of three cells" "━╸━" (Bar.horizontal ~width:3 0.5);
  check_plain "half of four cells" "━━╺━" (Bar.horizontal ~width:4 0.5);
  check_plain "a quarter of one cell is half of it" "╸"
    (Bar.horizontal ~width:1 0.25);
  check_plain "below a quarter is none" "━━" (Bar.horizontal ~width:2 0.12);
  check_plain "19.8 halves is 20" "━━━━━━━━━━" (Bar.horizontal ~width:10 0.99)

(* bar.mli, v: the fraction is clamped to 0..1 and NaN is 0. *)
let test_v_clamped () =
  check_plain "negative" "━━━" (Bar.horizontal ~width:3 (-1.));
  check_plain "over one" "━━━" (Bar.horizontal ~width:3 2.);
  check_plain "infinity" "━━━" (Bar.horizontal ~width:3 Float.infinity);
  check_plain "nan" "━━━" (Bar.horizontal ~width:3 Float.nan);
  check_plain "zero width" "" (Bar.horizontal ~width:0 0.5)

(* bar.mli, spans: the fill carries [style], the track and the brackets
   [track], and a part with no cell adds no style. *)
let test_v_styles () =
  check_ansi "fill and track"
    [ View.styled red (View.text "━╸"); View.styled dim (View.text "━") ]
    (Bar.horizontal ~style:red ~track:dim ~width:3 0.5);
  check_ansi "a gap before the track"
    [ View.styled red (View.text "━"); View.styled dim (View.text "╺━") ]
    (Bar.horizontal ~style:red ~track:dim ~width:3 0.34);
  check_ansi "full bar, no track"
    [ View.styled red (View.text "━━") ]
    (Bar.horizontal ~style:red ~track:dim ~width:2 1.);
  check_ansi "empty bar, no fill"
    [ View.styled dim (View.text "━━") ]
    (Bar.horizontal ~style:red ~track:dim ~width:2 0.);
  check_plain "brackets" "▕━╸━▏" (Bar.horizontal ~brackets:true ~width:3 0.5);
  check_ansi "brackets in the track style"
    [
      View.styled dim (View.text "▕");
      View.styled red (View.text "━╸");
      View.styled dim (View.text "━");
      View.styled dim (View.text "▏");
    ]
    (Bar.horizontal ~style:red ~track:dim ~brackets:true ~width:3 0.5)

(* bar.mli: on a formatter that is not UTF-8, whole cells of [#] on a track
   of [-]. *)
let test_ascii () =
  let ascii bar =
    let buf = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_utf_8 ppf false;
    View.pp ppf bar;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Alcotest.(check string) "half" "##--" (ascii (Bar.horizontal ~width:4 0.5));
  Alcotest.(check string)
    "brackets" "[#--]"
    (ascii (Bar.horizontal ~brackets:true ~width:3 0.4))

(* [bar] printed on a formatter in [theme]. *)
let in_theme ?(utf_8 = true) theme bar =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Fmt.set_utf_8 ppf utf_8;
  View.pp ppf bar;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* theme.mli, bar: a theme gives its bars their glyphs. [dos] fills whole
   cells with full blocks on a light shade, in brackets, [ascii] draws [#]
   on [-] even in UTF-8, and the others heavy lines. *)
let test_theme_glyphs () =
  let check msg want got = Alcotest.(check string) msg want got in
  let half = Bar.horizontal ~width:4 0.5 in
  check "dos" "\u{2588}\u{2588}\u{2591}\u{2591}" (in_theme Theme.dos half);
  check "dos brackets" "[\u{2588}\u{2591}\u{2591}]"
    (in_theme Theme.dos (Bar.horizontal ~brackets:true ~width:3 0.4));
  check "dos in ASCII" "##--" (in_theme ~utf_8:false Theme.dos half);
  check "ascii in UTF-8" "##--" (in_theme Theme.ascii half);
  check "ascii brackets" "[#--]"
    (in_theme Theme.ascii (Bar.horizontal ~brackets:true ~width:3 0.4));
  check "default" "\u{2501}\u{2501}\u{257a}\u{2501}"
    (in_theme Theme.default half);
  check "nord" "\u{2501}\u{2501}\u{257a}\u{2501}" (in_theme Theme.nord half);
  Alcotest.(check bool)
    "Theme.bar" true
    (Theme.bar Theme.dos = `Block
    && Theme.bar Theme.ascii = `Ascii
    && Theme.bar Theme.default = `Line
    && Theme.bar (Theme.v ~bar:`Block ()) = `Block)

(* theme.mli, palette: a bar given no style spreads a palette of several
   colours along its length, each cell in the colour of its place, so the
   fill uncovers the colours as it grows; a bar given a style is in it, and
   one in a palette of one colour is in that colour, as a progress row's bar
   is, whatever the main colour. *)
let test_palette () =
  let styled theme bar =
    let buf = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color;
    View.pp ppf bar;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let palette = Array.of_list (Theme.palette Theme.rainbow) in
  let muted = Style.fg (Theme.muted Theme.rainbow) in
  let cells n =
    List.init n (fun i ->
        View.styled (Style.fg palette.(i)) (View.text "\u{2501}"))
  in
  let want spans = View.to_string ~styled:true (View.concat spans) in
  Alcotest.(check string)
    "full"
    (want (cells 6))
    (styled Theme.rainbow (Bar.horizontal ~width:6 1.));
  Alcotest.(check string)
    "half"
    (want
       (cells 3 @ [ View.styled muted (View.text "\u{257a}\u{2501}\u{2501}") ]))
    (styled Theme.rainbow (Bar.horizontal ~width:6 0.5));
  Alcotest.(check string)
    "a style given"
    (want [ View.styled red (View.text "\u{2501}\u{2501}") ])
    (styled Theme.rainbow (Bar.horizontal ~style:red ~width:2 1.));
  Alcotest.(check string)
    "one colour"
    (want
       [
         View.styled
           (Style.fg (Theme.accent Theme.nord))
           (View.text "\u{2501}\u{2501}");
       ])
    (styled Theme.nord (Bar.horizontal ~width:2 1.));
  Alcotest.(check string)
    "a palette of one colour gives that colour"
    (want [ View.styled (Style.fg Color.red) (View.text "\u{2501}\u{2501}") ])
    (styled
       (Theme.v ~palette:[ Color.red ] ~accent:Color.blue ())
       (Bar.horizontal ~width:2 1.));
  Alcotest.(check string)
    "plain" "\u{2501}\u{2501}\u{2501}\u{257a}\u{2501}\u{2501}"
    (in_theme Theme.rainbow (Bar.horizontal ~width:6 0.5))

let test_span_invalid () =
  Alcotest.check_raises "v width"
    (Invalid_argument "Console.Bar.horizontal: negative width") (fun () ->
      ignore (Bar.horizontal ~width:(-1) 0.5));
  ()

(* {1 Vertical bars} *)

let column ?(height = 1) ?theme ?utf_8 frac =
  Display_harness.at ?theme ?utf_8 ~time:0.
    (View.sized ~height (Bar.vertical frac))

(* A vertical bar fills from the bottom, an eighth of a cell at a time. *)
let test_vertical_fill () =
  let one frac = column frac in
  Alcotest.(check (list string))
    "one row"
    [ " "; "▁"; "▄"; "█"; " "; " "; "█" ]
    [ one 0.; one 0.125; one 0.5; one 1.; one Float.nan; one (-1.); one 2. ];
  Alcotest.(check string) "rounded to the eighth" "▂" (one 0.2);
  Alcotest.(check string)
    "three rows, half full" " \n▄\n█" (column ~height:3 0.5);
  Alcotest.(check string) "three rows, full" "█\n█\n█" (column ~height:3 1.);
  Alcotest.(check string)
    "an eighth of three rows" " \n \n▃" (column ~height:3 0.125);
  Alcotest.(check string)
    "one row at its natural size" "▄"
    (View.to_string (Bar.vertical 0.5))

let test_vertical_ascii () =
  Alcotest.(check (list string))
    "off UTF-8"
    [ " "; "."; ":"; "#"; "#" ]
    (List.map (column ~utf_8:false) [ 0.; 0.25; 0.5; 0.9; 1. ]);
  Alcotest.(check string) "an ASCII theme" ":" (column ~theme:Theme.ascii 0.6)

(* Vertical bars side by side are a sparkline, or a histogram in a room. *)
let test_vertical_charts () =
  let values = [ 0.; 0.25; 0.5; 0.75; 1. ] in
  Alcotest.(check string)
    "sparkline" " ▂▄▆█"
    (View.to_string (View.hcat ~gap:0 (List.map Bar.vertical values)));
  Alcotest.(check string)
    "histogram" "      ▂ █\n    ▄ █ █\n  ▆ █ █ █"
    (Display_harness.at ~time:0.
       (View.sized ~height:3 (View.hcat ~gap:1 (List.map Bar.vertical values))))

let test_vertical_palette () =
  let theme = Theme.v ~palette:[ Color.red; Color.blue ] () in
  let s =
    Display_harness.at ~theme ~time:0. (View.sized ~height:2 (Bar.vertical 1.))
  in
  Alcotest.(check string) "plain" "█\n█" s

(* A sparkline spans its values, its lowest at an eighth. *)
let test_sparkline () =
  Alcotest.(check string)
    "from the lowest to the highest" "▁▃▆█"
    (View.to_string (Bar.sparkline [ 5.; 6.; 7.; 8. ]));
  Alcotest.(check string)
    "from a floor" " ▄█"
    (View.to_string (Bar.sparkline ~min:0. ~max:2. [ 0.; 1.; 2. ]));
  Alcotest.(check string)
    "all equal" "▁▁"
    (View.to_string (Bar.sparkline [ 3.; 3. ]))

(* A chart's bars are as wide as their labels, their values above them and
   their labels under a baseline. *)
let test_chart () =
  Alcotest.(check string)
    "values, bars, baseline and labels"
    " 2   4 \n    ███\n███ ███\n───────\nMon Tue"
    (View.to_string (Bar.chart ~height:2 [ ("Mon", 2.); ("Tue", 4.) ]));
  Alcotest.(check string)
    "off UTF-8" " 2   4 \n    ###\n### ###\n-------\nMon Tue"
    (Display_harness.at ~utf_8:false ~time:0.
       (Bar.chart ~height:2 [ ("Mon", 2.); ("Tue", 4.) ]))

(* A vertical bar of no width grows to its room, as a column does. *)
let test_vertical_width () =
  Alcotest.(check string)
    "fixed" "▄▄▄"
    (View.to_string (Bar.vertical ~width:3 0.5));
  Alcotest.(check string)
    "its room" "▄▄▄▄▄"
    (Display_harness.at ~width:5 ~time:0. (Bar.vertical 0.5));
  Alcotest.(check string)
    "shared in a row" "▄▄ ██"
    (Display_harness.at ~width:5 ~time:0.
       (View.hcat ~gap:1 [ Bar.vertical 0.5; Bar.vertical 1. ]))

let suite =
  ( "bar",
    [
      Alcotest.test_case "sparkline" `Quick test_sparkline;
      Alcotest.test_case "chart" `Quick test_chart;
      Alcotest.test_case "vertical width" `Quick test_vertical_width;
      Alcotest.test_case "vertical fill" `Quick test_vertical_fill;
      Alcotest.test_case "vertical ASCII" `Quick test_vertical_ascii;
      Alcotest.test_case "sparkline and histogram" `Quick test_vertical_charts;
      Alcotest.test_case "vertical palette" `Quick test_vertical_palette;
      Alcotest.test_case "v halves" `Quick test_v_halves;
      Alcotest.test_case "v rounding" `Quick test_v_rounding;
      Alcotest.test_case "v clamped" `Quick test_v_clamped;
      Alcotest.test_case "v styles" `Quick test_v_styles;
      Alcotest.test_case "ASCII" `Quick test_ascii;
      Alcotest.test_case "glyphs of the theme" `Quick test_theme_glyphs;
      Alcotest.test_case "a palette along the bar" `Quick test_palette;
      Alcotest.test_case "span invalid" `Quick test_span_invalid;
    ] )
