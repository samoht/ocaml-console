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

(* [bar] printed on a formatter in [theme]. *)
let in_theme ?(utf_8 = true) theme bar =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Fmt.set_utf_8 ppf utf_8;
  Bar.pp ppf bar;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* theme.mli, bar: a theme gives its bars their glyphs. [dos] fills whole
   cells with full blocks on a light shade, in brackets, [ascii] draws [#]
   on [-] even in UTF-8, and the others heavy lines. *)
let test_theme_glyphs () =
  let check msg want got = Alcotest.(check string) msg want got in
  let half = Bar.v ~width:4 0.5 in
  check "dos" "\u{2588}\u{2588}\u{2591}\u{2591}" (in_theme Theme.dos half);
  check "dos brackets" "[\u{2588}\u{2591}\u{2591}]"
    (in_theme Theme.dos (Bar.v ~brackets:true ~width:3 0.4));
  check "dos in ASCII" "##--" (in_theme ~utf_8:false Theme.dos half);
  check "ascii in UTF-8" "##--" (in_theme Theme.ascii half);
  check "ascii brackets" "[#--]"
    (in_theme Theme.ascii (Bar.v ~brackets:true ~width:3 0.4));
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
    Bar.pp ppf bar;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let palette = Array.of_list (Theme.palette Theme.rainbow) in
  let muted = Style.fg (Theme.muted Theme.rainbow) in
  let cells n =
    List.init n (fun i -> Span.styled (Style.fg palette.(i)) "\u{2501}")
  in
  let want spans = Span.to_string ~styled:true (Span.concat spans) in
  Alcotest.(check string)
    "full"
    (want (cells 6))
    (styled Theme.rainbow (Bar.v ~width:6 1.));
  Alcotest.(check string)
    "half"
    (want (cells 3 @ [ Span.styled muted "\u{257a}\u{2501}\u{2501}" ]))
    (styled Theme.rainbow (Bar.v ~width:6 0.5));
  Alcotest.(check string)
    "a style given"
    (want [ Span.styled red "\u{2501}\u{2501}" ])
    (styled Theme.rainbow (Bar.v ~style:red ~width:2 1.));
  Alcotest.(check string)
    "one colour"
    (want
       [ Span.styled (Style.fg (Theme.accent Theme.nord)) "\u{2501}\u{2501}" ])
    (styled Theme.nord (Bar.v ~width:2 1.));
  Alcotest.(check string)
    "a palette of one colour gives that colour"
    (want [ Span.styled (Style.fg Color.red) "\u{2501}\u{2501}" ])
    (styled
       (Theme.v ~palette:[ Color.red ] ~accent:Color.blue ())
       (Bar.v ~width:2 1.));
  Alcotest.(check string)
    "plain" "\u{2501}\u{2501}\u{2501}\u{257a}\u{2501}\u{2501}"
    (in_theme Theme.rainbow (Bar.v ~width:6 0.5))

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
      Alcotest.test_case "glyphs of the theme" `Quick test_theme_glyphs;
      Alcotest.test_case "a palette along the bar" `Quick test_palette;
      Alcotest.test_case "span invalid" `Quick test_span_invalid;
    ] )
