(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

(* The DOS preset is ASCII through and through: a blocky bar, the ASCII spinner
   and tree guide. *)
let test_dos () =
  let t = Theme.dos in
  Alcotest.(check string)
    "ascii spinner" "/"
    (Spinner.frame (Theme.spinner t) ~elapsed:0.1);
  Alcotest.(check string)
    "ascii guide branch" "+-- "
    (Guide.branch (Theme.guide t))

let test_default () =
  let t = Theme.default in
  Alcotest.(check string)
    "braille spinner" "⠙"
    (Spinner.frame (Theme.spinner t) ~elapsed:0.1)

let test_rainbow () =
  let t = Theme.rainbow in
  Alcotest.(check int)
    "palette of six colours" 6
    (List.length (Theme.palette t))

(* [v] is the default theme with the parts it is given replaced, and its
   palette follows its accent. *)
let test_v_defaults () =
  let t = Theme.v ~accent:Color.red ~spinner:Spinner.ascii () in
  Alcotest.(check string) "default guide" "├── " (Guide.branch (Theme.guide t));
  Alcotest.(check bool)
    "palette is the accent" true
    (match Theme.palette t with [ c ] -> Color.equal c Color.red | _ -> false);
  Alcotest.(check string)
    "the spinner given" "/"
    (Spinner.frame (Theme.spinner t) ~elapsed:0.1)

(* Each scheme keeps the background its authors chose. *)
let test_schemes () =
  List.iter
    (fun (name, t, bg) ->
      Alcotest.(check (option string))
        name (Some bg)
        (Option.map (Fmt.str "%a" Color.pp) (Theme.background t)))
    [
      ("dracula", Theme.dracula, "#282a36");
      ("nord", Theme.nord, "#2e3440");
      ("solarized-dark", Theme.solarized_dark, "#002b36");
      ("solarized-light", Theme.solarized_light, "#fdf6e3");
      ("gruvbox-dark", Theme.gruvbox_dark, "#282828");
      ("gruvbox-light", Theme.gruvbox_light, "#fbf1c7");
      ("catppuccin-latte", Theme.catppuccin_latte, "#eff1f5");
      ("catppuccin-mocha", Theme.catppuccin_mocha, "#1e1e2e");
      ("tokyo-night", Theme.tokyo_night, "#1a1b26");
      ("one-dark", Theme.one_dark, "#282c34");
      ("monokai", Theme.monokai, "#272822");
      ("github-dark", Theme.github_dark, "#0d1117");
      ("github-light", Theme.github_light, "#ffffff");
    ]

(* A theme is kept on its formatter, as its colour depth is: a formatter
   given none has the default, and giving one to a formatter changes no
   other, nor what Fmt keeps on it. *)
let test_on_formatter () =
  let ppf = Format.formatter_of_buffer (Buffer.create 0) in
  let other = Format.formatter_of_buffer (Buffer.create 0) in
  let check what want ppf =
    Alcotest.(check bool) what true (Theme.get ppf == want)
  in
  check "none given" Theme.default ppf;
  Theme.set ppf Theme.dos;
  check "given" Theme.dos ppf;
  Fmt.set_style_renderer ppf `Ansi_tty;
  Theme.set ppf Theme.nord;
  check "given again" Theme.nord ppf;
  Alcotest.(check bool)
    "Fmt's renderer kept" true
    (Fmt.style_renderer ppf = `Ansi_tty);
  check "another formatter" Theme.default other

let printed ?theme ?(styled = false) ?(margin = 40) pp v =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Option.iter (Theme.set ppf) theme;
  if styled then Fmt.set_style_renderer ppf `Ansi_tty;
  Format.pp_set_margin ppf margin;
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* A widget built once takes the look of each formatter it is printed on. *)
let test_widgets_read_it () =
  let check what pp v ~ascii ~default =
    Alcotest.(check bool)
      (what ^ " in ascii") true
      (contains ascii (printed ~theme:Theme.ascii pp v));
    Alcotest.(check bool)
      (what ^ " by default") true
      (contains default (printed pp v))
  in
  check "table" Table.pp
    (Table.v [ Table.column "a" ] [ [ Span.text "1" ] ])
    ~ascii:"+---+" ~default:"\u{256d}\u{2500}";
  check "panel" Panel.pp
    (Panel.v (Span.text "1"))
    ~ascii:"+---+" ~default:"\u{256d}\u{2500}";
  check "tree" Tree.pp
    (Tree.v (Span.text "r") [ Tree.v (Span.text "c") [] ])
    ~ascii:"+-- c" ~default:"\u{2514}\u{2500}\u{2500} c";
  let amber s =
    Span.to_string ~styled:true
      (Span.styled (Style.fg (Theme.accent Theme.dos)) s)
  in
  Alcotest.(check string)
    "rule in the accent" (amber "-----")
    (printed ~theme:Theme.dos ~styled:true ~margin:5 Rule.pp
       (Rule.v ~glyph:"-" ()));
  Alcotest.(check bool)
    "bar in the accent" true
    (Astring.String.is_prefix
       ~affix:(String.sub (amber "x") 0 10)
       (printed ~theme:Theme.dos ~styled:true Bar.pp (Bar.v ~width:2 1.)))

(* A display takes the theme of the formatter it writes on. *)
let test_display_reads_it () =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf Theme.dos;
  let d = Display.v ~now:(fun () -> 0.) ppf in
  Display.succeed (Display.task d "done");
  Display.close d;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "dos marks" true
    (contains "[+] done" (Buffer.contents buf))

let test_pp () =
  let s = Fmt.str "%a" Theme.pp Theme.dos in
  Alcotest.(check bool) "mentions the done marker" true (contains "[+]" s)

(* WCAG 2.1 contrast ratio: (L1 + 0.05) / (L2 + 0.05) of the relative
   luminances, the lighter first. *)
let luminance c =
  let r, g, b = Color.to_rgb c in
  let lin v =
    let v = float_of_int v /. 255. in
    if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4
  in
  (0.2126 *. lin r) +. (0.7152 *. lin g) +. (0.0722 *. lin b)

let contrast c0 c1 =
  let l0 = luminance c0 and l1 = luminance c1 in
  (Float.max l0 l1 +. 0.05) /. (Float.min l0 l1 +. 0.05)

let presets =
  Theme.
    [
      ("default", default);
      ("ascii", ascii);
      ("dos", dos);
      ("matrix", matrix);
      ("rainbow", rainbow);
      ("dracula", dracula);
      ("nord", nord);
      ("solarized_dark", solarized_dark);
      ("solarized_light", solarized_light);
      ("gruvbox_dark", gruvbox_dark);
      ("gruvbox_light", gruvbox_light);
      ("catppuccin_latte", catppuccin_latte);
      ("catppuccin_mocha", catppuccin_mocha);
      ("tokyo_night", tokyo_night);
      ("one_dark", one_dark);
      ("monokai", monokai);
      ("github_dark", github_dark);
      ("github_light", github_light);
    ]

(* The text a theme colours stands out from its background at 3:1 or more;
   a theme without a background is read on black and on white alike. *)
let test_contrast () =
  List.iter
    (fun (name, t) ->
      let backgrounds =
        match Theme.background t with
        | Some bg -> [ bg ]
        | None -> [ Color.rgb 0 0 0; Color.rgb 255 255 255 ]
      in
      let colours =
        [
          ("accent", Theme.accent t);
          ("ok", Theme.ok t);
          ("warn", Theme.warn t);
          ("fail", Theme.fail t);
        ]
        @ List.mapi (fun i c -> (Fmt.str "palette %d" i, c)) (Theme.palette t)
      in
      List.iter
        (fun bg ->
          List.iter
            (fun (what, c) ->
              let ratio = contrast c bg in
              if ratio < 3. then
                Alcotest.failf "%s: %s %a on %a is %.2f:1" name what Color.pp c
                  Color.pp bg ratio)
            colours)
        backgrounds)
    presets

let suite =
  ( "theme",
    [
      Alcotest.test_case "dos preset" `Quick test_dos;
      Alcotest.test_case "default preset" `Quick test_default;
      Alcotest.test_case "rainbow preset" `Quick test_rainbow;
      Alcotest.test_case "v defaults" `Quick test_v_defaults;
      Alcotest.test_case "schemes" `Quick test_schemes;
      Alcotest.test_case "pp" `Quick test_pp;
      Alcotest.test_case "on the formatter" `Quick test_on_formatter;
      Alcotest.test_case "widgets read it" `Quick test_widgets_read_it;
      Alcotest.test_case "a display reads it" `Quick test_display_reads_it;
      Alcotest.test_case "contrast" `Quick test_contrast;
    ] )
