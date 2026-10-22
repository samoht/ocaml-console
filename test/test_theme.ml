(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let printed ?theme ?(styled = false) ?(utf_8 = true) ?(margin = 40) pp v =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Option.iter (Theme.set ppf) theme;
  Fmt.set_utf_8 ppf utf_8;
  if styled then begin
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color
  end;
  Format.pp_set_margin ppf margin;
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The DOS preset draws as Norton Commander did in code page 437: frames of
   double lines with single lines inside them, tree guides of single lines,
   blocky bars and the ASCII spinner; ASCII alone where UTF-8 is not
   shown. *)
let test_dos () =
  let t = Theme.dos in
  Alcotest.(check string)
    "ascii spinner" "/"
    (Spinner.frame (Theme.spinner t) ~elapsed:0.1);
  Alcotest.(check bool) "blocky bars" true (Theme.bar t = `Block);
  let table =
    Table.v
      [ Table.column (Span.text "a"); Table.column (Span.text "b") ]
      [ [ Span.text "1"; Span.text "2" ] ]
  in
  let tree = Tree.v (Span.text "r") [ Tree.v (Span.text "c") [] ] in
  Alcotest.(check string)
    "double-line frame, single-line inside"
    "\u{2554}\u{2550}\u{2550}\u{2550}\u{2564}\u{2550}\u{2550}\u{2550}\u{2557}\n\
     \u{2551} a \u{2502} b \u{2551}\n\
     \u{255f}\u{2500}\u{2500}\u{2500}\u{253c}\u{2500}\u{2500}\u{2500}\u{2562}\n\
     \u{2551} 1 \u{2502} 2 \u{2551}\n\
     \u{255a}\u{2550}\u{2550}\u{2550}\u{2567}\u{2550}\u{2550}\u{2550}\u{255d}"
    (printed ~theme:t Table.pp table);
  Alcotest.(check string)
    "double-line panel"
    "\u{2554}\u{2550}\u{2550}\u{2550}\u{2557}\n\
     \u{2551} 1 \u{2551}\n\
     \u{255a}\u{2550}\u{2550}\u{2550}\u{255d}"
    (printed ~theme:t Panel.pp (Panel.v (Span.text "1")));
  Alcotest.(check string)
    "single-line tree guides" "r\n\u{2514}\u{2500}\u{2500} c"
    (printed ~theme:t Tree.pp tree);
  Alcotest.(check string)
    "ASCII table" "+---+---+\n| a | b |\n+---+---+\n| 1 | 2 |\n+---+---+"
    (printed ~theme:t ~utf_8:false Table.pp table);
  Alcotest.(check string)
    "ASCII tree" "r\n+-- c"
    (printed ~theme:t ~utf_8:false Tree.pp tree)

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
    (Table.v [ Table.column (Span.text "a") ] [ [ Span.text "1" ] ])
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
    "rule in the muted colour"
    (Span.to_string ~styled:true
       (Span.styled (Style.fg (Theme.muted Theme.dos)) "-----"))
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
    (contains "[+] done" (Buffer.contents buf));
  let term = terminal ~theme:Theme.dos () in
  let task = Display.task term.display "copy" in
  Display.set_count task ~cur:1 ~total:2;
  Display.redraw term.display;
  Alcotest.(check bool)
    "dos bars" true
    (contains "\u{2588}\u{2591}" (written term))

(* The progress rows of a theme whose palette has several colours take
   their bars in the whole palette, from its first colour to its last. *)
let test_display_palette () =
  let term = terminal ~theme:Theme.rainbow ~styled:true () in
  let task = Display.task term.display "copy" in
  Display.set_count task ~cur:1 ~total:1;
  Display.redraw term.display;
  let written = written term in
  List.iter
    (fun c ->
      let sgr =
        Span.to_string ~styled:true (Span.styled (Style.fg c) "\u{2501}")
      in
      let sgr = String.sub sgr 0 (String.index sgr 'm' + 1) ^ "\u{2501}" in
      Alcotest.(check bool)
        (Fmt.str "%a" Color.pp c) true (contains sgr written))
    [ List.hd (Theme.palette Theme.rainbow); Theme.accent Theme.rainbow ]

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

(* Text a theme colours reads on its background at 4.5:1 or more, and a
   glyph it colours alone at 3:1 or more, as WCAG 2.1 asks of text and of
   graphics. The muted colour and the header's are text: counts, timers and
   headings; so are the colours of the roles, which [Span.role] gives text,
   on a theme that names its background. A theme without one is read on
   black and on white alike, where 4.5:1 on both asks a luminance of 0.175
   to 0.183, which no amber of the 256-colour palette has: its roles are
   held to 3:1 on both, as glyphs. *)
let test_contrast () =
  List.iter
    (fun (name, t) ->
      let backgrounds =
        match Theme.background t with
        | Some bg -> [ bg ]
        | None -> [ Color.rgb 0 0 0; Color.rgb 255 255 255 ]
      in
      let roles =
        [
          ("accent", Theme.accent t);
          ("ok", Theme.ok t);
          ("warn", Theme.warn t);
          ("fail", Theme.fail t);
        ]
      in
      let palette =
        List.mapi (fun i c -> (Fmt.str "palette %d" i, c)) (Theme.palette t)
      in
      let text =
        ("muted", Theme.muted t)
        :: Option.to_list
             (Option.map
                (fun c -> ("header", c))
                (Style.foreground (Theme.header t)))
      in
      let glyphs, text =
        match Theme.background t with
        | Some _ -> (palette, roles @ text)
        | None -> (roles @ palette, text)
      in
      List.iter
        (fun bg ->
          List.iter
            (fun (least, colours) ->
              List.iter
                (fun (what, c) ->
                  let ratio = contrast c bg in
                  if ratio < least then
                    Alcotest.failf "%s: %s %a on %a is %.2f:1, below %.1f:1"
                      name what Color.pp c Color.pp bg ratio least)
                colours)
            [ (3., glyphs); (4.5, text) ])
        backgrounds)
    presets

(* Every theme sets its own headers apart. *)
let test_header () =
  List.iter
    (fun (name, t) ->
      if Style.is_none (Theme.header t) then
        Alcotest.failf "%s: plain headers" name)
    presets

(* The escape code that selects [style], as a span in it starts. *)
let sgr style =
  let s = Span.to_string ~styled:true (Span.styled style "x") in
  String.sub s 0 (String.index s 'x')

(* Frames, guides, rules, the track of a bar and the counts and timers of a
   display are in the theme's muted colour; table headers, panel titles and
   the display's header in its header style. *)
let test_roles () =
  let t = Theme.nord in
  let muted = sgr (Style.fg (Theme.muted t))
  and header = sgr (Theme.header t) in
  let print pp v = printed ~theme:t ~styled:true pp v in
  let check what affix s =
    if not (contains affix s) then Alcotest.failf "%s: no %S in %S" what affix s
  in
  let table =
    print Table.pp
      (Table.v [ Table.column (Span.text "a") ] [ [ Span.text "1" ] ])
  in
  check "table frame" (muted ^ "\u{256d}") table;
  check "table header" (header ^ "a") table;
  let panel = print Panel.pp (Panel.v ~title:(Span.text "T") (Span.text "1")) in
  check "panel frame" (muted ^ "\u{256d}") panel;
  check "panel title" (header ^ "T") panel;
  check "tree guide" (muted ^ "\u{2514}")
    (print Tree.pp (Tree.v (Span.text "r") [ Tree.v (Span.text "c") [] ]));
  check "rule" (muted ^ "\u{2500}") (print Rule.pp (Rule.v ()));
  check "bar track" muted (print Bar.pp (Bar.v ~width:4 0.5));
  let term = terminal ~theme:t ~styled:true ~header:"Build" () in
  let task = Display.task term.display "compile" in
  Display.set_count task ~cur:1 ~total:3;
  Display.redraw term.display;
  let out = written term in
  check "display count" (muted ^ "1/3") out;
  check "display timer" (muted ^ "0.0s") out;
  check "display header" (header ^ "Build") out

(* A command line chooses a theme by name: [all] holds every predefined
   theme once, under the name [of_string] reads back. *)
let test_names () =
  Alcotest.(check (list string))
    "names"
    [
      "default";
      "ascii";
      "dos";
      "matrix";
      "rainbow";
      "dracula";
      "nord";
      "solarized-dark";
      "solarized-light";
      "gruvbox-dark";
      "gruvbox-light";
      "catppuccin-latte";
      "catppuccin-mocha";
      "tokyo-night";
      "one-dark";
      "monokai";
      "github-dark";
      "github-light";
    ]
    (List.map fst Theme.all);
  List.iter
    (fun (name, t) ->
      Alcotest.(check string) "name" name (Theme.name t);
      Alcotest.(check bool)
        ("of_string " ^ name) true
        (match Theme.of_string name with Ok t' -> t' == t | Error _ -> false))
    Theme.all;
  Alcotest.(check bool)
    "presets" true
    (List.for_all2 (fun (_, t) (_, t') -> t == t') presets Theme.all);
  (match Theme.of_string "solarized" with
  | Ok _ -> Alcotest.fail "unknown name accepted"
  | Error (`Msg m) ->
      Alcotest.(check bool)
        "names the choices" true
        (contains "solarized-dark" m));
  Alcotest.(check string) "custom" "custom" (Theme.name (Theme.v ()));
  Alcotest.(check string) "named" "mine" (Theme.name (Theme.v ~name:"mine" ()))

let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

(* A display on a terminal that is not UTF-8, with a spinner, a bar, a
   nested row, a log line, a label cut to fit and rows hidden behind a
   marker. *)
let display_bytes theme =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Fmt.set_utf_8 ppf false;
  let d =
    Display.v ~header:"Build" ~size:(fun () -> (24, 6)) ~now:(fun () -> 0.3) ppf
  in
  let parent = Display.task d "a label much wider than the terminal" in
  let child = Display.task ~parent d "child" in
  Display.set_count child ~cur:1 ~total:3;
  Display.log parent "careful";
  Driver.report d `Debug "debug";
  List.iter (fun l -> ignore (Display.task d l)) [ "b"; "c"; "d"; "e" ];
  Display.succeed child;
  Display.close d;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* A formatter that is not UTF-8 gets ASCII alone from every widget, in
   every theme, a line cut to fit included. *)
let test_ascii_fallback () =
  let long = Span.text "a label much wider than the margin" in
  let widgets theme =
    let print pp v = printed ~theme ~utf_8:false ~margin:12 pp v in
    [
      ( "table",
        print Table.pp
          (Table.v
             [
               Table.column (Span.text "a");
               Table.column ~overflow:`Truncate (Span.text "b");
             ]
             [ [ Span.text "1"; long ] ]) );
      ("panel", print Panel.pp (Panel.v ~title:long long));
      ( "tree",
        print Tree.pp
          (Tree.v long [ Tree.v long [ Tree.v long [] ]; Tree.v long [] ]) );
      ("rule", print Rule.pp (Rule.v ~label:long ()));
      ("bar", print Bar.pp (Bar.v ~brackets:true ~width:10 0.55));
      ("display", display_bytes theme);
    ]
  in
  List.iter
    (fun (name, theme) ->
      List.iter
        (fun (what, s) ->
          if not (is_ascii s) then Alcotest.failf "%s: %s: %S" name what s)
        (widgets theme))
    presets;
  Alcotest.(check string)
    "a cut line ends with dots" "a label ..."
    (printed ~utf_8:false ~margin:11 Rule.pp
       (Rule.v ~align:`Left ~label:long ()))

(* {1 Motion} *)

(* matrix's frames rain, rainbow's sweep its colours round and the themes of
   colour schemes shimmer in their main colour; the others stand still. *)
let test_motion () =
  List.iter
    (fun (name, t) ->
      let want =
        match name with
        | "default" | "ascii" | "dos" -> None
        | "matrix" -> Some `Rain
        | "rainbow" -> Some `Sweep
        | _ -> Some `Shimmer
      in
      if Theme.motion t <> want then Alcotest.failf "%s: the wrong motion" name)
    presets;
  Alcotest.(check bool) "v: none" true (Theme.motion (Theme.v ()) = None);
  Alcotest.(check bool)
    "v: the one given" true
    (Theme.motion (Theme.v ~motion:`Sweep ()) = Some `Sweep)

(* [still] keeps a theme's look and stops what moves: its frames and its
   spinner. *)
let test_still () =
  let t = Theme.still Theme.matrix in
  Alcotest.(check bool) "no motion" true (Theme.motion t = None);
  Alcotest.(check string) "its name" "matrix" (Theme.name t);
  Alcotest.(check bool)
    "its colours" true
    (Color.equal (Theme.accent t) (Theme.accent Theme.matrix));
  let frame elapsed = Spinner.frame (Theme.spinner t) ~elapsed in
  Alcotest.(check (list string))
    "a spinner that does not turn"
    [ frame 0.; frame 0. ]
    [ frame 0.1; frame 0.55 ]

(* Printed on a formatter, a frame is motionless: its glyphs are those of
   the theme without motion, and so are its colours, but for a sweep, which
   colours the whole frame and prints the palette graded round it, as a
   display draws it at its start. *)
let test_still_printed () =
  let table =
    Table.v
      [ Table.column (Span.text "a"); Table.column (Span.text "b") ]
      [ [ Span.text "1"; Span.text "2" ] ]
  in
  let panel = Panel.v ~title:(Span.text "T") (Span.text "body") in
  let colours s =
    let re =
      Re.compile
        (Re.seq [ Re.str "\027[38;"; Re.rep1 (Re.rg '0' ';'); Re.char 'm' ])
    in
    List.sort_uniq compare (Re.matches re s)
  in
  List.iter
    (fun (name, t) ->
      let still = Theme.still t in
      let check what pp v =
        Alcotest.(check string)
          (name ^ " " ^ what ^ ", plain")
          (printed ~theme:still pp v)
          (printed ~theme:t pp v);
        let moving = printed ~theme:t ~styled:true pp v in
        Alcotest.(check string)
          (name ^ " " ^ what ^ ", motionless")
          moving
          (printed ~theme:t ~styled:true pp v);
        match Theme.motion t with
        | Some `Sweep ->
            let first = List.hd (Theme.palette t) in
            if not (String.starts_with ~prefix:(sgr (Style.fg first)) moving)
            then Alcotest.failf "%s %s: %S" name what moving;
            if List.length (colours moving) < 6 then
              Alcotest.failf "%s %s: a frame of %d colours" name what
                (List.length (colours moving))
        | _ ->
            Alcotest.(check string)
              (name ^ " " ^ what)
              (printed ~theme:still ~styled:true pp v)
              moving
      in
      check "table" Table.pp table;
      check "panel" Panel.pp panel)
    presets

(* The cells of the frame of a table drawn in a live display in [theme], as
   (glyph, colour) pairs, at each of [times] seconds. *)
let frames theme times =
  let term = terminal ~theme ~styled:true ~width:40 ~height:12 () in
  let table =
    Table.v
      [
        Table.column (Span.text "step");
        Table.column ~align:`Right (Span.text "time");
      ]
      [ [ Span.text "fetch"; Span.text "1.2s" ] ]
  in
  Display.set_scene term.display (fun ~height:_ ppf -> Table.pp ppf table);
  let now = ref 0. in
  List.map
    (fun time ->
      term.advance (time -. !now);
      now := time;
      Display.redraw term.display;
      let rows = screen term in
      let height = 5 and width = Width.string_width (List.hd rows) in
      let grid = Console_vte.grid term.vte in
      let cell x y =
        let i = Grid.idx grid ~x ~y in
        let r, g, b = Ansi.Color.to_rgb (Console_vte.fg grid i) in
        (Grid.get_text grid i, Color.rgb r g b)
      in
      List.concat
        (List.init height (fun y ->
             if y = 0 || y = height - 1 then List.init width (fun x -> cell x y)
             else [ cell 0 y; cell (width - 1) y ])))
    times

let same_frame f0 f1 =
  List.for_all2 (fun (g0, c0) (g1, c1) -> g0 = g1 && Color.equal c0 c1) f0 f1

(* Every frame of a theme with motion is drawn in colours at 3:1 or more on
   its background, as a glyph is, and the frames change; a theme without
   motion draws the same frame at every time. *)
let test_frame_contrast () =
  let times = List.init 40 (fun i -> float_of_int i *. 0.1) in
  List.iter
    (fun (name, t) ->
      let frames = frames t times in
      let first = List.hd frames in
      let moves = List.exists (fun f -> not (same_frame first f)) frames in
      if moves <> Option.is_some (Theme.motion t) then
        Alcotest.failf "%s: %s" name
          (if moves then "a still theme moves" else "its frame does not move");
      let backgrounds =
        match Theme.background t with
        | Some bg -> [ bg ]
        | None -> [ Color.rgb 0 0 0; Color.rgb 255 255 255 ]
      in
      List.iter
        (List.iter (fun (glyph, c) ->
             List.iter
               (fun bg ->
                 let ratio = contrast c bg in
                 if ratio < 3. then
                   Alcotest.failf "%s: %S in %a on %a is %.2f:1" name glyph
                     Color.pp c Color.pp bg ratio)
               backgrounds))
        frames)
    presets

(* Rain runs along an intact frame: in every frame the corners and the tees,
   where lines meet, keep their glyphs, a trail of three cells at most shows
   other glyphs, and behind it the frame's own lines fade from the main
   colour. *)
let test_rain_intact () =
  let times = List.init 60 (fun i -> float_of_int i *. 0.07) in
  let still = List.hd (frames (Theme.still Theme.matrix) [ 0. ]) in
  List.iteri
    (fun i frame ->
      let swapped = ref 0 and faded = ref 0 in
      List.iter2
        (fun (g0, c0) (g, c) ->
          if g <> g0 then begin
            if not (List.mem g0 [ "\u{2500}"; "\u{2502}" ]) then
              Alcotest.failf "frame %d: %S, where lines meet, became %S" i g0 g;
            incr swapped
          end
          else if not (Color.equal c c0) then incr faded)
        still frame;
      if !swapped > 3 then
        Alcotest.failf "frame %d: %d glyphs swapped, at most 3" i !swapped;
      if !faded < 4 then
        Alcotest.failf "frame %d: a trail of %d line glyphs fading" i !faded)
    (frames Theme.matrix times)

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
      Alcotest.test_case "a display's bars in the palette" `Quick
        test_display_palette;
      Alcotest.test_case "ASCII when not UTF-8" `Quick test_ascii_fallback;
      Alcotest.test_case "names" `Quick test_names;
      Alcotest.test_case "header" `Quick test_header;
      Alcotest.test_case "roles" `Quick test_roles;
      Alcotest.test_case "contrast" `Quick test_contrast;
      Alcotest.test_case "motion" `Quick test_motion;
      Alcotest.test_case "still" `Quick test_still;
      Alcotest.test_case "printed still" `Quick test_still_printed;
      Alcotest.test_case "every frame in contrast" `Quick test_frame_contrast;
      Alcotest.test_case "rain on an intact frame" `Quick test_rain_intact;
    ] )
