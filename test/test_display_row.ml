(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

(* Every log line and every finished row the display draws carries a marker,
   and the marker is the theme's. The default half pins the six default
   glyphs; the custom half is what proves they are read from the theme rather
   than recited beside it. *)
let markers_drawn ?theme () =
  let (), output =
    capture ?theme (fun display _ ->
        let row = Display.task display "note" in
        Display.log ~level:`Info row "an info line";
        Display.log ~level:`Warning row "a warning line";
        Display.log ~level:`Error row "an error line";
        Display.succeed row;
        Display.fail (Display.task display "broken");
        Display.cancel (Display.task display "abandoned"))
  in
  output

let test_markers_come_from_the_theme () =
  let defaults =
    [
      ("an info line", "# an info line");
      ("a warning line", "! a warning line");
      ("an error line", "x an error line");
      ("a succeeded row", "=> note");
      ("a failed row", "!! broken");
      ("a cancelled row", "-- abandoned");
    ]
  in
  let drawn = markers_drawn () in
  List.iter
    (fun (what, line) ->
      Alcotest.(check bool)
        (what ^ " keeps its glyph")
        true (contains line drawn))
    defaults;
  let theme =
    Theme.v ~note_marker:"(i)" ~warn_marker:"(w)" ~error_marker:"(e)"
      ~committed_succeeded:"(+)" ~committed_failed:"(-)"
      ~committed_cancelled:"(~)" ~spinner:Spinner.ascii ~done_marker:"[+]" ()
  in
  let drawn = markers_drawn ~theme () in
  List.iter
    (fun (what, line) ->
      Alcotest.(check bool)
        (what ^ " no longer draws the default")
        false (contains line drawn))
    defaults;
  List.iter
    (fun (what, line) ->
      Alcotest.(check bool)
        (what ^ " draws the theme's")
        true (contains line drawn))
    [
      ("an info line", "(i) an info line");
      ("a warning line", "(w) a warning line");
      ("an error line", "(e) an error line");
      ("a succeeded row", "(+) note");
      ("a failed row", "(-) broken");
      ("a cancelled row", "(~) abandoned");
    ]

let test_zero_progress_has_no_empty_bar () =
  ignore
    (terminal_run ~height:4 ~width:56 (fun display _advance sync vte ->
         let task = Display.task display "checking packages" in
         Display.set_count task ~cur:0 ~total:459;
         sync ();
         let row = List.hd (String.split_on_char '\n' (Vte.to_string vte)) in
         Alcotest.(check bool)
           "zero progress has no orphaned delimiters" false
           (contains "▕" row || contains "▏" row);
         Alcotest.(check bool)
           "count remains informative" true (contains "0/459" row);
         Display.set_count task ~cur:1 ~total:459;
         sync ();
         let row = List.hd (String.split_on_char '\n' (Vte.to_string vte)) in
         Alcotest.(check bool)
           "first completed item has visible fill" true (contains "▕▏" row)))

let test_committed_metadata_is_right_aligned () =
  let width = 48 in
  let vte =
    terminal_run ~height:8 ~width (fun display advance _sync _vte ->
        let finish ?count elapsed label =
          let task = Display.task display label in
          Option.iter
            (fun (cur, total) -> Display.set_count task ~cur ~total)
            count;
          advance elapsed;
          Display.succeed task
        in
        finish ~count:(1, 2) 0.3 "pins synced";
        finish ~count:(459, 459) 1.1 "packages checked";
        finish 0.1 "workspace prepared")
  in
  let rows =
    terminal_rows vte
    |> List.filter (fun row -> String.starts_with ~prefix:"=>" row)
  in
  Alcotest.(check int) "three permanent task rows" 3 (List.length rows);
  List.iter
    (fun row ->
      Alcotest.(check int)
        "committed row reaches the right edge" width (Width.string_width row);
      Alcotest.(check bool)
        "elapsed time is the rightmost field" true
        (String.ends_with ~suffix:"s" row))
    rows;
  Alcotest.(check int)
    "committed rows do not enter scrollback" 0 (Vte.scrollback_size vte);
  Alcotest.(check (pair int int))
    "cursor begins the row after history" (3, 0) (Vte.cursor_pos vte)

(* A whole block of rows shares one column grid: a count cell sized from the
   widest total the block carries ("470/470", seven columns), two spaces, and a
   timer cell six columns wide. So a row counting to 2 puts its pair in the same
   columns as one counting to 470, a row with no count leaves those columns
   empty rather than sliding its timer left, and the row whose timer passed ten
   seconds does not sit a column off its neighbours. The expected cells are the
   grid itself, not this display's own answer read back. *)
let test_rows_share_one_grid () =
  let width = 70 in
  let vte =
    terminal_run ~height:10 ~width (fun display advance _sync _vte ->
        let finish ?count elapsed label =
          let task = Display.task display label in
          Option.iter
            (fun (cur, total) -> Display.set_count task ~cur ~total)
            count;
          advance elapsed;
          Display.succeed task
        in
        finish ~count:(12, 256) 0.0 "pins synced";
        finish ~count:(470, 470) 0.2 "opam synced";
        finish ~count:(470, 470) 11.4 "packages checked";
        finish 0.4 "workspace prepared";
        finish 0.0 "split";
        finish ~count:(2, 2) 1.0 "pushed")
  in
  let rows =
    terminal_rows vte
    |> List.filter (fun row -> String.starts_with ~prefix:"=>" row)
  in
  let expected =
    [
      (" 12/256", "  0.0s");
      ("470/470", "  0.2s");
      ("470/470", " 11.4s");
      ("       ", "  0.4s");
      ("       ", "  0.0s");
      ("    2/2", "  1.0s");
    ]
  in
  Alcotest.(check int)
    "six permanent task rows" (List.length expected) (List.length rows);
  List.iter2
    (fun (count, elapsed) row ->
      Alcotest.(check int)
        "committed row reaches the right edge" width (Width.string_width row);
      Alcotest.(check string)
        "count cell, two spaces, timer cell"
        (count ^ "  " ^ elapsed)
        (trailing 15 row))
    expected rows;
  Alcotest.(check int)
    "committed rows do not enter scrollback" 0 (Vte.scrollback_size vte)

(* The same grid on a row that is still running. The timer crosses ten seconds
   and then a minute; it grows inside its own cell, so the count to its left
   holds its columns and the live line does not jump as it ticks. *)
let test_timer_keeps_its_cell () =
  let width = 70 in
  let seen = ref [] in
  ignore
    (terminal_run ~height:6 ~width (fun display advance sync vte ->
         let task = Display.task display "packages checked" in
         Display.set_count task ~cur:470 ~total:470;
         List.iter
           (fun dt ->
             advance dt;
             sync ();
             let row =
               List.hd (String.split_on_char '\n' (Vte.to_string vte))
             in
             seen := !seen @ [ trailing 15 row ])
           [ 9.9; 0.1; 90.0 ];
         Display.succeed task));
  Alcotest.(check (list string))
    "the timer grows inside its cell"
    [ "470/470    9.9s"; "470/470   10.0s"; "470/470   1m40s" ]
    !seen

(* The count cell is the block's and is sized from each row's total, so the
   columns a live label may take are one budget for the whole block: a row that
   has counted nothing yet towards 470 and a row with no count at all both give
   the label what is left after a seven-column count cell ("470/470"), not after
   the five their own text needs ("0/470") or the none a countless row needs. On
   a forty-column terminal that leaves 21 columns (a spinner, a space, the
   spacer's column, the count cell, two separators and the six-column timer
   cell), so a 22-column label loses its last word on both rows. A row
   counting from zero draws no bar, so the bar takes no columns from either. *)
let test_block_shares_one_label_budget () =
  let width = 40 in
  let rows = ref [] in
  ignore
    (terminal_run ~height:6 ~width (fun display _advance sync vte ->
         let counting = Display.task display "workspace prepared now" in
         Display.set_count counting ~cur:0 ~total:470;
         let bare = Display.task display "packages checked twice" in
         sync ();
         rows :=
           List.filteri
             (fun i _ -> i < 2)
             (String.split_on_char '\n' (Vte.to_string vte));
         Display.succeed counting;
         Display.succeed bare));
  let expected =
    [
      ("workspace prepared now", "workspace prepared", "  0/470    0.0s");
      ("packages checked twice", "packages checked", "           0.0s");
    ]
  in
  Alcotest.(check int) "two live rows" 2 (List.length !rows);
  List.iter2
    (fun (label, kept, cells) row ->
      Alcotest.(check bool)
        (label ^ " is shortened to the block's budget")
        false (contains label row);
      Alcotest.(check bool)
        (label ^ " keeps its leading words whole")
        true
        (contains (" " ^ kept ^ " ") row);
      Alcotest.(check bool)
        (label ^ " carries no ellipsis")
        false
        (contains "\xe2\x80\xa6" row);
      Alcotest.(check string)
        (label ^ ": count cell, two spaces, timer cell")
        cells (trailing 15 row))
    expected !rows

let test_default_visual_hierarchy_and_color_scope () =
  let width = 60 in
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  (* The accent exists because this formatter renders style; the terminal alone
     does not decide it. *)
  Fmt.set_style_renderer ppf `Ansi_tty;
  let now () = 0. in
  let vte = Vte.create ~rows:4 ~cols:width () in
  Display.run ~ctx:(ctx ~is_tty:true ~width ~height:4 ~ppf ~now ())
    ~mode:`Activity ~width ~height:4 (fun display ->
      let root = Display.task display "compile" in
      ignore (Display.task ~parent:root display "link");
      Display.tick display;
      Format.pp_print_flush ppf ();
      Vte.feed_string vte (Buffer.contents buffer);
      let rows = Vte.to_string vte |> String.split_on_char '\n' in
      Alcotest.(check bool)
        "root starts with the status glyph, not a fake tree branch" true
        (String.starts_with ~prefix:"⠋ compile" (List.nth rows 0));
      Alcotest.(check bool)
        "only nested work gets a guide" true
        (String.starts_with ~prefix:"  └── ⠋ link" (List.nth rows 1));
      let grid = Vte.grid vte in
      let is_accent x =
        let i = Grid.idx grid ~x ~y:0 in
        Grid.get_fg_r grid i < 0.5
        && Grid.get_fg_g grid i > 0.5
        && Grid.get_fg_b grid i > 0.5
      in
      let accented =
        List.init width Fun.id |> List.filter is_accent |> List.length
      in
      Alcotest.(check int)
        "accent is scoped to the live status glyph" 1 accented)

(* A digest cut at the terminal's last column reads as a value continuing on
   the next line, so a digest is never cut. *)
let digest_label =
  "sha256:a376ef543f48c9b17ed2b1cbe32b4d1e6a5f0c9d8e7b6a5948372615f0e1d2c3"

(* A row that does not fit the terminal is shortened by words, never by
   characters: it drops words the row can lose, or folds onto a second line
   the way git and cargo do. A path, an id or a digest is always printed
   whole. The row below is a digest between two words, on a terminal
   too narrow for all of it. *)
let pushed_label = "pushed " ^ digest_label ^ " to the registry"

let check_whole what ~width rows =
  Alcotest.(check bool)
    (what ^ " prints the digest whole")
    true
    (List.exists (contains digest_label) rows);
  Alcotest.(check bool)
    (what ^ " carries no ellipsis")
    false
    (List.exists (contains "\xe2\x80\xa6") rows);
  List.iter
    (fun row ->
      Alcotest.(check bool)
        (what ^ " fits the terminal")
        true
        (Width.string_width row <= width))
    rows

let test_row_keeps_a_digest_whole () =
  let width = 100 in
  let committed =
    terminal_run ~height:6 ~width (fun display _advance sync vte ->
        let task = Display.task display pushed_label in
        sync ();
        check_whole "the live row" ~width
          [ List.hd (String.split_on_char '\n' (Vte.to_string vte)) ];
        Display.succeed task)
  in
  let rows = terminal_rows committed in
  check_whole "the committed row" ~width rows;
  Alcotest.(check bool)
    "the word that did not fit beside it folds under it" true
    (List.exists (fun row -> String.equal (String.trim row) "registry") rows)

(* A log line is the caller's own text, and a cut through its middle joins two
   ends that were never next to each other into what reads as a shorter
   sentence. So a log line keeps its start and loses words from its end, whole:
   no word is cut and no mark is drawn. The line below is a service's output
   whose last clause names the number its reader watches. *)
let service_line =
  "[filter] kept 76 of 96 frames, 466944 bytes, dropped 20 over 0.35 cloud \
   (from the recorded pass in scene/, not from camera: services cannot send \
   frames to each other yet)"

let ellipsis = "\xe2\x80\xa6"

let rstrip row =
  let n = ref (String.length row) in
  while !n > 0 && row.[!n - 1] = ' ' do
    decr n
  done;
  String.sub row 0 !n

let index_of needle haystack =
  let n = String.length needle and m = String.length haystack in
  let rec loop i =
    if i + n > m then None
    else if String.sub haystack i n = needle then Some i
    else loop (i + 1)
  in
  loop 0

(* The live row, read while the task still runs: history commits a log line
   whole, so a screen read after the row settles would answer about the
   permanent copy and not about the one being fitted. *)
let logged_line ~width =
  let drawn = ref None in
  ignore
    (terminal_run ~height:8 ~width (fun display _advance sync vte ->
         let task = Display.task ~log_lines:2 display "payload/filter" in
         Display.log task service_line;
         sync ();
         drawn :=
           List.find_opt (contains "kept 76")
             (String.split_on_char '\n' (Vte.to_string vte));
         Display.succeed task));
  match !drawn with
  | None -> Alcotest.fail "the service's own line never reached the screen"
  | Some row -> rstrip row

let check_word_cut what ~width row =
  Alcotest.(check bool)
    (what ^ " fits the terminal")
    true
    (Width.string_width row <= width);
  Alcotest.(check bool)
    (what ^ " carries no ellipsis")
    false (contains ellipsis row);
  match index_of "[filter]" row with
  | None -> Alcotest.fail (what ^ " lost the start of the service's line")
  | Some start ->
      let message = String.sub row start (String.length row - start) in
      let n = String.length message in
      Alcotest.(check string)
        (what ^ " is the service's own line, from its start")
        (String.sub service_line 0 (min n (String.length service_line)))
        message;
      Alcotest.(check bool)
        (what ^ " ends where a word of the line ends")
        true
        (n >= String.length service_line || service_line.[n] = ' ')

let test_a_log_line_loses_words_from_its_end () =
  let row = logged_line ~width:100 in
  check_word_cut "a service's line on a 100-column terminal" ~width:100 row;
  Alcotest.(check bool)
    "the number the developer is told to watch is kept" true
    (contains "dropped 20 over 0.35 cloud" row)

let test_a_narrow_log_line_loses_words_from_its_end () =
  check_word_cut "a service's line on a 60-column terminal" ~width:60
    (logged_line ~width:60)

(* Every label and line this suite draws, on every terminal from 20 to 200
   columns wide: the live frame and the history after it carry no U+2026. *)
let ellipsis_fixtures =
  [
    digest_label;
    pushed_label;
    service_line;
    "workspace prepared now";
    "packages checked twice";
    "a very long task label with \xe4\xbd\xa0\xe5\xa5\xbd and control\tbytes";
    "Target: waiting for op-hello's boot verdict (300s bound)";
  ]

let test_no_row_carries_an_ellipsis () =
  for width = 20 to 200 do
    List.iter
      (fun label ->
        let live = ref "" in
        let vte =
          terminal_run ~height:12 ~width ~header:label
            (fun display _advance sync vte ->
              let task = Display.task ~log_lines:2 display label in
              Display.log task service_line;
              Display.log task label;
              sync ();
              live := Vte.to_string vte;
              Display.succeed task)
        in
        List.iter
          (fun row ->
            if contains ellipsis row then
              Alcotest.failf "%d columns, %S: a row carries U+2026: %S" width
                label row)
          (String.split_on_char '\n' !live @ terminal_rows vte))
      ellipsis_fixtures;
    List.iter
      (fun line ->
        if contains ellipsis (Span.to_string line) then
          Alcotest.failf "%d columns: Span.wrap marks a cut: %S" width
            (Span.to_string line))
      (Span.wrap width (Span.text digest_label))
  done

(* The SGR sequences in [s]: a CSI whose final byte is ['m']. A cursor move
   (final ['A']), an erase (['K']) and the cursor-visibility pair (['h'], ['l'])
   are CSIs too and are not counted, which is what separates no colour from no
   redraw. *)
let sgr_sequences s =
  let n = String.length s in
  let rec final j =
    if j >= n then j
    else
      let c = Char.code s.[j] in
      if c >= 0x40 && c <= 0x7e then j else final (j + 1)
  in
  let rec go i count =
    if i + 1 >= n then count
    else if s.[i] = '\027' && s.[i + 1] = '[' then
      let stop = final (i + 2) in
      go (stop + 1) (if stop < n && s.[stop] = 'm' then count + 1 else count)
    else go (i + 1) count
  in
  go 0 0

let colour_width = 40
let colour_height = 10

(* One live session on a terminal of a fixed geometry, drawn on a formatter
   whose style renderer is [renderer]: a row that logs and finishes, then a
   verdict. Its bytes are what the terminal received. *)
let colour_session renderer =
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf renderer;
  let now () = 0. in
  Display.run
    ~ctx:
      (ctx ~is_tty:true ~width:colour_width ~height:colour_height ~ppf ~now ())
    ~mode:`Activity ~width:colour_width ~height:colour_height ~header:"Build"
    (fun display ->
      let task = Display.task display "app" in
      Display.log task "planning";
      Display.succeed task;
      Display.set_result display (`Ok "done"));
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

let colour_screen bytes =
  let vte = Vte.create ~rows:colour_height ~cols:colour_width () in
  Vte.feed_string vte bytes;
  vte

(* The screen the session leaves: the log line the finished row handed to
   history, the row's own committed line with its elapsed time at the right
   edge, then the header and the verdict the close writes. Four rows, whatever
   the colour: the region was redrawn in place rather than appended to. *)
let colour_expected_screen =
  [
    "  # planning";
    "=> app" ^ String.make 30 ' ' ^ "0.0s";
    "[+] Build 0.0s (1/1)";
    "✓ done";
  ]

(* The known-correct screen: the same four lines typed straight at a terminal
   of the same geometry, with no styling and no cursor control to reach a
   frame above. Comparing the session's screen against this one, cell for
   cell, says both halves at once -- a cell that carries a style the plain
   text does not, and a glyph that is not where the plain text put it. *)
let colour_plain_screen () =
  let vte = Vte.create ~rows:colour_height ~cols:colour_width () in
  List.iter
    (fun line -> Vte.feed_string vte (line ^ "\r\n"))
    colour_expected_screen;
  vte

let unlike_plain_text vte =
  let drawn = Vte.grid vte and plain = Vte.grid (colour_plain_screen ()) in
  List.filter
    (fun i -> not (Grid.cells_equal drawn i plain i))
    (List.init (Grid.width plain * Grid.height plain) Fun.id)

(* A reader who turned colour off still gets the redraw: no SGR reaches the
   terminal, and the screen is the one the same text typed plainly leaves --
   no cell styled, every glyph where the plain text put it, so the region was
   redrawn in place rather than appended to. https://no-color.org asks that
   "the addition of ANSI color" be prevented, and the library adds it through
   one answer -- [Fmt.style_renderer] on the formatter it writes to -- which
   is the answer every other surface here already reads. *)
let test_a_plain_renderer_writes_no_sgr () =
  let bytes = colour_session `None in
  Alcotest.(check int) "no SGR reached the terminal" 0 (sgr_sequences bytes);
  let vte = colour_screen bytes in
  Alcotest.(check (list string))
    "the region still redrew in place" colour_expected_screen
    (Console_vte.screen ~scrollback:true vte);
  Alcotest.(check int)
    "no cell differs from the same text typed plainly" 0
    (List.length (unlike_plain_text vte))

(* The positive control, and the reading that says the session had colour to
   lose: the same session on a formatter that renders style carries the
   outcome's green, the label's bold and the metadata's dim, and puts exactly
   the same glyphs on the screen. *)
let test_an_ansi_renderer_writes_sgr () =
  let bytes = colour_session `Ansi_tty in
  Alcotest.(check bool)
    "the committed marker is green" true
    (contains "\027[32m" bytes);
  Alcotest.(check bool) "the label is bold" true (contains "\027[1m" bytes);
  Alcotest.(check bool) "the metadata is dim" true (contains "\027[2m" bytes);
  Alcotest.(check (list string))
    "colour changes no glyph" colour_expected_screen
    (Console_vte.screen ~scrollback:true (colour_screen bytes))

(* No colour is not no redraw. The cursor control the live region is made of
   is emitted whatever the style renderer says: it hides the cursor, moves up
   over the frame it is replacing, erases to the end of a line it shortened,
   and shows the cursor again at the close. *)
let test_a_plain_renderer_keeps_the_cursor_control () =
  let bytes = colour_session `None in
  let has what sequence =
    Alcotest.(check bool) what true (contains sequence bytes)
  in
  has "the cursor was hidden" "\027[?25l";
  has "the frame above was reached" "\027[1A";
  has "a shortened line was erased" "\027[K";
  has "the cursor came back" "\027[?25h"

(* The append-only layout is compact, not colourless: a history-mode session on
   a formatter that renders style carries the outcome's green on a committed
   row's marker and dim on its metadata, and its header carries the same styles
   the terminal header does. The one answer whether to colour is the
   formatter's own style renderer, so the same session on [`None] adds no SGR
   at all and the glyphs are unchanged. *)
let history_colour_session ?theme ?(ending = Display.succeed ?message:None)
    renderer =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf renderer;
  let now, advance = fake_clock () in
  Display.run ~ctx:(ctx ~ppf ~now ()) ?theme ~mode:`History ~header:"Build"
    (fun display ->
      let task = Display.task display "app" in
      advance 1.3;
      ending task);
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

(* The glyphs and layout of [s], with every SGR sequence removed. *)
let strip_sgr s =
  let n = String.length s in
  let buffer = Buffer.create n in
  let rec final j =
    if j >= n then j
    else
      let c = Char.code s.[j] in
      if c >= 0x40 && c <= 0x7e then j else final (j + 1)
  in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && s.[i] = '\027' && s.[i + 1] = '[' then begin
      let stop = final (i + 2) in
      if stop < n && s.[stop] = 'm' then go (stop + 1)
      else begin
        Buffer.add_substring buffer s i (stop + 1 - i);
        go (stop + 1)
      end
    end
    else begin
      Buffer.add_char buffer s.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents buffer

let test_history_carries_colour_on_a_styled_surface () =
  let bytes = history_colour_session `Ansi_tty in
  Alcotest.(check bool)
    "the committed marker carries the outcome's green" true
    (contains "\027[1m\027[32m=>\027[0m app" bytes);
  Alcotest.(check bool)
    "the committed metadata is dim" true
    (contains "\027[2m  1.3s\027[0m" bytes)

(* The theme's roles colour an outcome: ok a success, fail a failure, warn a
   cancellation. *)
let test_history_takes_the_theme_roles () =
  let theme =
    Theme.v ~ok:(Color.palette 82) ~fail:(Color.palette 197)
      ~warn:(Color.palette 214) ~spinner:Spinner.ascii ~done_marker:"+" ()
  in
  let check label expected ending =
    Alcotest.(check bool)
      label true
      (contains expected (history_colour_session ~theme ~ending `Ansi_tty))
  in
  check "ok" "\027[1m\027[38;5;82m=>\027[0m app" (Display.succeed ?message:None);
  check "fail" "\027[1m\027[38;5;197m!!\027[0m app" (Display.fail ?message:None);
  check "warn" "\027[1m\027[38;5;214m--\027[0m app"
    (Display.cancel ?message:None)

let test_history_adds_no_sgr_on_a_plain_surface () =
  Alcotest.(check int)
    "no SGR reached the pipe" 0
    (sgr_sequences (history_colour_session `None))

let test_history_glyphs_do_not_depend_on_colour () =
  Alcotest.(check string)
    "colour changes no glyph"
    (history_colour_session `None)
    (strip_sgr (history_colour_session `Ansi_tty))

let suite =
  ( "display_row",
    [
      Alcotest.test_case "history takes the theme roles" `Quick
        test_history_takes_the_theme_roles;
      Alcotest.test_case "markers come from the theme" `Quick
        test_markers_come_from_the_theme;
      Alcotest.test_case "zero progress has no empty bar" `Quick
        test_zero_progress_has_no_empty_bar;
      Alcotest.test_case "committed metadata is right aligned" `Quick
        test_committed_metadata_is_right_aligned;
      Alcotest.test_case "a block shares one column grid" `Quick
        test_rows_share_one_grid;
      Alcotest.test_case "a ticking timer keeps its cell" `Quick
        test_timer_keeps_its_cell;
      Alcotest.test_case "a live block shares one label budget" `Quick
        test_block_shares_one_label_budget;
      Alcotest.test_case "default visual hierarchy and color scope" `Quick
        test_default_visual_hierarchy_and_color_scope;
      Alcotest.test_case "a row keeps a digest whole" `Quick
        test_row_keeps_a_digest_whole;
      Alcotest.test_case "no row carries an ellipsis" `Quick
        test_no_row_carries_an_ellipsis;
      Alcotest.test_case "a log line loses words from its end" `Quick
        test_a_log_line_loses_words_from_its_end;
      Alcotest.test_case "a narrow log line loses words from its end" `Quick
        test_a_narrow_log_line_loses_words_from_its_end;
      Alcotest.test_case "a plain renderer writes no SGR" `Quick
        test_a_plain_renderer_writes_no_sgr;
      Alcotest.test_case "an ansi renderer writes SGR" `Quick
        test_an_ansi_renderer_writes_sgr;
      Alcotest.test_case "a plain renderer keeps the cursor control" `Quick
        test_a_plain_renderer_keeps_the_cursor_control;
      Alcotest.test_case "history carries colour on a styled surface" `Quick
        test_history_carries_colour_on_a_styled_surface;
      Alcotest.test_case "history adds no SGR on a plain surface" `Quick
        test_history_adds_no_sgr_on_a_plain_surface;
      Alcotest.test_case "history glyphs do not depend on colour" `Quick
        test_history_glyphs_do_not_depend_on_colour;
    ] )
