(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let src = Logs.Src.create "tty.test.display" ~doc:"Console display tests"

module Log = (val Logs.src_log src : Logs.LOG)

let contains pattern str = Re.execp (Re.compile (Re.str pattern)) str

let strip_ansi s =
  Re.(
    replace
      (compile
         (seq [ char '\027'; char '['; rep (alt [ rg '0' '?' ]); rg '@' '~' ]))
      ~f:(fun _ -> "")
      s)

let count_char c s =
  String.fold_left (fun n x -> if x = c then n + 1 else n) 0 s

(* {2 Byte formatter} *)

let test_bytes () =
  let c = Alcotest.(check string) in
  c "zero" "0 B" (Display.bytes 0L);
  c "small" "512 B" (Display.bytes 512L);
  c "kib" "1.00 KiB" (Display.bytes 1024L);
  c "kib frac" "1.50 KiB" (Display.bytes 1536L);
  c "mib" "30.00 MiB" (Display.bytes (Int64.mul 30L 1048576L));
  c "gib" "2.00 GiB" (Display.bytes (Int64.mul 2L 1073741824L));
  c "tib" "1.00 TiB" (Display.bytes (Int64.mul 1L 1099511627776L))

let mib n = Int64.mul (Int64.of_int n) 1048576L
let gib n = Int64.mul (Int64.of_int n) 1073741824L

(* A controllable clock: [now] reads the time, [advance dt] moves it on. *)
let fake_clock () =
  let t = ref 0. in
  ((fun () -> !t), fun dt -> t := !t +. dt)

(* The library leaves wide-character widths to its caller; the tests render CJK
   and emoji the way a terminal does, so they install the Uucp-backed hint. *)
let uucp_char_width u =
  match Uucp.Break.tty_width_hint u with -1 -> 0 | n -> n

let test_ctx ?(is_tty = false) ?(width = 80) ?(height = 24) ~ppf ~now () =
  Display.with_char_width
    (Display.ctx ~ppf ~now ~dimensions:(fun () -> (width, height)) ~is_tty ())
    uucp_char_width

(* {2 Plain mode -- deterministic, append-only} *)

let plain_display ?header ?bar () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, advance = fake_clock () in
  let ctx = test_ctx ~ppf ~now () in
  let d = Display.v ~ctx ~mode:`Plain ?header ?bar () in
  let out () =
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  (d, advance, out)

let test_plain_appends () =
  let d, advance, out = plain_display ~header:"Building" () in
  let a = Display.row d "resolve" in
  advance 1.3;
  Display.finish a;
  let b = Display.row d "fetch" in
  Display.set_bytes b ~cur:(mib 30) ~total:(gib 2);
  advance 2.0;
  Display.finish b;
  Display.finish_all d;
  Alcotest.(check string)
    "append-only lines"
    "=> resolve  1.3s\n\
     => fetch  30.00 MiB / 2.00 GiB  2.0s\n\
     [+] Building 3.3s (2/2)\n"
    (out ())

let test_plain_no_header () =
  let d, advance, out = plain_display () in
  let r = Display.row d "only" in
  advance 0.2;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check string)
    "no summary without a header" "=> only  0.2s\n" (out ())

let test_plain_no_ansi () =
  let d, advance, out = plain_display () in
  let r = Display.row d "step" in
  advance 0.5;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check bool)
    "no escape characters" false
    (String.contains (out ()) '\027')

let test_plain_count_final () =
  let d, advance, out = plain_display () in
  let r = Display.row d "items" in
  Display.set_count r ~cur:2 ~total:7;
  advance 0.5;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check string)
    "committed shows total/total" "=> items  2/7  0.5s\n" (out ())

let test_plain_child_indent () =
  let d, advance, out = plain_display ~header:"Building" () in
  let parent = Display.row d "build" in
  let child = Display.row ~parent d "layer" in
  advance 0.4;
  Display.finish child;
  advance 0.6;
  Display.finish parent;
  Display.finish_all d;
  Alcotest.(check string)
    "child indented under parent"
    "  => layer  0.4s\n=> build  1.0s\n[+] Building 1.0s (2/2)\n" (out ())

let test_plain_grandchild_indent () =
  let d, _, out = plain_display () in
  let a = Display.row d "a" in
  let b = Display.row ~parent:a d "b" in
  let c = Display.row ~parent:b d "c" in
  Display.finish c;
  Display.finish_all d;
  Alcotest.(check bool)
    "grandchild indented 4 spaces" true
    (contains "    => c" (out ()))

let test_plain_finish_message () =
  let d, advance, out = plain_display () in
  let r = Display.row d "compile" in
  advance 1.0;
  Display.finish ~message:"compiled 12 modules" r;
  Display.finish_all d;
  Alcotest.(check bool)
    "message replaces label" true
    (contains "=> compiled 12 modules  1.0s" (out ()))

let test_plain_set_label () =
  let d, advance, out = plain_display () in
  let r = Display.row d "old" in
  Display.set_label r "new";
  advance 0.1;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check bool)
    "label update reflected" true
    (contains "=> new" (out ()))

let test_plain_finish_idempotent () =
  let d, advance, out = plain_display () in
  let r = Display.row d "once" in
  advance 0.3;
  Display.finish r;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check int) "row committed exactly once" 1 (count_char '\n' (out ()))

let test_plain_tick_silent () =
  let d, advance, out = plain_display () in
  let _ = Display.row d "work" in
  advance 0.5;
  Display.tick d;
  Display.tick d;
  Alcotest.(check string) "tick emits nothing in plain mode" "" (out ())

let test_plain_clear_silent () =
  let d, _, out = plain_display () in
  let _ = Display.row d "work" in
  Display.clear d;
  Alcotest.(check string) "clear emits nothing in plain mode" "" (out ())

let test_plain_with_output_runs () =
  let d, _, out = plain_display () in
  let ran = ref false in
  let v =
    Display.with_output d (fun () ->
        ran := true;
        42)
  in
  Alcotest.(check int) "returns f's value" 42 v;
  Alcotest.(check bool) "f ran" true !ran;
  Alcotest.(check string) "no region churn in plain" "" (out ())

let test_plain_result_banner () =
  let d, _, out = plain_display ~header:"Build" () in
  let r = Display.row d "step" in
  Display.finish r;
  Display.finish_all ~result:(`Fail "builder build failed")
    ~detail:"Library \"wire\" not found\nlogs: space logs bootstrap" d;
  let s = out () in
  Alcotest.(check bool)
    "detail before banner" true
    (contains "Library \"wire\" not found\nlogs: space logs bootstrap\n" s);
  Alcotest.(check bool)
    "fail banner after detail" true
    (contains "[FAIL] builder build failed\n" s)

let test_plain_set_result () =
  (* The verdict recorded on the display is rendered by a bare [finish_all]:
     the detail paragraph, then the banner, exactly as if the caller had
     passed [~result ?detail] itself. *)
  let d, _, out = plain_display ~header:"Build" () in
  let r = Display.row d "step" in
  Display.finish r;
  Display.set_result d ~detail:"3 files, 0 errors" (`Ok "build succeeded");
  Display.finish_all d;
  Alcotest.(check bool)
    "stored detail then stored banner" true
    (contains "3 files, 0 errors\n[OK] build succeeded\n" (out ()))

let test_plain_set_result_last_wins () =
  let d, _, out = plain_display () in
  let r = Display.row d "step" in
  Display.finish r;
  Display.set_result d (`Ok "early verdict");
  Display.set_result d (`Fail "late verdict");
  Display.finish_all d;
  let s = out () in
  Alcotest.(check bool)
    "last verdict rendered" true
    (contains "[FAIL] late verdict\n" s);
  Alcotest.(check bool)
    "earlier verdict gone" false
    (contains "early verdict" s)

let test_plain_finish_all_result_overrides () =
  (* An explicit [~result] passed to [finish_all] wins over the recorded
     verdict, and the recorded detail does not leak under it. *)
  let d, _, out = plain_display () in
  let r = Display.row d "step" in
  Display.finish r;
  Display.set_result d ~detail:"stored detail" (`Ok "stored verdict");
  Display.finish_all ~result:(`Fail "explicit verdict") d;
  let s = out () in
  Alcotest.(check bool)
    "explicit verdict rendered" true
    (contains "[FAIL] explicit verdict\n" s);
  Alcotest.(check bool)
    "stored verdict gone" false
    (contains "stored verdict" s);
  Alcotest.(check bool) "stored detail gone" false (contains "stored detail" s)

let test_finish_all_finishes_rows () =
  let d, advance, out = plain_display ~header:"H" () in
  let _ = Display.row d "a" in
  let _ = Display.row d "b" in
  advance 1.0;
  Display.finish_all d;
  let s = out () in
  Alcotest.(check bool) "row a committed" true (contains "=> a  1.0s" s);
  Alcotest.(check bool) "row b committed" true (contains "=> b  1.0s" s);
  Alcotest.(check bool) "summary 2/2" true (contains "(2/2)" s)

(* {2 Bar style} *)

let test_style_default_segments () =
  let d, _, out = plain_display () in
  (* The default style is exercised on live rows; in plain mode it is the
     committed line we observe, which is style-independent. Assert the style
     value is usable and counts render. *)
  let r = Display.row ~bar:Display.Line.default d "x" in
  Display.set_count r ~cur:3 ~total:3;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check bool)
    "counts in committed line" true
    (contains "=> x  3/3" (out ()))

(* {2 TTY mode -- structural assertions on escaped output} *)

let tty_display ?header ?bar () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, advance = fake_clock () in
  let d =
    Display.v
      ~ctx:(test_ctx ~is_tty:true ~ppf ~now ())
      ~mode:`Console ?header ?bar ()
  in
  let raw () =
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let text () = strip_ansi (raw ()) in
  (d, advance, raw, text)

let test_tty_emits_ansi () =
  let d, advance, raw, text = tty_display ~header:"Building" () in
  let r = Display.row d "resolve image" in
  Display.set_count r ~cur:3 ~total:4;
  advance 1.0;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check bool)
    "uses ANSI escapes" true
    (String.contains (raw ()) '\027');
  let t = text () in
  Alcotest.(check bool) "done glyph" true (contains "[+]" t);
  Alcotest.(check bool) "label" true (contains "resolve image" t);
  Alcotest.(check bool) "header" true (contains "[+] Building" t)

let test_tty_live_bar () =
  let d, _, _, text = tty_display () in
  let r = Display.row d "x" in
  Display.set_count r ~cur:1 ~total:2;
  let t = text () in
  Alcotest.(check bool) "bar bracket drawn" true (contains "█" t);
  Alcotest.(check bool) "percentage drawn" true (contains "50%" t);
  Alcotest.(check bool) "counters drawn" true (contains "1/2" t);
  Display.clear d

let test_tty_bytes_rate () =
  let d, advance, _, text = tty_display () in
  let r = Display.row d "dl" in
  Display.set_bytes r ~cur:0L ~total:(gib 1);
  advance 1.0;
  Display.set_bytes r ~cur:(mib 100) ~total:(gib 1);
  let t = text () in
  Alcotest.(check bool)
    "humanised sizes" true
    (contains "100.00 MiB / 1.00 GiB" t);
  Alcotest.(check bool) "rate shown" true (contains "/s" t);
  Display.clear d

let test_tty_custom_style () =
  let bar = Display.Line.(v [ percent; text "of"; count ]) in
  let d, _, _, text = tty_display ~bar () in
  let r = Display.row d "x" in
  Display.set_count r ~cur:1 ~total:2;
  let t = text () in
  Alcotest.(check bool) "percent" true (contains "50%" t);
  Alcotest.(check bool) "text segment" true (contains "of" t);
  Alcotest.(check bool) "counters" true (contains "1/2" t);
  Alcotest.(check bool) "no bar bracket" false (contains "█" t);
  Display.clear d

let test_tty_set_bar_accent () =
  let bar =
    Display.Line.(
      v ~color:`Red [ accent "[ERROR]"; text " "; label; spacer; elapsed ])
  in
  let d, advance, raw, text = tty_display () in
  let r = Display.row d "compile" in
  Display.set_bar r bar;
  advance 0.4;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check bool)
    "accent marker" true
    (contains "[ERROR] compile" (text ()));
  Alcotest.(check bool) "red accent" true (contains "\027[31m[ERROR]" (raw ()));
  Alcotest.(check bool) "no done marker" false (contains "[+]" (text ()))

let test_tty_scroll_off () =
  let d, advance, _, text = tty_display ~header:"B" () in
  let a = Display.row d "first" in
  let b = Display.row d "second" in
  advance 0.5;
  Display.finish a;
  (* [first] is committed (done glyph), [second] still live. *)
  let t = text () in
  Alcotest.(check bool) "finished row committed" true (contains "[+]" t);
  Alcotest.(check bool)
    "both labels seen" true
    (contains "first" t && contains "second" t);
  Display.finish b;
  Display.finish_all d

let test_tty_clear_no_commit () =
  let d, advance, raw, text = tty_display () in
  let _ = Display.row d "aborted" in
  advance 0.5;
  Display.clear d;
  Alcotest.(check bool)
    "no done glyph (nothing committed)" false
    (contains "[+]" (text ()));
  Alcotest.(check bool) "restores cursor" true (contains "\027[?25h" (raw ()))

let test_tty_with_output () =
  let d, _, _, text = tty_display () in
  let _ = Display.row d "live" in
  Display.with_output d (fun () -> Fmt.pr "interjected\n");
  (* output landed; region redrawn afterwards (label still present) *)
  Alcotest.(check bool)
    "row still live after interjection" true
    (contains "live" (text ()));
  Display.clear d

let test_tty_tick_redraws () =
  let d, advance, raw, _ = tty_display () in
  let _ = Display.row d "x" in
  (* An idle tick -- nothing changed since the last paint -- repaints nothing:
     clearing and rewriting an identical frame on every tick is the flicker the
     display avoids. *)
  let before = String.length (raw ()) in
  Display.tick d;
  Alcotest.(check int)
    "idle tick paints nothing" before
    (String.length (raw ()));
  (* Once the timer advances the frame differs, so the next tick does
     repaint. *)
  advance 1.0;
  Display.tick d;
  Alcotest.(check bool)
    "tick repaints once the frame changes" true
    (String.length (raw ()) > before);
  Display.clear d

(* A [Layout.hcat_anim] banner redraws on content, not on the clock: the display
   only repaints when the joined frame's bytes actually change, so ticking while
   the layout looks the same costs nothing even as time advances. The two blocks
   here read ["lo"] until 1s, then ["hi"], so the joined banner is stable across
   the first two ticks and differs on the third. *)
let test_tty_layout_minimal_redraw () =
  let d, advance, raw, text = tty_display () in
  let block tag =
    Anim.v (fun ~elapsed -> if elapsed < 1.0 then tag ^ ":lo" else tag ^ ":hi")
  in
  Display.set_banner d (Layout.hcat_anim [ block "L"; block "R" ]);
  (* First tick paints the joined banner. *)
  advance 0.2;
  Display.tick d;
  let after_first = String.length (raw ()) in
  Alcotest.(check bool)
    "joined banner painted" true
    (contains "L:lo R:lo" (text ()));
  (* The clock advances but the joined frame is byte-identical (still under 1s),
     so the tick must repaint nothing -- the minimal-redraw guarantee. *)
  advance 0.3;
  Display.tick d;
  Alcotest.(check int)
    "idle tick paints nothing while the layout is unchanged" after_first
    (String.length (raw ()));
  (* Crossing 1s changes the joined frame, so the next tick repaints. *)
  advance 0.6;
  Display.tick d;
  Alcotest.(check bool)
    "repaints once the layout frame changes" true
    (String.length (raw ()) > after_first);
  Alcotest.(check bool)
    "new joined banner present" true
    (contains "L:hi R:hi" (text ()));
  Display.clear d

let test_tty_unicode_theme () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, advance = fake_clock () in
  let d =
    Display.v
      ~ctx:(test_ctx ~is_tty:true ~ppf ~now ())
      ~mode:`Console ~theme:Theme.unicode ()
  in
  let r = Display.row d "x" in
  Display.set_count r ~cur:1 ~total:2;
  Alcotest.(check bool)
    "smooth bar bracket" true
    (contains "▕" (strip_ansi (Buffer.contents buf)));
  advance 1.0;
  Display.finish r;
  Display.finish_all d;
  Alcotest.(check bool)
    "check-mark done glyph" true
    (contains "✔" (strip_ansi (Buffer.contents buf)))

(* {2 Per-row log window} *)

let test_tty_log_window () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, advance = fake_clock () in
  let d =
    Display.v ~ctx:(test_ctx ~is_tty:true ~ppf ~now ()) ~mode:`Console ()
  in
  let r = Display.row ~log_lines:3 d "build" in
  List.iter (Display.log r) [ "line1"; "line2"; "line3"; "line4" ];
  (* Inspect only the final frame: earlier frames still held the dropped line.
     Advance the clock a hair so the row timer differs and the tick actually
     repaints (an identical frame is skipped). *)
  advance 0.1;
  Buffer.clear buf;
  Display.tick d;
  Format.pp_print_flush ppf ();
  let t = strip_ansi (Buffer.contents buf) in
  Alcotest.(check bool) "oldest line dropped" false (contains "line1" t);
  Alcotest.(check bool)
    "recent lines kept" true
    (contains "line2" t && contains "line3" t && contains "line4" t);
  Alcotest.(check bool) "tail marker indented" true (contains "  # line4" t);
  Display.clear d

let test_tty_no_log_without_window () =
  let d, _, _, text = tty_display () in
  let r = Display.row d "build" in
  Display.log r "should not show";
  Alcotest.(check bool)
    "no tail without a window" false
    (contains "should not show" (text ()));
  Display.clear d

let test_default_row_memoized () =
  (* The untagged-logs row is created on first request and handed back on
     every later one, so all destination-less log lines share one window. *)
  let d, _, out = plain_display () in
  let a = Display.default_row d in
  let b = Display.default_row ~log_lines:5 d in
  Alcotest.(check bool) "same row handed back" true (a == b);
  Display.log a "untagged-line";
  Display.finish_all d;
  let s = out () in
  Alcotest.(check bool)
    "log lands in the shared window" true
    (contains "# untagged-line" s);
  Alcotest.(check int)
    "exactly one logs row committed" 1
    (List.length
       (List.filter (contains "=> logs") (String.split_on_char '\n' s)))

let test_tty_default_row_window () =
  (* [log_lines] sizes the window when the call creates the row: the logged
     line shows in the live tail under the [logs] label. *)
  let d, _, _, text = tty_display () in
  let r = Display.default_row ~log_lines:3 d in
  Display.log r "untagged-line";
  let t = text () in
  Alcotest.(check bool) "logs row shown" true (contains "logs" t);
  Alcotest.(check bool) "line in the window" true (contains "# untagged-line" t);
  Display.clear d

let test_plain_log_appends () =
  let d, _, out = plain_display () in
  let r = Display.row d "build" in
  Display.log r "compiling foo\ncompiling bar";
  Display.finish r;
  Display.finish_all d;
  let s = out () in
  Alcotest.(check bool)
    "log lines printed append-only" true
    (contains "  # compiling foo\n  # compiling bar\n" s);
  Alcotest.(check bool)
    "committed line after logs" true
    (contains "=> build  0.0s" s)

let test_plain_log_gap () =
  (* A log line that arrives after a stall carries the elapsed gap; rapid lines
     do not, so the noise stays low and the idle time stands out. *)
  let d, advance, out = plain_display () in
  let r = Display.row d "build" in
  Display.log r "start";
  advance 18.0;
  Display.log r "after a long pause";
  advance 0.2;
  Display.log r "quick follow-up";
  let s = out () in
  Alcotest.(check bool)
    "gap shown on the stalled line" true
    (contains "# after a long pause  +18.0s" s);
  Alcotest.(check bool) "no gap on the first line" true (contains "# start\n" s);
  Alcotest.(check bool)
    "no gap on a sub-second line" false
    (contains "# quick follow-up  +" s)

let test_tty_log_gap () =
  (* The same gap surfaces in the live log window. *)
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, advance = fake_clock () in
  let d =
    Display.v ~ctx:(test_ctx ~is_tty:true ~ppf ~now ()) ~mode:`Console ()
  in
  let r = Display.row ~log_lines:3 d "build" in
  Display.log r "fetch";
  advance 12.5;
  Display.log r "extracting";
  (* Advance a hair so the frame differs and the snapshot tick repaints; the
     logged gap (+12.5s) is fixed at log time, so it is unaffected. *)
  advance 0.1;
  Buffer.clear buf;
  Display.tick d;
  Format.pp_print_flush ppf ();
  let t = strip_ansi (Buffer.contents buf) in
  Alcotest.(check bool)
    "stall gap rendered in window" true
    (contains "# extracting  +12.5s" t);
  Display.clear d

(* A user-defined theme: a camel for the spinner, a chequered flag when done. *)
let test_tty_custom_theme () =
  let camels = Theme.v ~spinner:(Spinner.v [| "🐫" |]) ~done_marker:"🏁" () in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, _ = fake_clock () in
  let d =
    Display.v
      ~ctx:(test_ctx ~is_tty:true ~ppf ~now ())
      ~mode:`Console ~theme:camels ()
  in
  let r = Display.row d "caravan" in
  Display.set_count r ~cur:1 ~total:2;
  Alcotest.(check bool)
    "camel spinner shows" true
    (contains "🐫" (strip_ansi (Buffer.contents buf)));
  Display.clear d

(* {2 Terminal replay -- assert the final on-screen state, not the byte soup}

   The raw Buffer holds every byte the renderer emitted, including the cursor
   moves that overwrite earlier frames. To catch duplicated rows we must apply
   that stream the way a terminal would and inspect the resulting screen +
   scrollback. This [Vt] models exactly the controls Display emits: printable
   text, [\r], [\n] (as CR+LF, matching a cooked-mode tty with ONLCR), CSI K/2K
   erase, and CSI nA cursor-up clamped to the top of the physical screen (so a
   region that scrolled into scrollback is unreachable, as on a real terminal).
   SGR colours and cursor-visibility toggles are consumed and ignored. *)
module Vt = struct
  type t = {
    height : int;
    width : int;
    mutable lines : string array;
    mutable used : int;
    mutable row : int;
    mutable col : int;
  }

  let v ?(height = 24) ?(width = 80) () =
    { height; width; lines = Array.make 16 ""; used = 1; row = 0; col = 0 }

  let ensure t r =
    if r >= Array.length t.lines then begin
      let bigger = Array.make (max (r + 1) (2 * Array.length t.lines)) "" in
      Array.blit t.lines 0 bigger 0 t.used;
      t.lines <- bigger
    end;
    if r >= t.used then t.used <- r + 1

  let screen_top t = max 0 (t.used - t.height)

  let put t s =
    ensure t t.row;
    let line = t.lines.(t.row) in
    let line =
      if String.length line < t.col then
        line ^ String.make (t.col - String.length line) ' '
      else line
    in
    let before = String.sub line 0 t.col in
    let tail_off = t.col + String.length s in
    let after =
      if String.length line > tail_off then
        String.sub line tail_off (String.length line - tail_off)
      else ""
    in
    t.lines.(t.row) <- before ^ s ^ after;
    t.col <- t.col + String.length s

  (* Read a CSI sequence starting just after "\027[": returns (params, final,
     next index). *)
  let read_csi s i =
    let n = String.length s in
    let j = ref i in
    while !j < n && not (s.[!j] >= '@' && s.[!j] <= '~') do
      incr j
    done;
    let params = String.sub s i (!j - i) in
    if !j < n then (params, s.[!j], !j + 1) else (params, ' ', !j)

  let int_param p = match int_of_string_opt p with Some n -> n | None -> 1

  let feed t s =
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      match s.[!i] with
      | '\027' when !i + 1 < n && s.[!i + 1] = '[' ->
          let params, final, next = read_csi s (!i + 2) in
          (match final with
          | 'A' -> t.row <- max (screen_top t) (t.row - int_param params)
          | 'B' -> t.row <- t.row + int_param params
          | 'K' ->
              ensure t t.row;
              if params = "2" then t.lines.(t.row) <- "" (* whole line *)
              else
                let line = t.lines.(t.row) in
                if String.length line > t.col then
                  t.lines.(t.row) <- String.sub line 0 t.col (* to EOL *)
          | _ -> () (* SGR colours, cursor show/hide, etc.: no cursor effect *));
          i := next
      | '\r' ->
          t.col <- 0;
          incr i
      | '\n' ->
          t.col <- 0;
          t.row <- t.row + 1;
          ensure t t.row;
          incr i
      | _ ->
          (* Autowrap like a real terminal: a printable past the last column
             moves to column 0 of the next row (scrolling if at the bottom).
             This is what turns an un-truncated, over-wide line into a second
             physical row and desyncs the redraw -- the bug this models. One
             UTF-8 byte at a time is fine: the wrap tests use ASCII. *)
          if t.col >= t.width then begin
            t.col <- 0;
            t.row <- t.row + 1;
            ensure t t.row
          end;
          put t (String.make 1 s.[!i]);
          incr i
    done

  let rows t = Array.to_list (Array.sub t.lines 0 t.used)

  (* How many rows (screen + scrollback) contain [needle] as a substring. *)
  let count_rows t needle =
    List.fold_left
      (fun acc line ->
        if contains needle (strip_ansi line) then acc + 1 else acc)
      0 (rows t)
end

(* A tty display whose every byte is mirrored into a [Vt], so a test can assert
   the final terminal state. [raw] writes bytes straight to the same terminal,
   bypassing the lib's line accounting, to model a subprocess that inherited the
   tty or an error printed without [suspend]. *)
type vt_harness = {
  d : Display.t;
  vt : Vt.t;
  sync : unit -> unit;
  raw : string -> unit;
  advance : float -> unit;
}

let vt_display ?(height = 24) ?(width = 80) ?header () =
  let vt = Vt.v ~height ~width () in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, advance = fake_clock () in
  let ctx = test_ctx ~is_tty:true ~width ~height ~ppf ~now () in
  let d = Display.v ~ctx ~mode:`Console ~width ~height ?header () in
  let sync () =
    Format.pp_print_flush ppf ();
    Vt.feed vt (Buffer.contents buf);
    Buffer.clear buf
  in
  let raw s =
    sync ();
    Vt.feed vt s
  in
  { d; vt; sync; raw; advance }

(* {2 Nested rows + per-row log windows} *)

let test_tty_child_log_indent () =
  let h = vt_display ~header:"B" () in
  let parent = Display.row h.d "payloads" in
  let child = Display.row ~parent ~log_lines:3 h.d "Run" in
  Display.log child "hello";
  h.sync ();
  let line =
    List.find (contains "hello") (List.map strip_ansi (Vt.rows h.vt))
  in
  (* child is depth 1 (indent 2) and its log tail adds two more, so "# hello"
     sits at column 4. *)
  Alcotest.(check bool)
    "child log indented to depth" true
    (contains "    # hello" line);
  Display.clear h.d

let test_tty_header_marker () =
  (* The header reads [-] while work is in progress and flips to [+] once every
     row is done. *)
  let h = vt_display ~header:"Build" () in
  let r = Display.row h.d "step" in
  h.sync ();
  Alcotest.(check int) "WIP marker [-]" 1 (Vt.count_rows h.vt "[-] Build");
  Alcotest.(check int) "no done marker yet" 0 (Vt.count_rows h.vt "[+] Build");
  Display.finish ~live:true r;
  h.sync ();
  Alcotest.(check int) "done marker [+]" 1 (Vt.count_rows h.vt "[+] Build");
  Alcotest.(check int) "WIP marker gone" 0 (Vt.count_rows h.vt "[-] Build");
  Display.clear h.d;
  (* A row that finished in error flips the completed marker to [x]. *)
  let h2 = vt_display ~header:"Build" () in
  let r2 = Display.row h2.d "step" in
  Display.finish ~live:true ~error:true r2;
  h2.sync ();
  Alcotest.(check int) "error marker [x]" 1 (Vt.count_rows h2.vt "[x] Build");
  Alcotest.(check int) "no success marker" 0 (Vt.count_rows h2.vt "[+] Build");
  Display.clear h2.d

let test_tty_result_banner () =
  let h = vt_display ~header:"Build" () in
  let r = Display.row h.d "step" in
  Display.finish ~live:true r;
  Display.finish_all ~result:(`Ok "bootstrap complete") h.d;
  h.sync ();
  Alcotest.(check int)
    "ok banner shown once" 1
    (Vt.count_rows h.vt "[OK] bootstrap complete")

let test_tty_set_result_banner () =
  (* The recorded verdict renders through a bare [finish_all] on a terminal
     too, on exactly one row. *)
  let h = vt_display ~header:"Build" () in
  let r = Display.row h.d "step" in
  Display.finish ~live:true r;
  Display.set_result h.d (`Ok "bootstrap complete");
  Display.finish_all h.d;
  h.sync ();
  Alcotest.(check int)
    "recorded ok banner shown once" 1
    (Vt.count_rows h.vt "[OK] bootstrap complete")

let test_tty_themed_banner () =
  (* A theme can swap the verdict markers; the unicode theme uses a tick. *)
  let vt = Vt.v ~height:24 ~width:80 () in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, _ = fake_clock () in
  let d =
    Display.v
      ~ctx:(test_ctx ~is_tty:true ~width:80 ~height:24 ~ppf ~now ())
      ~mode:`Console ~width:80 ~height:24 ~theme:Theme.unicode ~header:"Build"
      ()
  in
  let r = Display.row d "step" in
  Display.finish ~live:true r;
  Display.finish_all ~result:(`Ok "done") d;
  Format.pp_print_flush ppf ();
  Vt.feed vt (Buffer.contents buf);
  Alcotest.(check int) "tick verdict from theme" 1 (Vt.count_rows vt "✔ done")

let test_tty_remove_row () =
  (* A transient byte row removed on completion leaves no trace -- neither a
     committed scrollback line nor a kept-live count line -- while its sibling
     stays. Mirrors dropping a per-transfer rate line when a COPY finishes. *)
  let h = vt_display ~header:"B" () in
  let step = Display.row h.d "COPY" in
  let bytes = Display.row ~parent:step h.d "transfer" in
  Display.set_bytes bytes ~cur:(gib 1) ~total:(gib 1);
  h.sync ();
  Alcotest.(check int)
    "transfer visible while active" 1
    (Vt.count_rows h.vt "transfer");
  Display.remove bytes;
  Display.finish ~live:true step;
  h.sync ();
  Alcotest.(check int)
    "transfer gone after remove" 0
    (Vt.count_rows h.vt "transfer");
  Alcotest.(check int) "step still shown" 1 (Vt.count_rows h.vt "COPY");
  Display.finish_all h.d

let test_tty_collapse_no_clobber () =
  (* Collapsing a tall live region (many rows removed at once) must never clear
     above the screen into committed scrollback: the height cap keeps the region
     on screen, so committed lines survive the collapse even though the live
     region shrinks upward. *)
  let h = vt_display ~height:12 ~width:50 ~header:"Build" () in
  for i = 1 to 6 do
    let r = Fmt.kstr (Display.row h.d) "done %d" i in
    Display.finish r;
    h.sync ()
  done;
  let rows =
    List.init 8 (fun i ->
        let r = Fmt.kstr (Display.row h.d) "live %d" i in
        Display.finish ~live:true r;
        h.sync ();
        r)
  in
  List.iter Display.remove rows;
  h.sync ();
  (* Committed rows above are untouched; the removed live rows are gone. *)
  List.iter
    (fun i ->
      Alcotest.(check int)
        (Fmt.str "committed done %d kept" i)
        1
        (Fmt.kstr (Vt.count_rows h.vt) "done %d" i))
    [ 1; 2; 3; 4; 5; 6 ];
  Alcotest.(check int) "live rows gone" 0 (Vt.count_rows h.vt "live ");
  Alcotest.(check int) "header once" 1 (Vt.count_rows h.vt "Build");
  Display.finish_all h.d

let test_tty_finish_child_then_log_parent () =
  (* The exact field scenario: a child subtask completes, then a new log line is
     appended to the still-running parent. The renderer's own byte stream must
     leave the parent row and header on exactly one terminal row each -- no
     stranded duplicate. *)
  let h = vt_display ~header:"Bootstrap" () in
  let parent = Display.row ~log_lines:4 h.d "payloads" in
  Display.log parent "appliance ready";
  let child = Display.row ~parent ~log_lines:4 h.d "Run" in
  Display.log child "console";
  Display.log child "init";
  h.sync ();
  Display.finish child;
  h.sync ();
  Display.log parent "cache oi";
  Display.log parent "seed";
  h.sync ();
  Alcotest.(check int) "header appears once" 1 (Vt.count_rows h.vt "Bootstrap");
  Alcotest.(check int)
    "parent row appears once" 1
    (Vt.count_rows h.vt "payloads");
  Alcotest.(check int)
    "finished child committed once" 1 (Vt.count_rows h.vt "Run");
  Display.finish_all h.d

let test_tty_foreign_write_desyncs () =
  (* The real field failure mode: bytes reaching the tty outside the lib (a
     subprocess that inherited the terminal, or an error printed without
     [suspend]) move the cursor without updating [live_height], so the next
     paint strands a copy of the live region. This reproduces it, then shows
     [suspend] keeps the accounting correct. *)
  let foreign = "subprocess line A\nsubprocess line B\n" in
  (* Bare foreign write between paints: the region is stranded. *)
  let h = vt_display ~header:"B" () in
  let _ = Display.row h.d "payloads" in
  h.sync ();
  h.raw foreign;
  (* Advance so the frame differs and the tick repaints (an unchanged frame is
     skipped); it repaints from the stale cursor, stranding the region. *)
  h.advance 1.0;
  Display.tick h.d;
  h.sync ();
  let bare_dups = Vt.count_rows h.vt "payloads" in
  Display.clear h.d;
  (* Same foreign write, but via [suspend]: clean. *)
  let h2 = vt_display ~header:"B" () in
  let _ = Display.row h2.d "payloads" in
  h2.sync ();
  Display.suspend (fun () -> h2.raw foreign);
  h2.sync ();
  let suspended_dups = Vt.count_rows h2.vt "payloads" in
  Display.clear h2.d;
  Alcotest.(check int) "suspend keeps the region on one row" 1 suspended_dups;
  Alcotest.(check bool)
    "bare foreign write strands a duplicate" true (bare_dups >= 2)

let test_tty_caps_to_height () =
  (* A region taller than the terminal is trimmed so a paint never scrolls a
     stale copy into unreachable scrollback. With a short screen, finishing a
     row (which commits and repaints) must still leave every live row once. *)
  let h = vt_display ~height:6 ~header:"B" () in
  let parent = Display.row ~log_lines:10 h.d "payloads" in
  List.iter (Display.log parent)
    [ "l1"; "l2"; "l3"; "l4"; "l5"; "l6"; "l7"; "l8" ];
  h.sync ();
  let child = Display.row ~parent h.d "child" in
  h.sync ();
  Display.finish child;
  h.sync ();
  Display.log parent "l9";
  h.sync ();
  (* No line of the live region is ever drawn on two rows at once. *)
  Alcotest.(check int) "header once" 1 (Vt.count_rows h.vt "B ");
  Alcotest.(check int) "parent once" 1 (Vt.count_rows h.vt "payloads");
  Alcotest.(check bool)
    "overflow marker shown" true
    (Vt.count_rows h.vt "more lines" >= 1);
  Display.finish_all h.d

let test_tty_ticks_at_full_screen () =
  (* The animate daemon ([Console_eio.Display.animate]) calls [tick] ~8x/s while a
     long step runs and nothing else changes. Once enough rows have committed to
     fill the screen, the live region sits at the bottom with committed
     scrollback above it. Repeated ticks must redraw it in place -- not append a
     fresh header every tick (the crawling-timestamp duplication in the field).
     The clock is fixed, so any extra header rows are stranded copies. *)
  let h = vt_display ~height:8 ~header:"Bootstrap dev (arm64)" () in
  for i = 1 to 6 do
    let r = Fmt.kstr (Display.row h.d) "step %d" i in
    Display.finish r;
    (* commit to scrollback *)
    h.sync ()
  done;
  let run = Display.row ~log_lines:2 h.d "Run: builder.json" in
  Display.log run "working";
  h.sync ();
  for _ = 1 to 20 do
    Display.tick h.d;
    h.sync ()
  done;
  Alcotest.(check int)
    "header once after many idle ticks" 1
    (Vt.count_rows h.vt "Bootstrap dev");
  Alcotest.(check int)
    "active leaf once" 1
    (Vt.count_rows h.vt "Run: builder.json");
  Display.finish_all h.d

let test_tty_full_screen_stress () =
  (* Hammer the height boundary the way a real build does: a live phase whose
     log window stays near the cap, finished steps piling up live, fresh commits
     scrolling the screen, and animate ticks throughout. After every step the
     header must be on exactly one row. *)
  let h = vt_display ~height:8 ~header:"Bootstrap dev (arm64)" () in
  let phase = Display.row ~log_lines:3 h.d "payloads: arm64" in
  let check label =
    Alcotest.(check int) label 1 (Vt.count_rows h.vt "Bootstrap dev")
  in
  for i = 1 to 12 do
    let step = Fmt.kstr (Display.row ~parent:phase h.d) "step %d" i in
    Fmt.kstr (Display.log phase) "working on %d" i;
    h.sync ();
    Display.tick h.d;
    h.sync ();
    (* Some steps commit (phase-style), some stay live (builder-step style). *)
    if i mod 3 = 0 then Display.finish step else Display.finish ~live:true step;
    h.sync ();
    Display.tick h.d;
    h.sync ();
    Fmt.kstr check "header once after step %d" i
  done;
  Display.finish_all h.d

let test_tty_wide_header_no_wrap () =
  (* A header longer than the terminal width must be truncated, not wrapped onto
     a second physical row. An un-truncated header wraps, so the redraw clears
     one row too few every frame and strands a fresh header on each tick (the
     crawling-timestamp duplication). Narrow terminal, long header, many ticks:
     the header must stay on exactly one row. *)
  let h =
    vt_display ~width:30 ~height:10
      ~header:"Bootstrap dev (arm64) building the whole world" ()
  in
  let r = Display.row ~log_lines:2 h.d "payloads" in
  Display.log r "working";
  h.sync ();
  for _ = 1 to 15 do
    Display.tick h.d;
    h.sync ()
  done;
  Alcotest.(check int)
    "header on one row after many ticks" 1
    (Vt.count_rows h.vt "Bootstrap dev");
  Alcotest.(check int) "active row once" 1 (Vt.count_rows h.vt "payloads");
  Display.finish_all h.d

let test_tty_live_rows_leave_wrap_column () =
  (* A live row must not occupy the terminal's last printable column. Some
     terminals keep a pending autowrap state after a full-width write; the next
     repaint then clears one physical row while the display consumed two. Keep a
     spare column in tty mode so every row is strictly bounded. *)
  let width = 20 in
  let h = vt_display ~width ~height:8 () in
  let _ = Display.row h.d "VMs" in
  h.sync ();
  for _ = 1 to 15 do
    Display.tick h.d;
    h.sync ()
  done;
  let rows =
    Vt.rows h.vt |> List.map strip_ansi
    |> List.filter (fun row -> contains "VMs" row)
  in
  Alcotest.(check int) "active row once" 1 (List.length rows);
  List.iter
    (fun row ->
      Alcotest.(check bool)
        "live row leaves wrap column" true
        (Width.string_width ~char_width:uucp_char_width row < width))
    rows;
  Display.finish_all h.d

let test_tty_live_accumulation_capped () =
  (* The builder's pattern (space/bin/builder_progress.ml): every finished step
     is kept in the live tree with [finish ~live:true] rather than committed, so
     the live region grows without bound. On a terminal shorter than the
     accumulated tree the uncapped renderer scrolls the header into unreachable
     scrollback and strands a duplicate. The height cap must keep the header --
     and the live phase row -- on exactly one row regardless of how many steps
     pile up. *)
  let h = vt_display ~height:8 ~header:"Bootstrap dev (arm64)" () in
  let phase = Display.row ~log_lines:4 h.d "payloads: arm64" in
  let step i =
    let r = Fmt.kstr (Display.row ~parent:phase h.d) "Step %d" i in
    Display.finish ~live:true r;
    h.sync ()
  in
  List.iter step [ 1; 2; 3; 4; 5; 6 ];
  let run = Display.row ~parent:phase ~log_lines:4 h.d "Run: builder.json" in
  List.iter (Display.log run) [ "udhcpc lease"; "runc /usr/bin/runc" ];
  h.sync ();
  Display.log phase "still working";
  h.sync ();
  Alcotest.(check int) "header once" 1 (Vt.count_rows h.vt "Bootstrap dev");
  Alcotest.(check int)
    "live phase row once" 1
    (Vt.count_rows h.vt "payloads: arm64");
  Alcotest.(check int)
    "active leaf once" 1
    (Vt.count_rows h.vt "Run: builder.json");
  Display.finish_all h.d

let test_plain_child_log_window () =
  (* In plain mode a child's log lines print append-only at the child's indent,
     interleaved with the committed rows. *)
  let d, _, out = plain_display ~header:"B" () in
  let parent = Display.row d "payloads" in
  let child = Display.row ~parent ~log_lines:3 d "Run" in
  Display.log child "compiling";
  Display.finish child;
  Display.finish parent;
  Display.finish_all d;
  let s = out () in
  Alcotest.(check bool)
    "child log at child indent" true
    (contains "    # compiling" s);
  Alcotest.(check bool) "child committed" true (contains "  => Run" s)

(* Regression tests for redraw bugs the fuzzer (ocaml-console/fuzz) found, each
   reduced to its minimal deterministic case and checked against mosaic's Vte, a
   faithful terminal emulator. *)

module D = Display

(* Drive [f] over a fresh display, mirroring its bytes into a Vte, and return
   the terminal. [sync] flushes pending output into the emulator. *)
let run ~height ~width ?header f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let clock = ref 0.0 in
  let now () =
    clock := !clock +. 0.1;
    !clock
  in
  let ctx = test_ctx ~is_tty:true ~width ~height ~ppf ~now () in
  let d = D.v ~ctx ~mode:`Console ~width ~height ?header () in
  let vte = Vte.create ~rows:height ~cols:width () in
  let sync () =
    Format.pp_print_flush ppf ();
    Vte.feed_string vte (Buffer.contents buf);
    Buffer.clear buf
  in
  f d sync;
  vte

let rows vte =
  Vte.scrollback_lines vte @ String.split_on_char '\n' (Vte.to_string vte)

let count vte needle = List.length (List.filter (contains needle) (rows vte))

(* A header with a tab rendered wider than its measured width (the tab expands
   to a tab stop), wrapping onto a second row and desyncing the redraw so the
   header duplicated. Control bytes in the header are now replaced with a space.
   (Minimal fuzz case: height 16, width 22, header "[HEADER]\t\007x".) *)
let test_header_control_char () =
  let vte =
    run ~height:16 ~width:22 ~header:"[HEADER]\t\007x" (fun d sync ->
        sync ();
        D.tick d;
        sync ();
        D.finish_all d;
        sync ())
  in
  Alcotest.(check int) "header appears once" 1 (count vte "[HEADER]")

(* A live region pinned to the bottom of the screen scrolled the header into
   scrollback on every repaint -- the trailing newline pushed one row past the
   bottom -- stranding a stale copy the cursor-relative clear could not reach.
   Ticking many times must keep the header on screen exactly once. *)
let test_no_scroll_leak () =
  let vte =
    run ~height:3 ~width:40 ~header:"BUILD" (fun d sync ->
        let r = D.row ~log_lines:2 d "compile" in
        D.log r "linking";
        for _ = 1 to 30 do
          D.tick d;
          sync ()
        done)
  in
  Alcotest.(check int) "header once after many ticks" 1 (count vte "BUILD")

(* When more rows are live than fit, the first row (the build phase / context)
   and the most recent rows stay; the middle collapses behind a marker. The
   newest is always shown -- hiding it would be the surprising thing. *)
let test_collapse_keeps_phase_and_leaf () =
  let vte =
    run ~height:6 ~width:40 ~header:"[H]" (fun d sync ->
        for i = 1 to 20 do
          Fmt.kstr (fun s -> ignore (D.row d s)) "[R%d]" i
        done;
        D.tick d;
        sync ())
  in
  let screen = String.split_on_char '\n' (Vte.to_string vte) in
  let on s = List.exists (contains s) screen in
  Alcotest.(check bool)
    "region within height" true
    (List.length (List.filter (contains "[R") screen) <= 6);
  Alcotest.(check bool) "newest row shown" true (on "[R20]");
  Alcotest.(check bool) "first (phase) row kept" true (on "[R1]");
  Alcotest.(check bool) "a middle row collapsed" true (not (on "[R10]"));
  Alcotest.(check bool) "overflow marker present" true (on "more lines)")

(* A table cell with a tab rendered wider than its column, wrapping the row.
   Control bytes in cells are now sanitized, keeping the table a rectangle. *)
let test_table_control_char () =
  let t =
    Table.of_rows
      [ Table.column "a"; Table.column "b" ]
      [
        [ Span.text "x\ty"; Span.text "z" ]; [ Span.text "pp"; Span.text "qq" ];
      ]
  in
  let lines =
    List.filter
      (fun l -> l <> "")
      (String.split_on_char '\n'
         (Table.to_string
            ~ctx:
              (test_ctx ~is_tty:true ~width:80 ~height:24
                 ~ppf:Format.std_formatter
                 ~now:(fun () -> 0.)
                 ())
            t))
  in
  match List.map (Width.string_width ~char_width:uucp_char_width) lines with
  | [] -> Alcotest.fail "table rendered nothing"
  | w :: rest ->
      List.iter
        (fun w' -> Alcotest.(check int) "every row same width" w w')
        rest

(* A banner is painted above the header as part of the live region, and is
   sampled each paint so it animates: a clock-varying banner shows a later frame
   after the clock advances. *)
let test_banner () =
  let vte =
    run ~height:8 ~width:40 ~header:"Build" (fun d sync ->
        D.set_banner d
          (Anim.v (fun ~elapsed ->
               Fmt.str "<<F%d>>" (int_of_float (elapsed *. 10.))));
        let _ = D.row d "step" in
        D.tick d;
        sync ())
  in
  let screen = String.split_on_char '\n' (Vte.to_string vte) in
  Alcotest.(check bool)
    "banner shown above header" true
    (List.exists (contains "<<F") screen
    && List.exists (contains "Build") screen)

(* {2 Logs reporter}

   A display owns the terminal, so it -- not its caller -- wraps whatever
   reporter Logs holds when it takes over, and puts the original back when it
   lets go. A log line printed under a live region has to clear and redraw it,
   and a program that only calls [Logs.set_reporter] must not have to know
   that. *)

let counting_reporter count =
  let report _src _level ~over k _msgf =
    incr count;
    over ();
    k ()
  in
  { Logs.report }

(* Logs' reporter and level are process-wide; leave them as they were found. *)
let with_logs_state f =
  let reporter = Logs.reporter () and level = Logs.level () in
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter reporter;
      Logs.set_level level)
    f

let tty_display () =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, _ = fake_clock () in
  Display.v ~ctx:(test_ctx ~is_tty:true ~ppf ~now ()) ~mode:`Console ()

let test_logs_reporter_wrapped_on_install () =
  with_logs_state @@ fun () ->
  let count = ref 0 in
  let base = counting_reporter count in
  Logs.set_reporter base;
  Logs.set_level (Some Logs.Info);
  let d = tty_display () in
  Alcotest.(check bool)
    "taking the terminal replaced the reporter" true
    (Logs.reporter () != base);
  Log.info (fun m -> m "through the wrapper");
  Alcotest.(check int) "the wrapper still delegates to the base" 1 !count;
  Display.clear d;
  Alcotest.(check bool)
    "clear restored the original reporter" true
    (Logs.reporter () == base)

let test_logs_reporter_restored_by_finish_all () =
  with_logs_state @@ fun () ->
  let base = counting_reporter (ref 0) in
  Logs.set_reporter base;
  let d = tty_display () in
  Alcotest.(check bool) "wrapped" true (Logs.reporter () != base);
  Display.finish_all d;
  Alcotest.(check bool)
    "finish_all restored the original reporter" true
    (Logs.reporter () == base)

let test_logs_reporter_untouched_in_plain_mode () =
  with_logs_state @@ fun () ->
  let base = counting_reporter (ref 0) in
  Logs.set_reporter base;
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now, _ = fake_clock () in
  let d = Display.v ~ctx:(test_ctx ~ppf ~now ()) ~mode:`Plain () in
  Alcotest.(check bool)
    "plain mode never takes the terminal, so it never wraps" true
    (Logs.reporter () == base);
  Display.clear d

(* Two displays in a row must not nest one wrapper inside another: the second
   restores the reporter the first was given, not the first's wrapper. *)
let test_logs_reporter_does_not_nest () =
  with_logs_state @@ fun () ->
  let base = counting_reporter (ref 0) in
  Logs.set_reporter base;
  let d1 = tty_display () in
  Display.clear d1;
  let d2 = tty_display () in
  Display.clear d2;
  Alcotest.(check bool)
    "reporter is the original, not a doubly-wrapped one" true
    (Logs.reporter () == base)

let suite =
  ( "display",
    [
      Alcotest.test_case "bytes" `Quick test_bytes;
      (* logs reporter *)
      Alcotest.test_case "logs reporter wrapped on install" `Quick
        test_logs_reporter_wrapped_on_install;
      Alcotest.test_case "logs reporter restored by finish_all" `Quick
        test_logs_reporter_restored_by_finish_all;
      Alcotest.test_case "logs reporter untouched in plain mode" `Quick
        test_logs_reporter_untouched_in_plain_mode;
      Alcotest.test_case "logs reporter does not nest" `Quick
        test_logs_reporter_does_not_nest;
      (* plain *)
      Alcotest.test_case "plain appends" `Quick test_plain_appends;
      Alcotest.test_case "plain no header" `Quick test_plain_no_header;
      Alcotest.test_case "plain no ansi" `Quick test_plain_no_ansi;
      Alcotest.test_case "plain count final" `Quick test_plain_count_final;
      Alcotest.test_case "plain child indent" `Quick test_plain_child_indent;
      Alcotest.test_case "plain grandchild indent" `Quick
        test_plain_grandchild_indent;
      Alcotest.test_case "plain finish message" `Quick test_plain_finish_message;
      Alcotest.test_case "plain set_label" `Quick test_plain_set_label;
      Alcotest.test_case "plain finish idempotent" `Quick
        test_plain_finish_idempotent;
      Alcotest.test_case "plain tick silent" `Quick test_plain_tick_silent;
      Alcotest.test_case "plain clear silent" `Quick test_plain_clear_silent;
      Alcotest.test_case "plain with_output runs" `Quick
        test_plain_with_output_runs;
      Alcotest.test_case "finish_all finishes rows" `Quick
        test_finish_all_finishes_rows;
      Alcotest.test_case "plain result banner" `Quick test_plain_result_banner;
      Alcotest.test_case "plain set_result" `Quick test_plain_set_result;
      Alcotest.test_case "plain set_result last wins" `Quick
        test_plain_set_result_last_wins;
      Alcotest.test_case "plain finish_all result overrides" `Quick
        test_plain_finish_all_result_overrides;
      (* style *)
      Alcotest.test_case "style default segments" `Quick
        test_style_default_segments;
      (* tty *)
      Alcotest.test_case "tty emits ansi" `Quick test_tty_emits_ansi;
      Alcotest.test_case "tty live bar" `Quick test_tty_live_bar;
      Alcotest.test_case "tty bytes rate" `Quick test_tty_bytes_rate;
      Alcotest.test_case "tty custom style" `Quick test_tty_custom_style;
      Alcotest.test_case "tty set_bar accent" `Quick test_tty_set_bar_accent;
      Alcotest.test_case "tty scroll off" `Quick test_tty_scroll_off;
      Alcotest.test_case "tty clear no commit" `Quick test_tty_clear_no_commit;
      Alcotest.test_case "tty with_output" `Quick test_tty_with_output;
      Alcotest.test_case "tty tick redraws" `Quick test_tty_tick_redraws;
      Alcotest.test_case "tty layout minimal redraw" `Quick
        test_tty_layout_minimal_redraw;
      Alcotest.test_case "tty unicode theme" `Quick test_tty_unicode_theme;
      Alcotest.test_case "tty custom theme" `Quick test_tty_custom_theme;
      Alcotest.test_case "tty log window" `Quick test_tty_log_window;
      Alcotest.test_case "tty no log without window" `Quick
        test_tty_no_log_without_window;
      Alcotest.test_case "default_row memoized" `Quick test_default_row_memoized;
      Alcotest.test_case "tty default_row window" `Quick
        test_tty_default_row_window;
      Alcotest.test_case "plain log appends" `Quick test_plain_log_appends;
      Alcotest.test_case "plain log gap" `Quick test_plain_log_gap;
      Alcotest.test_case "tty log gap" `Quick test_tty_log_gap;
      Alcotest.test_case "tty header marker" `Quick test_tty_header_marker;
      Alcotest.test_case "tty result banner" `Quick test_tty_result_banner;
      Alcotest.test_case "tty set_result banner" `Quick
        test_tty_set_result_banner;
      Alcotest.test_case "tty themed banner" `Quick test_tty_themed_banner;
      Alcotest.test_case "tty remove row" `Quick test_tty_remove_row;
      Alcotest.test_case "tty collapse no clobber" `Quick
        test_tty_collapse_no_clobber;
      (* nested rows + log windows, via terminal replay *)
      Alcotest.test_case "tty child log indent" `Quick test_tty_child_log_indent;
      Alcotest.test_case "tty finish child then log parent" `Quick
        test_tty_finish_child_then_log_parent;
      Alcotest.test_case "tty foreign write desyncs" `Quick
        test_tty_foreign_write_desyncs;
      Alcotest.test_case "tty caps to height" `Quick test_tty_caps_to_height;
      Alcotest.test_case "tty ticks at full screen" `Quick
        test_tty_ticks_at_full_screen;
      Alcotest.test_case "tty full screen stress" `Quick
        test_tty_full_screen_stress;
      Alcotest.test_case "tty wide header no wrap" `Quick
        test_tty_wide_header_no_wrap;
      Alcotest.test_case "tty live rows leave wrap column" `Quick
        test_tty_live_rows_leave_wrap_column;
      Alcotest.test_case "tty live accumulation capped" `Quick
        test_tty_live_accumulation_capped;
      Alcotest.test_case "plain child log window" `Quick
        test_plain_child_log_window;
      (* fuzzer-found redraw regressions, replayed through a Vte *)
      Alcotest.test_case "header control char" `Quick test_header_control_char;
      Alcotest.test_case "no scroll leak" `Quick test_no_scroll_leak;
      Alcotest.test_case "collapse keeps phase and leaf" `Quick
        test_collapse_keeps_phase_and_leaf;
      Alcotest.test_case "table control char" `Quick test_table_control_char;
      Alcotest.test_case "animated banner" `Quick test_banner;
    ] )
