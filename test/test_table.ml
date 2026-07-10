(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains pattern str = Re.execp (Re.compile (Re.str pattern)) str

let ctx =
  Display.ctx ~ppf:Format.std_formatter
    ~now:(fun () -> 0.)
    ~dimensions:(fun () -> (80, 24))
    ~is_tty:false ()

let ctx_tty =
  Display.ctx ~ppf:Format.std_formatter
    ~now:(fun () -> 0.)
    ~dimensions:(fun () -> (80, 24))
    ~is_tty:true ()

let test_basic () =
  let table =
    Table.(
      of_rows
        [ column "A"; column "B" ]
        [ [ Span.text "1"; Span.text "2" ]; [ Span.text "3"; Span.text "4" ] ])
  in
  let output = Table.to_string ~ctx table in
  Alcotest.(check bool) "contains A" true (contains "A" output);
  Alcotest.(check bool) "contains 1" true (contains "1" output)

let test_empty () =
  let table = Table.(v [ column "X" ]) in
  let output = Table.to_string ~ctx table in
  Alcotest.(check bool) "contains X" true (contains "X" output)

let test_truncation () =
  let table =
    Table.(
      of_rows
        [ column ~max_width:6 ~overflow:`Truncate "Header" ]
        [ [ Span.text "very long text that should be truncated" ] ])
  in
  let output = Table.to_string ~ctx table in
  (* Should contain ellipsis and not the full text *)
  Alcotest.(check bool) "contains ellipsis" true (contains "…" output);
  Alcotest.(check bool)
    "does not contain full text" false
    (contains "truncated" output)

let test_wrap () =
  let table =
    Table.(
      of_rows
        [ column ~max_width:10 ~overflow:`Wrap "Header" ]
        [ [ Span.text "hello world this wraps" ] ])
  in
  let output = Table.to_string ~ctx table in
  (* Should contain all words (wrapped to multiple lines) *)
  Alcotest.(check bool) "contains hello" true (contains "hello" output);
  Alcotest.(check bool) "contains world" true (contains "world" output);
  Alcotest.(check bool) "contains wraps" true (contains "wraps" output);
  (* Should NOT contain ellipsis since we're wrapping, not truncating *)
  Alcotest.(check bool) "no ellipsis" false (contains "…" output)

let demo () =
  Table.of_string_rows
    [ Table.column "name"; Table.column "age" ]
    [ [ "Neo"; "30" ]; [ "Trinity"; "28" ] ]

(* A still table animation is its rendered string at every frame. *)
let test_anim_const () =
  let t = demo () in
  let a = Table.anim ~ctx t in
  Alcotest.(check string)
    "frame is to_string" (Table.to_string ~ctx t)
    (Anim.frame a ~elapsed:2.5)

(* The rain frames the table green, keeps its content, and is deterministic. *)
let test_rain () =
  let a = Table.rain ~ctx (demo ()) in
  let f = Anim.frame a ~elapsed:0.4 in
  Alcotest.(check bool) "green frame" true (contains "\027[32m" f);
  Alcotest.(check bool) "content kept" true (contains "Trinity" f);
  Alcotest.(check string) "deterministic" f (Anim.frame a ~elapsed:0.4);
  Alcotest.(check bool)
    "advances over time" true
    (f <> Anim.frame a ~elapsed:1.9)

(* [is_tty] gates ANSI exactly as it gates width: a table with a bold header (the
   default) writes escapes to a terminal but plain text to a pipe or file, the
   same contract Display rows already honour. *)
let test_is_tty_gates_ansi () =
  let t =
    Table.of_string_rows [ Table.column "A"; Table.column "B" ] [ [ "1"; "2" ] ]
  in
  let has_esc s = String.contains s '\027' in
  Alcotest.(check bool)
    "tty keeps ANSI" true
    (has_esc (Table.to_string ~ctx:ctx_tty t));
  Alcotest.(check bool)
    "piped is plain" false
    (has_esc (Table.to_string ~ctx t))

(* A [~shrinkable:false] column keeps its full width when the table is fitted to
   a narrow terminal; a shrinkable neighbour gives way instead. *)
let test_shrinkable_protects_column () =
  let ctx_narrow =
    Display.ctx ~ppf:Format.std_formatter
      ~now:(fun () -> 0.)
      ~dimensions:(fun () -> (24, 24))
      ~is_tty:true ()
  in
  let long_name = "a-very-long-package-name" in
  let detail = "detail text that must give way first" in
  let protected =
    Table.(
      of_rows
        [
          column ~shrinkable:false "Package";
          column ~overflow:`Truncate "Detail";
        ]
        [ [ Span.text long_name; Span.text detail ] ])
  in
  Alcotest.(check bool)
    "unshrinkable package name is kept in full" true
    (contains long_name (Table.to_string ~ctx:ctx_narrow protected));
  let shrinkable =
    Table.(
      of_rows
        [ column ~overflow:`Truncate "Package"; column "Detail" ]
        [ [ Span.text long_name; Span.text detail ] ])
  in
  Alcotest.(check bool)
    "a shrinkable package name is cut at the same width" false
    (contains long_name (Table.to_string ~ctx:ctx_narrow shrinkable))

let suite =
  ( "table",
    [
      Alcotest.test_case "basic" `Quick test_basic;
      Alcotest.test_case "shrinkable protects column" `Quick
        test_shrinkable_protects_column;
      Alcotest.test_case "is_tty gates ANSI" `Quick test_is_tty_gates_ansi;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "truncation" `Quick test_truncation;
      Alcotest.test_case "wrap" `Quick test_wrap;
      Alcotest.test_case "anim is still" `Quick test_anim_const;
      Alcotest.test_case "matrix rain" `Quick test_rain;
    ] )
