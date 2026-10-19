(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let of_string_rows ?theme ?border columns rows =
  Table.of_rows ?theme ?border columns (List.map (List.map Span.text) rows)

let contains pattern str = Re.execp (Re.compile (Re.str pattern)) str

let test_basic () =
  let table =
    Table.(
      of_rows
        [ column "A"; column "B" ]
        [ [ Span.text "1"; Span.text "2" ]; [ Span.text "3"; Span.text "4" ] ])
  in
  let output = Table.to_string table in
  Alcotest.(check bool) "contains A" true (contains "A" output);
  Alcotest.(check bool) "contains 1" true (contains "1" output)

let test_empty () =
  let table = Table.(of_rows [ column "X" ] []) in
  let output = Table.to_string table in
  Alcotest.(check bool) "contains X" true (contains "X" output)

let test_wrap () =
  let table =
    Table.(
      of_rows
        [ column ~max_width:10 ~overflow:`Wrap "Header" ]
        [ [ Span.text "hello world this wraps" ] ])
  in
  let output = Table.to_string table in
  (* Should contain all words (wrapped to multiple lines) *)
  Alcotest.(check bool) "contains hello" true (contains "hello" output);
  Alcotest.(check bool) "contains world" true (contains "world" output);
  Alcotest.(check bool) "contains wraps" true (contains "wraps" output);
  (* Should NOT contain ellipsis since we're wrapping, not truncating *)
  Alcotest.(check bool) "no ellipsis" false (contains "…" output)

(* A URL is one selectable terminal token. When another column can wrap at
   whitespace, fitting gives that slack up before splitting the URL at a slash.
   The hard terminal-width bound still wins when no such slack remains. *)
let test_wrap_preserves_unbroken_token () =
  let url = "https://rg.fr-par.scw.cloud/run-space" in
  let table =
    of_string_rows
      [
        Table.column "Context"; Table.column "Service"; Table.column "Endpoint";
      ]
      [ [ "infra  ACTIVE"; "Scheduler"; url ] ]
  in
  let natural_width = 10 + 13 + 9 + String.length url in
  let output = Table.to_string ~width:(natural_width - 1) table in
  Alcotest.(check bool) "URL remains contiguous" true (contains url output)

(* String conversion is plain. The formatter passed to [pp], not terminal
   detection, owns colour policy. *)
let test_string_conversion_is_plain () =
  let t =
    of_string_rows [ Table.column "A"; Table.column "B" ] [ [ "1"; "2" ] ]
  in
  let has_esc s = String.contains s '\027' in
  Alcotest.(check bool)
    "tty string is plain" false
    (has_esc (Table.to_string t));
  Alcotest.(check bool) "piped is plain" false (has_esc (Table.to_string t))

let render renderer table =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf renderer;
  Table.pp ppf table;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_formatter_gates_cell_styles () =
  let table =
    Table.of_rows
      [ Table.column "A" ]
      [ [ Span.styled Style.(fg Color.red) "red" ] ]
  in
  Alcotest.(check bool)
    "plain formatter has no escapes" false
    (String.contains (render `None table) '\027');
  Alcotest.(check bool)
    "ANSI formatter keeps cell style" true
    (contains "\027[31m" (render `Ansi_tty table))

let test_link_cell () =
  let table =
    Table.of_rows
      [ Table.column "A" ]
      [ [ Span.link ~uri:"https://example.com" "docs" ] ]
  in
  Alcotest.(check bool)
    "plain keeps label" true
    (contains "docs" (render `None table));
  let ansi = render `Ansi_tty table in
  Alcotest.(check bool) "ANSI keeps OSC 8" true (contains "\027]8;;" ansi)

let test_formatter_margin_never_wraps_border () =
  let width = 48 in
  let table =
    of_string_rows ~border:Border.rounded
      [
        Table.column ~shrinkable:false "Package";
        Table.column ~overflow:`Wrap "Remote";
      ]
      [
        [ "ocaml-parquet"; "git@git.recoil.org:gazagnaire.org/ocaml-parquet" ];
        [ "ocaml-sqlite"; "git@git.recoil.org:gazagnaire.org/ocaml-sqlite" ];
      ]
  in
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin ppf width;
  Fmt.set_style_renderer ppf `Ansi_tty;
  Table.pp ppf table;
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents buffer in
  let logical_rows =
    raw |> String.split_on_char '\n'
    |> List.filter (fun row -> String.trim row <> "")
  in
  List.iter
    (fun row ->
      Alcotest.(check int)
        "raw row reaches margin" width (Width.string_width row))
    logical_rows;
  let vte = Vte.create ~rows:12 ~cols:width () in
  Vte.feed_string vte raw;
  let rows =
    Vte.to_string vte |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (fun row -> row <> "")
  in
  let left_edge row =
    List.exists
      (fun prefix -> String.starts_with ~prefix row)
      [ "╭"; "│"; "├"; "╰" ]
  in
  let right_edge row =
    List.exists
      (fun suffix -> String.ends_with ~suffix row)
      [ "╮"; "│"; "┤"; "╯" ]
  in
  Alcotest.(check bool)
    "every physical row retains both borders" true
    (List.for_all (fun row -> left_edge row && right_edge row) rows);
  Alcotest.(check int)
    "no wrapped physical rows" (List.length logical_rows) (List.length rows);
  List.iter
    (fun row ->
      Alcotest.(check int)
        "visual row reaches margin" width (Width.string_width row))
    rows;
  Alcotest.(check int)
    "table does not enter scrollback" 0 (Vte.scrollback_size vte);
  Alcotest.(check (pair int int))
    "cursor begins the next physical row"
    (List.length logical_rows, 0)
    (Vte.cursor_pos vte)

(* Every practical narrow width is a hard limit. Protected columns, preferred minima, wide glyphs, combining
   marks, control bytes and malformed UTF-8 are all incapable of overriding the
   terminal margin. This catches the whole right-border-wrap family, including
   one-cell errors at column-selection boundaries. *)
let test_every_margin_is_a_hard_limit () =
  let hostile =
    [
      "\xe4\xbd\xa0\xe5\xa5\xbd";
      "e\xcc\x81\xcc\x82";
      "\xff\xfe\xfd";
      "\t\007control";
      String.make 200 'W';
    ]
  in
  let columns =
    List.mapi
      (fun i header ->
        Table.column ~min_width:12
          ~shrinkable:(i mod 2 = 0)
          ~overflow:(if i mod 2 = 0 then `Wrap else `Elide)
          header)
      hostile
  in
  let table = of_string_rows ~border:Border.rounded columns [ hostile ] in
  for width = 4 to 80 do
    let buffer = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer buffer in
    Format.pp_set_margin ppf width;
    Fmt.set_style_renderer ppf `Ansi_tty;
    Table.pp ppf table;
    Format.pp_print_flush ppf ();
    let raw = Buffer.contents buffer in
    let logical_rows =
      raw |> String.split_on_char '\n'
      |> List.filter (fun row -> String.trim row <> "")
    in
    Alcotest.(check bool)
      (Fmt.str "width %d rendered a table" width)
      true (logical_rows <> []);
    List.iter
      (fun row ->
        Alcotest.(check int)
          (Fmt.str "raw row at width %d" width)
          width (Width.string_width row))
      logical_rows;
    let vte = Vte.create ~rows:(List.length logical_rows + 2) ~cols:width () in
    Vte.feed_string vte raw;
    let visual_rows =
      Vte.to_string vte |> String.split_on_char '\n'
      |> List.filter (fun row -> String.trim row <> "")
    in
    Alcotest.(check int)
      (Fmt.str "physical row count at width %d" width)
      (List.length logical_rows) (List.length visual_rows);
    Alcotest.(check int)
      (Fmt.str "no scrollback at width %d" width)
      0 (Vte.scrollback_size vte);
    List.iter
      (fun row ->
        Alcotest.(check bool)
          (Fmt.str "left border at width %d" width)
          true
          (List.exists
             (fun edge -> String.starts_with ~prefix:edge row)
             [ "╭"; "│"; "├"; "╰" ]);
        Alcotest.(check bool)
          (Fmt.str "right border at width %d" width)
          true
          (List.exists
             (fun edge -> String.ends_with ~suffix:edge row)
             [ "╮"; "│"; "┤"; "╯" ]);
        Alcotest.(check int)
          (Fmt.str "visual row at width %d" width)
          width (Width.string_width row))
      visual_rows;
    Alcotest.(check (pair int int))
      (Fmt.str "cursor at width %d" width)
      (List.length logical_rows, 0)
      (Vte.cursor_pos vte)
  done

let test_invalid_shape () =
  Alcotest.check_raises "negative minimum width"
    (Invalid_argument "Console.Table.column: negative min_width") (fun () ->
      ignore (Table.column ~min_width:(-1) "A"));
  Alcotest.check_raises "minimum exceeds maximum"
    (Invalid_argument "Console.Table.column: min_width exceeds max_width")
    (fun () -> ignore (Table.column ~min_width:3 ~max_width:2 "A"));
  let columns = [ Table.column "A"; Table.column "B" ] in
  Alcotest.check_raises "too many cells"
    (Invalid_argument "Console.Table.of_rows: expected 1 to 2 cells, got 3")
    (fun () ->
      ignore (Table.of_rows columns [ List.map Span.text [ "a"; "b"; "c" ] ]));
  Alcotest.check_raises "no cell"
    (Invalid_argument "Console.Table.of_rows: expected 1 to 2 cells, got 0")
    (fun () -> ignore (Table.of_rows columns [ [] ]))

(* A [~shrinkable:false] column keeps its full width when the table is fitted to
   a narrow terminal; a shrinkable neighbour gives way instead. *)
let test_shrinkable_protects_column () =
  let long_name = "a-very-long-package-name" in
  let detail = "1234567890" in
  let protected =
    Table.(
      of_rows
        [ column ~shrinkable:false "Package"; column ~overflow:`Elide "Detail" ]
        [ [ Span.text long_name; Span.text detail ] ])
  in
  Alcotest.(check bool)
    "unshrinkable package name is kept in full" true
    (contains long_name (Table.to_string ~width:35 protected));
  let shrinkable =
    Table.(
      of_rows
        [ column ~overflow:`Elide "Package"; column "Detail" ]
        [ [ Span.text long_name; Span.text detail ] ])
  in
  Alcotest.(check bool)
    "a shrinkable package name is cut at the same width" false
    (contains long_name (Table.to_string ~width:35 shrinkable))

(* A border with no glyphs draws no edge either: a row starts at its first
   cell and ends at its last cell's text, so a listing pipes as it prints. A
   right-aligned last column keeps its padding, which is on its left. *)
let test_no_border_draws_no_edge () =
  let lines table =
    Table.to_string table |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")
  in
  Alcotest.(check (list string))
    "each row starts at its first cell and ends at its last"
    [ "NAME         SIZE"; "alpha        1"; "beta-longer  22" ]
    (lines
       (of_string_rows ~border:Border.none
          [ Table.column "NAME"; Table.column "SIZE" ]
          [ [ "alpha"; "1" ]; [ "beta-longer"; "22" ] ]));
  Alcotest.(check (list string))
    "a right-aligned last column is padded on its left"
    [ "NAME  SIZE"; "a        1"; "b       22" ]
    (lines
       (of_string_rows ~border:Border.none
          [ Table.column "NAME"; Table.column ~align:`Right "SIZE" ]
          [ [ "a"; "1" ]; [ "b"; "22" ] ]));
  Alcotest.(check (list string))
    "a cell of fewer lines than its row ends where its text ends"
    [ "NAME  NOTE"; "a     one"; "      two" ]
    (lines
       (of_string_rows ~border:Border.none
          [ Table.column "NAME"; Table.column "NOTE" ]
          [ [ "a"; "one\ntwo" ] ]));
  let col = Table.column "" in
  Alcotest.(check string)
    "a line ends at its last cell that is not empty"
    "fix   stuck  /pool\nmain\n"
    (Table.to_string
       (of_string_rows ~border:Border.none [ col; col; col ]
          [ [ "fix"; "stuck"; "/pool" ]; [ "main"; ""; "" ] ]))

(* A border in one style is one sequence per run of border glyphs. *)
let test_styled_border_runs () =
  let t =
    Table.of_rows
      ~border:(Border.with_style Style.(fg Color.red) Border.single)
      [ Table.column "a" ]
      [ [ Span.text "b" ] ]
  in
  let lines = String.split_on_char '\n' (Table.to_string ~styled:true t) in
  Alcotest.(check string)
    "top"
    "\027[31m\xe2\x94\x8c\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x90\027[0m"
    (List.hd lines);
  Alcotest.(check string)
    "data row" "\027[31m\xe2\x94\x82\027[0m b \027[31m\xe2\x94\x82\027[0m"
    (List.nth lines 3)

let suite =
  ( "table",
    [
      Alcotest.test_case "styled border runs" `Quick test_styled_border_runs;
      Alcotest.test_case "no border draws no edge" `Quick
        test_no_border_draws_no_edge;
      Alcotest.test_case "basic" `Quick test_basic;
      Alcotest.test_case "shrinkable protects column" `Quick
        test_shrinkable_protects_column;
      Alcotest.test_case "string conversion is plain" `Quick
        test_string_conversion_is_plain;
      Alcotest.test_case "formatter gates cell styles" `Quick
        test_formatter_gates_cell_styles;
      Alcotest.test_case "link cell" `Quick test_link_cell;
      Alcotest.test_case "formatter margin keeps borders on one row" `Quick
        test_formatter_margin_never_wraps_border;
      Alcotest.test_case "every formatter margin is a hard limit" `Quick
        test_every_margin_is_a_hard_limit;
      Alcotest.test_case "invalid shape" `Quick test_invalid_shape;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "wrap" `Quick test_wrap;
      Alcotest.test_case "wrap preserves an unbroken token" `Quick
        test_wrap_preserves_unbroken_token;
    ] )
