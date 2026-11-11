(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let uchars s = Array.to_list (Display_harness.glyphs s)
let cells u = max 0 (Uucp.Break.tty_width_hint u)

let is_spacing_mark u =
  Uucp.Gc.general_category u = `Mc || Uucp.Break.grapheme_cluster u = `SM

let is_regional_indicator u =
  0x1f1e6 <= Uchar.to_int u && Uchar.to_int u <= 0x1f1ff

(* An emoji that is text by default, which a VS16 right after it shows as
   an emoji. *)
let is_text_emoji u =
  Uucp.Emoji.is_emoji u && not (Uucp.Emoji.is_emoji_presentation u)

let rec selects_emoji = function
  | a :: (b :: _ as rest) ->
      (Uchar.to_int b = 0xfe0f && is_text_emoji a) || selects_emoji rest
  | _ -> false

(* A cluster takes the cells of its widest character, two if it is an
   emoji sequence (a flag, or a VS16 after a text emoji), plus the cells of
   each spacing mark after its first character. *)
let cluster_cells c =
  match uchars c with
  | [] -> 0
  | first :: rest as all ->
      let marks, others = List.partition is_spacing_mark rest in
      let widest =
        List.fold_left (fun n u -> max n (cells u)) 0 (first :: others)
      in
      let emoji =
        selects_emoji all
        || List.length (List.filter is_regional_indicator all) = 2
      in
      (if emoji then max widest 2 else widest)
      + List.fold_left (fun n u -> n + cells u) 0 marks

let policy_width s =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun n c -> n + cluster_cells c) 0 s

let policy_clusters s =
  List.rev (Uuseg_string.fold_utf_8 `Grapheme_cluster (fun l c -> c :: l) [] s)

(* Characters of every kind the policy names, to build text of clusters
   that bytes alone rarely make. *)
let alphabet =
  [|
    "a"; "1"; "#"; " "; "\u{00E9}"; "\u{0301}"; "\u{754C}"; "\u{0915}";
    "\u{093F}"; "\u{0940}"; "\u{094D}"; "\u{0937}"; "\u{09BE}"; "\u{0D4E}";
    "\u{1F389}"; "\u{1F44D}"; "\u{1F3FD}"; "\u{261D}"; "\u{1F1EB}";
    "\u{1F1F7}"; "\u{2764}"; "\u{203C}"; "\u{2600}"; "\u{00A9}"; "\u{FE0F}";
    "\u{FE0E}"; "\u{200D}"; "\u{20E3}"; "\u{302E}"; "\u{1100}"; "\u{1161}";
    "\u{11A8}"; "\u{200B}"; "\u{00AD}"; "\u{231A}"; "\u{1F468}"; "\u{0600}";
    "\u{FFFD}"; "\xff"; "\u{0E01}"; "\u{0E33}"; "\u{0EB3}"; "\u{1F3F4}";
    "\u{E0067}"; "\u{E007F}"; "\u{2501}"; "\u{2578}"; "\u{2500}";
    "\u{2588}"; "\u{2591}"; "\u{2550}";
  |]
[@@ocamlformat "disable"]

let text =
  map
    [ list uint8 ]
    (fun l ->
      String.concat ""
        (List.map (fun k -> alphabet.(k mod Array.length alphabet)) l))

(* Text of runs of one character, as bars and rules draw. *)
let runs =
  map
    [ list (pair uint8 uint8) ]
    (fun l ->
      String.concat ""
        (List.map
           (fun (k, n) ->
             String.concat ""
               (List.init (n mod 9) (fun _ ->
                    alphabet.(k mod Array.length alphabet))))
           l))

(* [string_width] follows the policy above on any bytes, and [clusters]
   gives its clusters on sanitized text. *)
let test_policy s =
  check_eq ~pp:Fmt.int (policy_width s) (Console_width.string_width s);
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    (policy_clusters (Console_text.sanitize s))
    (Console_width.clusters (Console_text.sanitize s))

(* [sub_width s ~pos ~len] is [string_width (String.sub s pos len)]. *)
let test_sub s a b =
  let n = String.length s in
  let pos = if n = 0 then 0 else a mod (n + 1) in
  let len = if n - pos = 0 then 0 else b mod (n - pos + 1) in
  check_eq ~pp:Fmt.int
    (Console_width.string_width (String.sub s pos len))
    (Console_width.sub_width s ~pos ~len)

(* Text that does not join the text before it adds its cells and its
   clusters to those of that text. *)
let test_joins a b =
  let a = Console_text.sanitize a and b = Console_text.sanitize b in
  if not (Console_width.joins a b) then begin
    check_eq ~pp:Fmt.int
      (Console_width.string_width a + Console_width.string_width b)
      (Console_width.string_width (a ^ b));
    check_eq
      ~pp:Fmt.(Dump.list Dump.string)
      (Console_width.clusters a @ Console_width.clusters b)
      (Console_width.clusters (a ^ b))
  end

(* For printable UTF-8, the width is the column a terminal's cursor reaches
   after the same bytes. *)
let test_matches_terminal s =
  let s = Console_text.sanitize ~keep_newlines:false s in
  guard (Console_width.string_width s < 1000);
  guard (Console_vte.counts_as_console s);
  let vte = Console_vte.v ~rows:1 ~cols:1000 () in
  Console_vte.feed vte s;
  check_eq ~pp:Fmt.int
    (snd (Console_vte.cursor_pos vte))
    (Console_width.string_width s)

let suite =
  ( "width",
    [
      test_case "follows the policy" [ bytes ] test_policy;
      test_case "follows the policy, on clusters" [ text ] test_policy;
      test_case "follows the policy, on runs" [ runs ] test_policy;
      test_case "of a part" [ text; uint8; uint8 ] test_sub;
      test_case "of a part of runs" [ runs; uint8; uint8 ] test_sub;
      test_case "joins" [ runs; runs ] test_joins;
      test_case "joins, on bytes" [ bytes; bytes ] test_joins;
      test_case "matches the terminal" [ bytes ] test_matches_terminal;
    ] )
