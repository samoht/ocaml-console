(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* {1 The width policy, from uuseg and uucp} *)

let uchars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length d) (Uchar.utf_decode_uchar d :: acc)
  in
  go 0 []

let cells u = max 0 (Uucp.Break.tty_width_hint u)
let is_spacing_mark u = Uucp.Gc.general_category u = `Mc

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
    "\u{FFFD}"; "\xff";
  |]
[@@ocamlformat "disable"]

let text =
  map
    [ list uint8 ]
    (fun l ->
      String.concat ""
        (List.map (fun k -> alphabet.(k mod Array.length alphabet)) l))

(* Console's width is the policy's, on any bytes. *)
let test_policy s =
  check_eq ~pp:Fmt.int (policy_width s) (Width.string_width s);
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    (policy_clusters (Console_text.sanitize s))
    (Width.clusters (Console_text.sanitize s))

(* The width of a part is that of the part as a string. *)
let test_sub s a b =
  let n = String.length s in
  let pos = if n = 0 then 0 else a mod (n + 1) in
  let len = if n - pos = 0 then 0 else b mod (n - pos + 1) in
  check_eq ~pp:Fmt.int
    (Width.string_width (String.sub s pos len))
    (Width.sub_width s ~pos ~len)

(* {1 The test terminal} *)

(* For printable UTF-8, the width is the column a terminal's cursor reaches
   after the same bytes. *)
let test_matches_terminal s =
  let s = Console_text.sanitize ~keep_newlines:false s in
  guard (Width.string_width s < 1000);
  let vte = Console_vte.v ~rows:1 ~cols:1000 () in
  Console_vte.feed vte s;
  check_eq ~pp:Fmt.int (snd (Console_vte.cursor_pos vte)) (Width.string_width s)

let suite =
  ( "width",
    [
      test_case "follows the policy" [ bytes ] test_policy;
      test_case "follows the policy, on clusters" [ text ] test_policy;
      test_case "of a part" [ text; uint8; uint8 ] test_sub;
      test_case "matches the terminal" [ bytes ] test_matches_terminal;
    ] )
