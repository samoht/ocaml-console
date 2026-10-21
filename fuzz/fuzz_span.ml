(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* Text whose clusters, cells and bytes do not line up: wide characters, a
   spacing mark that joins the letter before it in a cluster of two cells,
   an emoji sequence, spaces, punctuation and slashes. *)
let alphabet =
  [|
    "x";
    "\xe7\x95\x8c";
    "\xc3\xa9";
    "\xea\xa8\xb4";
    "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9";
    " ";
    "  ";
    ",";
    "/";
    "\n";
  |]

let text s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         alphabet.(Char.code s.[i] mod Array.length alphabet)))

(* A span in several styles, cut between characters where the generator
   says. *)
let span s =
  let styles =
    [| Style.none; Style.bold; Style.fg Color.red; Style.underline |]
  in
  let n = String.length s in
  let rec go i acc =
    if i >= n then acc
    else
      let rec char_end j k =
        if j >= n || k = 0 then j
        else
          char_end
            (j + Uchar.utf_decode_length (String.get_utf_8_uchar s j))
            (k - 1)
      in
      let j = char_end i (1 + (Char.code s.[i] mod 3)) in
      go j
        Span.(
          acc
          ++ Span.styled styles.(Char.code s.[i] mod 4) (String.sub s i (j - i)))
  in
  go 0 Span.empty

let width n = 1 + (abs n mod 40)
let plain = Span.to_string

let no_space s =
  String.concat ""
    (String.split_on_char ' ' (String.concat "" (String.split_on_char '\n' s)))

(* Every span prints, whatever its bytes: a control is a space and
   malformed UTF-8 the replacement character. *)
let test_sanitized s =
  let p = plain (Span.text s) in
  check (String.is_valid_utf_8 p);
  String.iter
    (fun c ->
      let c = Char.code c in
      if (c < 0x20 && c <> 0x0a) || c = 0x7f then failf "control %d in %S" c p)
    p;
  ignore (Span.to_string ~styled:true (Span.styled Style.bold s))

(* [width (s0 ++ s1) <= width s0 + width s1]. *)
let test_width_subadditive a b =
  let a = span (text a) and b = span (text b) in
  check (Span.width Span.(a ++ b) <= Span.width a + Span.width b)

(* A style boundary adds no cell: the styled span is as wide as its text. *)
let test_width_styles s =
  let s = text s in
  check_eq ~pp:Fmt.int (Span.width (Span.text s)) (Span.width (span s))

(* [width (truncate w s) <= max 0 w], [truncate w s = s] when it fits, a cut
   line ends with an ellipsis, and what is kept is a prefix of the line. *)
let test_truncate s n =
  let s = span (text s) and w = abs n mod 50 in
  let t = Span.truncate w s in
  check (Span.width t <= max 0 w);
  if w > 0 && Span.width s <= w then
    check_eq ~pp:Fmt.Dump.string (plain s) (plain t);
  let lines = String.split_on_char '\n' (plain s)
  and cut = String.split_on_char '\n' (plain t) in
  if w > 0 then
    List.iter2
      (fun l c ->
        if not (String.equal l c) then begin
          let e = "\xe2\x80\xa6" in
          if not (String.ends_with ~suffix:e c) then failf "no ellipsis: %S" c;
          let kept = String.sub c 0 (String.length c - String.length e) in
          if not (String.starts_with ~prefix:kept l) then
            failf "%S is not a prefix of %S" kept l
        end)
      lines cut;
  check_eq ~pp:Fmt.Dump.string (plain t) (plain (Span.truncate w t))

(* The styled form of a cut span turns every style off. *)
let test_truncate_closes s n =
  let t =
    Span.to_string ~styled:true (Span.truncate (width n) (span (text s)))
  in
  (* The last escape code written is the reset. *)
  match String.rindex_opt t '\027' with
  | None -> ()
  | Some i ->
      let rest = String.sub t i (String.length t - i) in
      if not (String.starts_with ~prefix:"\027[0m" rest) then
        failf "a style left on: %S" t

(* Every line fits, the result is never empty, and with room for any
   character every non-space one is kept, in order. *)
(* The cells of the widest cluster of [s]. *)
let widest s =
  let w = ref 0 in
  Matrix_text.iter_graphemes
    (fun ~offset ~len ->
      w := max !w (Width.string_width (String.sub s offset len)))
    s;
  !w

(* Every character is kept but those wider than a line, which cannot be
   drawn. *)
let test_wrap s n =
  let s = span (text s) and w = width n in
  let lines = Span.wrap w s in
  check (lines <> []);
  List.iter (fun l -> check (Span.width l <= w)) lines;
  if w >= widest (plain s) then
    check_eq ~pp:Fmt.Dump.string
      (no_space (plain s))
      (no_space (String.concat "" (List.map plain lines)))

(* Characters that change the width of the one before them, or join it:
   VS16 and VS15, the zero-width joiner, a combining mark, the keycap mark
   and a skin tone, with the bases they act on, a regional indicator and
   letters. *)
let joiners =
  [|
    "a";
    "b";
    "1";
    "\u{fe0f}";
    "\u{fe0e}";
    "\u{200d}";
    "\u{301}";
    "\u{20e3}";
    "\u{1f3fd}";
    "\u{2764}";
    "\u{1f468}";
    "\u{1f469}";
    "\u{754c}";
    "\u{1f1eb}";
    " ";
  |]

let joined s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         joiners.(Char.code s.[i] mod Array.length joiners)))

(* The column a terminal's cursor reaches after [s]. *)
let on_screen s =
  let term = Console_vte.v ~rows:1 ~cols:500 () in
  Console_vte.feed term s;
  snd (Console_vte.cursor_pos term)

(* With VS16, VS15, joiners and marks, a cut line and every wrapped line
   fit their width, as console measures it and on the terminal. *)
let test_joiners_fit s n =
  let s = Span.text (joined s) and w = 1 + (abs n mod 12) in
  let fits what l =
    let cells = on_screen (plain l) in
    if Span.width l > w || cells > w then
      failf "%s to %d is %d cells, %d on the terminal: %S" what w (Span.width l)
        cells (plain l)
  in
  fits "truncated" (Span.truncate w s);
  List.iter (fits "wrapped") (Span.wrap w s)

let test_wrap_bytes s n = ignore (Span.wrap (width n) (Span.text s))

let test_wrap_refuses n =
  let w = -abs n in
  match Span.wrap w Span.empty with
  | _ -> fail "a non-positive width is refused"
  | exception Invalid_argument _ -> ()

(* RFC 3986, sections 2.1 to 2.3: the characters a URI may hold, the
   percent sign of an escape included. *)
let uri_char = function
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | ':' | '/' | '?' | '#' | '[' | ']' | '@' | '!' | '$'
  | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '%' ->
      true
  | _ -> false

(* A link's label is all its plain form, and the target it writes holds
   only the characters of a URI, as many escapes as [uri] has bytes outside
   them; an empty URI writes no link, and neither does a label of line ends
   alone, which has no text to click. *)
let test_link label uri =
  let l = Span.link ~uri label in
  check_eq ~pp:Fmt.Dump.string (plain (Span.text label)) (plain l);
  let ansi = Span.to_string ~styled:true l in
  match Re.exec_opt (Re.compile (Re.str "\027]8;;")) ansi with
  | None ->
      if uri <> "" && String.exists (( <> ) '\n') (plain l) then
        fail "no link written"
  | Some g ->
      if uri = "" then fail "an empty URI linked";
      let i = Re.Group.stop g 0 in
      let rest = String.sub ansi i (String.length ansi - i) in
      let target = String.sub rest 0 (String.index rest '\027') in
      if not (String.for_all uri_char target) then fail "unsafe URI written";
      let outside = ref 0 in
      String.iter (fun c -> if not (uri_char c) then incr outside) uri;
      check_eq ~pp:Fmt.int
        (String.length uri + (2 * !outside))
        (String.length target)

(* [concat ~sep] puts [sep] between each two. *)
let test_concat a b c =
  let parts = List.map Span.text [ a; b; c ] in
  check_eq ~pp:Fmt.Dump.string
    (String.concat "," (List.map plain parts))
    (plain (Span.concat ~sep:(Span.text ",") parts))

(* Each line must release its attributes before another writer can print. *)
let test_multiline_resets s =
  let line = Console_text.sanitize ~keep_newlines:false s ^ "x" in
  let raw =
    Span.to_string ~styled:true (Span.styled Style.bold (line ^ "\n" ^ line))
  in
  List.iter
    (fun l ->
      check (String.starts_with ~prefix:"\027[1m" l);
      check (String.ends_with ~suffix:"\027[0m" l))
    (String.split_on_char '\n' raw)

let test_linked_unicode_cuts s n =
  let line = Console_text.sanitize ~keep_newlines:false s in
  let source =
    Span.(
      styled Style.bold "\u{754c}\u{1f1eb}\u{1f1f7}"
      ++ link ~style:(Style.fg Color.red) ~uri:"https://example.invalid"
           ("\u{1f469}\u{200d}\u{1f4bb}" ^ line)
      ++ styled Style.underline "e\u{0301}\u{2764}\u{fe0f}")
  in
  let w = n mod 80 in
  let fitted = Span.truncate w source in
  let raw = Span.to_string ~styled:true fitted in
  check (String.is_valid_utf_8 raw);
  check (Span.width fitted <= w);
  let escapes =
    Re.compile
      (Re.alt
         [
           Re.seq
             [
               Re.str "\027[";
               Re.rep (Re.alt [ Re.rg '0' '9'; Re.char ';' ]);
               Re.char 'm';
             ];
           Re.seq [ Re.str "\027]8;;"; Re.rep (Re.rg ' ' '~'); Re.str "\027\\" ];
         ])
  in
  let stripped = Re.replace_string escapes ~by:"" raw in
  check_eq ~pp:Fmt.Dump.string (Span.to_string fitted) stripped;
  let occurrences needle =
    let re = Re.compile (Re.str needle) in
    List.length (Re.all re raw)
  in
  check_eq ~pp:Fmt.int
    (occurrences "\027]8;;https://example.invalid\027\\")
    (occurrences "\027]8;;\027\\");
  let vte = Console_vte.v ~rows:1 ~cols:81 () in
  Console_vte.feed vte raw;
  check_eq ~pp:Fmt.int (Span.width fitted) (snd (Console_vte.cursor_pos vte))

(* A gradient is as wide as its text, which it is without colour; in
   colour, at any depth and in any theme, it is that text and escape codes. *)
(* [span] is as wide as [t] and prints as [t]'s text in [theme], without
   colour and, its escapes taken out, at every depth. *)
let check_is_text ~theme t span =
  check_eq ~pp:Fmt.int (Span.width t) (Span.width span);
  check_eq ~pp:Fmt.Dump.string (plain t) (plain span);
  let printed depth =
    let buf = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    Option.iter
      (fun depth ->
        Fmt.set_style_renderer ppf `Ansi_tty;
        Color.set_depth ppf depth)
      depth;
    Span.pp ppf span;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  check_eq ~pp:Fmt.Dump.string (plain t) (printed None);
  let sgr =
    Re.compile (Re.seq [ Re.str "\027["; Re.rep (Re.rg '0' ';'); Re.char 'm' ])
  in
  List.iter
    (fun depth ->
      check_eq ~pp:Fmt.Dump.string (plain t)
        (Re.replace_string sgr ~by:"" (printed (Some depth))))
    [ `Ansi_16; `Ansi_256; `True_color ]

let theme_of k = snd (List.nth Theme.all (k mod List.length Theme.all))

let test_gradient s k =
  let s = if k land 1 = 0 then text s else s in
  let colors =
    match k / 2 mod 4 with
    | 0 -> None
    | n ->
        Some
          (List.init n (fun i ->
               if i = 0 then Color.palette k
               else Color.rgb (k * (i + 1) land 255) (k * 7 land 255) (255 - k)))
  in
  check_is_text ~theme:(theme_of k) (Span.text s) (Span.gradient ?colors s)

let roles = [| `Accent; `Ok; `Warn; `Fail; `Muted; `Header |]

let test_role s k =
  let s = if k land 1 = 0 then text s else s in
  let role = roles.(k / 2 mod Array.length roles) in
  check_is_text ~theme:(theme_of k) (Span.text s) (Span.role role s)

(* Fitting a gradient changes line breaks, not the colours of the text
   kept. Compare terminal cells after joining the wrapped pieces again. *)
let test_gradient_wrap s k =
  let clusters =
    [| "x"; "\u{754c}"; "e\u{0301}"; "\u{1f469}\u{200d}\u{1f4bb}"; "\u{0645}" |]
  in
  let input =
    String.concat ""
      (List.init
         (min 64 (String.length s))
         (fun i -> clusters.(Char.code s.[i] mod Array.length clusters)))
  in
  let gradient = Span.with_style Style.bold (Span.gradient input) in
  let theme = snd (List.nth Theme.all (k mod List.length Theme.all)) in
  let render span =
    let buf = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color;
    Span.pp ppf span;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let width = 2 + (k mod 11) in
  let pieces = Span.wrap width gradient in
  List.iter (fun piece -> check (Span.width piece <= width)) pieces;
  let raw = String.concat "" (List.map render pieces) in
  check (String.is_valid_utf_8 raw);
  check_eq ~pp:Fmt.Dump.string input (Console_text.strip raw);
  let cols = Span.width gradient + 2 in
  let expected = Console_vte.v ~rows:1 ~cols ()
  and actual = Console_vte.v ~rows:1 ~cols () in
  Console_vte.feed expected (render gradient ^ "!");
  Console_vte.feed actual (raw ^ "!");
  for i = 0 to cols - 1 do
    if
      not
        (Matrix_grid.cells_equal
           (Console_vte.grid expected)
           i (Console_vte.grid actual) i)
    then failf "wrapping changed the glyph or colour at column %d" i
  done

let suite =
  ( "span",
    [
      test_case "wrapping a gradient preserves glyphs and colours"
        [ bytes; uint8 ] test_gradient_wrap;
      test_case "linked Unicode cuts fit and keep complete escapes"
        [ bytes; uint8 ] test_linked_unicode_cuts;
      test_case "multiline styles reset before LF" [ bytes ]
        test_multiline_resets;
      test_case "every span prints" [ bytes ] test_sanitized;
      test_case "width is subadditive" [ bytes; bytes ] test_width_subadditive;
      test_case "styles add no cell" [ bytes ] test_width_styles;
      test_case "truncate fits, keeps a prefix, ends with an ellipsis"
        [ bytes; int ] test_truncate;
      test_case "truncate turns styles off" [ bytes; int ] test_truncate_closes;
      test_case "wrap fits and keeps the text" [ bytes; int ] test_wrap;
      test_case "wrap takes any bytes" [ bytes; int ] test_wrap_bytes;
      test_case "cuts fit with joiners and selectors" [ bytes; int ]
        test_joiners_fit;
      test_case "wrap refuses a non-positive width" [ int ] test_wrap_refuses;
      test_case "link" [ bytes; bytes ] test_link;
      test_case "concat" [ bytes; bytes; bytes ] test_concat;
      test_case "a gradient is its text, as wide" [ bytes; uint8 ] test_gradient;
      test_case "a role is its text, as wide" [ bytes; uint8 ] test_role;
    ] )
