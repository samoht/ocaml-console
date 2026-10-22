(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Csi_intermediate] is after an intermediate byte of a control sequence,
   which only intermediates and the final byte follow. [Ss3] is after ESC O,
   which a terminal in application cursor mode sends before an arrow's
   letter. *)
type esc = Normal | Esc | Csi | Csi_intermediate | Ss3

(* The line is a zipper of grapheme clusters split at the cursor: [left]
   holds the clusters before the cursor nearest-first (reversed), [right]
   those at and after it in order. Editing a whole cluster at a time keeps a
   character and its marks together, on the screen as in the line. *)
type t = {
  mutable left : string list;
  mutable right : string list;
  prompt : string;
  complete : string -> string list;
  mask : string option; (* replacement echoed for each secret character *)
  mutable hist : string list; (* submitted lines, newest first, seed included *)
  mutable pos : int option; (* history index being viewed, newest is 0 *)
  mutable draft : string; (* line in progress, saved while navigating *)
  mutable esc : esc; (* escape-sequence parser state *)
  mutable after_cr : bool; (* last byte was '\r', so swallow a following '\n' *)
  mutable partial : string; (* bytes of an incomplete multi-byte character *)
  mutable need : int; (* continuation bytes still expected for [partial] *)
}

(* What the editor echoes is text, never a control the terminal acts on. *)
let clean = Console_text.sanitize ~keep_newlines:false

let v ?(prompt = "") ?(history = []) ?(complete = fun _ -> []) ?mask () =
  {
    left = [];
    right = [];
    prompt = clean prompt;
    complete;
    mask = Option.map clean mask;
    hist = (if Option.is_some mask then [] else List.rev_map clean history);
    pos = None;
    draft = "";
    esc = Normal;
    after_cr = false;
    partial = "";
    need = 0;
  }

let line t = String.concat "" (List.rev_append t.left t.right)
let pending t = line t
let history t = List.rev t.hist
let add = Buffer.add_string

let utf8_len c =
  let b = Char.code c in
  if b < 0xc0 then 1 else if b < 0xe0 then 2 else if b < 0xf0 then 3 else 4

let clusters = Width.clusters
let display_char t c = match t.mask with Some mask -> mask | None -> c
let display t s = String.concat "" (List.map (display_char t) (clusters s))

(* A masked editor measures the replacement glyphs, never the secret
   characters they conceal. *)
let width t s = Width.string_width (display t s)
let display_chars t cs = String.concat "" (List.map (display_char t) cs)
let cols t cs = Width.string_width (display_chars t cs)

(* Move the cursor back over [t.right] after rewriting it, one backspace per
   cell. *)
let reposition t echo =
  let tail = cols t t.right in
  if tail > 0 then add echo (String.make tail '\b')

(* Insert [s] (one or more whole characters) at the cursor, its first joining
   the cluster before the cursor if it extends it. A cluster [s] extends is
   drawn again from its first cell, as a terminal does not join a mark
   written after the text that follows it; then the tail [s] pushed right is
   redrawn, the cells a narrower cluster leaves are blanked, and the cursor
   steps back to just after [s]. A cluster of no width with nothing before
   it to join is dropped, as a terminal drops it in the first column,
   except from a secret. *)
let insert t echo s =
  t.pos <- None;
  let prev, rest = match t.left with c :: l -> (c, l) | [] -> ("", []) in
  let kept =
    match clusters (prev ^ s) with
    | c :: cs when prev = "" && t.mask = None && Width.string_width c = 0 -> cs
    | cs -> cs
  in
  t.left <- List.rev_append kept rest;
  let redrawn, back =
    match kept with
    | c :: cs when prev <> "" && display_char t c = display_char t prev ->
        (cs, 0)
    | _ when prev = "" -> (kept, 0)
    | _ -> (kept, width t prev)
  in
  let blank = max 0 (back - cols t redrawn) in
  add echo (String.make back '\b');
  add echo (display_chars t redrawn);
  add echo (display_chars t t.right);
  add echo (String.make blank ' ');
  add echo (String.make (cols t t.right + blank) '\b')

(* Erase the character before the cursor: step left, redraw the tail with a
   trailing space to clear the vacated cell, then step back. At end of line this
   is the familiar "\b \b". *)
let backspace t echo =
  t.pos <- None;
  match t.left with
  | [] -> ()
  | c :: tl ->
      t.left <- tl;
      let w = width t c in
      add echo (String.make w '\b');
      add echo (display_chars t t.right);
      add echo (String.make w ' ');
      add echo (String.make (cols t t.right + w) '\b')

let cursor_left t echo =
  match t.left with
  | [] -> ()
  | c :: tl ->
      t.left <- tl;
      t.right <- c :: t.right;
      add echo (String.make (width t c) '\b')

let cursor_right t echo =
  match t.right with
  | [] -> ()
  | c :: tl ->
      t.right <- tl;
      t.left <- c :: t.left;
      add echo (display_char t c)

let submit t echo =
  let line = line t in
  t.left <- [];
  t.right <- [];
  add echo "\r\n";
  (if String.length line > 0 && t.mask = None then
     match t.hist with
     | last :: _ when String.equal last line -> ()
     | _ -> t.hist <- line :: t.hist);
  t.pos <- None;
  t.draft <- "";
  line

(* Replace the whole line with [s] (history recall): move to the end, erase
   every character, then write [s] with the cursor left at its end. *)
let recall t echo s =
  add echo (display_chars t t.right);
  for _ = 1 to width t (line t) do
    add echo "\b \b"
  done;
  t.left <- List.rev (clusters s);
  t.right <- [];
  add echo (display t s)

let hist_prev t echo =
  match (t.mask, t.hist) with
  | Some _, _ -> ()
  | None, [] -> ()
  | None, _ ->
      let last = List.length t.hist - 1 in
      let i = match t.pos with None -> 0 | Some i -> min (i + 1) last in
      if t.pos = None then t.draft <- line t;
      t.pos <- Some i;
      recall t echo (List.nth t.hist i)

let hist_next t echo =
  match (t.mask, t.pos) with
  | Some _, _ -> ()
  | None, None -> ()
  | None, Some 0 ->
      t.pos <- None;
      recall t echo t.draft
  | None, Some i ->
      t.pos <- Some (i - 1);
      recall t echo (List.nth t.hist (i - 1))

(* The longest run of whole clusters every candidate starts with. *)
let common_prefix = function
  | [] -> ""
  | first :: rest ->
      let rec common acc a b =
        match (a, b) with
        | x :: a, y :: b when String.equal x y -> common (x :: acc) a b
        | _ -> List.rev acc
      in
      String.concat ""
        (List.fold_left
           (fun p s -> common [] p (clusters s))
           (clusters first) rest)

(* The word completion acts on is the run after the last space before the
   cursor. *)
let last_word s =
  match String.rindex_opt s ' ' with
  | None -> s
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)

let complete t echo =
  if t.mask <> None then ()
  else
    let before = String.concat "" (List.rev t.left) in
    let word = last_word before in
    let candidates = List.map clean (t.complete word) in
    match List.filter (String.starts_with ~prefix:word) candidates with
    | [] -> ()
    | candidates ->
        let lcp = common_prefix candidates in
        if String.length lcp > String.length word then
          insert t echo
            (String.sub lcp (String.length word)
               (String.length lcp - String.length word));
        if List.length candidates > 1 then begin
          add echo "\r\n";
          add echo (String.concat "  " candidates);
          add echo "\r\n";
          add echo t.prompt;
          add echo (display t (line t));
          reposition t echo
        end

(* The character [t.partial] holds, once its last byte is in. A surrogate,
   an overlong form or a code point past U+10FFFF is no character: it is
   one replacement character, and a secret keeps its bytes. *)
let end_char t echo =
  let d = String.get_utf_8_uchar t.partial 0 in
  let valid =
    Uchar.utf_decode_is_valid d
    && Uchar.utf_decode_length d = String.length t.partial
  in
  let c =
    if t.mask <> None then t.partial
    else clean (if valid then t.partial else "\xef\xbf\xbd")
  in
  if c <> "" then insert t echo c;
  t.partial <- ""

let rec normal t echo lines ~after_cr c =
  let code = Char.code c in
  if t.need > 0 then
    if code >= 0x80 && code <= 0xbf then begin
      t.partial <- t.partial ^ String.make 1 c;
      t.need <- t.need - 1;
      if t.need = 0 then end_char t echo
    end
    else begin
      (* Not a continuation byte: the truncated character is dropped, or
         kept as typed in a secret, and [c] is read again. *)
      if t.mask <> None then insert t echo t.partial;
      t.partial <- "";
      t.need <- 0;
      normal t echo lines ~after_cr c
    end
  else
    match c with
    | '\027' -> t.esc <- Esc
    | '\r' ->
        t.after_cr <- true;
        lines := submit t echo :: !lines
    | '\n' -> if not after_cr then lines := submit t echo :: !lines
    | '\t' -> complete t echo
    | '\127' | '\008' -> backspace t echo
    | c when code >= 0xc0 && code <= 0xf7 ->
        t.partial <- String.make 1 c;
        t.need <- utf8_len c - 1
    | c when code >= 0x20 && code < 0x7f -> insert t echo (String.make 1 c)
    | c when code >= 0x80 && t.mask <> None -> insert t echo (String.make 1 c)
    | _ -> ()

let arrow t echo = function
  | 'A' -> hist_prev t echo
  | 'B' -> hist_next t echo
  | 'C' -> cursor_right t echo
  | 'D' -> cursor_left t echo
  | _ -> ()

(* ECMA-48 5.4: parameter bytes 03/00-03/15, intermediate bytes
   02/00-02/15, then the final byte, 04/00-07/14. With intermediates, the
   final byte names another function than an arrow. *)
let csi t echo ~intermediate c =
  let code = Char.code c in
  if code >= 0x40 && code <= 0x7e then begin
    if not intermediate then arrow t echo c;
    t.esc <- Normal
  end
  else if code >= 0x20 && code <= 0x2f then t.esc <- Csi_intermediate
  else if intermediate || not (code >= 0x30 && code <= 0x3f) then
    t.esc <- Normal

let feed t input =
  let echo = Buffer.create (String.length input) in
  let lines = ref [] in
  String.iter
    (fun c ->
      let after_cr = t.after_cr in
      t.after_cr <- false;
      match t.esc with
      | Esc -> t.esc <- (match c with '[' -> Csi | 'O' -> Ss3 | _ -> Normal)
      | Csi -> csi t echo ~intermediate:false c
      | Csi_intermediate -> csi t echo ~intermediate:true c
      | Ss3 ->
          arrow t echo c;
          t.esc <- Normal
      | Normal -> normal t echo lines ~after_cr c)
    input;
  (Buffer.contents echo, List.rev !lines)
