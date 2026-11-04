(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Csi_intermediate] follows an intermediate byte of a control sequence,
   after which only intermediate bytes and the final byte come. [Ss3]
   follows ESC O, which a terminal in application cursor mode sends before
   the letter of an arrow. *)
type esc = Normal | Esc | Csi | Csi_intermediate | Ss3

(* The line is a zipper of grapheme clusters split at the cursor: [left]
   holds the clusters before the cursor nearest-first (reversed), [right]
   those at and after it in order. Editing a whole cluster at a time keeps a
   character and its marks together, on the screen as in the line. *)
type t = {
  mutable left : string list;
  mutable right : string list;
  mutable len : int; (* the length in bytes of [left] and [right] *)
  max_line : int; (* the most bytes a line holds *)
  history_size : int; (* the most lines [hist] holds *)
  prompt : string;
  complete : string -> string list;
  mask : string option; (* echoed in place of each secret cluster *)
  mutable hist : string list; (* submitted lines, newest first, seed included *)
  mutable pos : int option; (* history index being viewed, newest is 0 *)
  mutable draft : string; (* line in progress, saved while navigating *)
  mutable esc : esc;
  mutable after_cr : bool; (* last byte was '\r', so swallow a following '\n' *)
  mutable partial : string; (* the start of a character, to be completed *)
  widths : (string, int) Hashtbl.t; (* the cells of recent clusters *)
}

(* What the editor echoes is text, never a control the terminal acts on. *)
let clean = Console_text.sanitize ~keep_newlines:false

(* The canonical-mode line of the Linux tty line discipline: its 4096-byte
   buffer (N_TTY_BUF_SIZE) less the line terminator. *)
let default_max_line = 4095

(* The lines bash keeps by default (HISTSIZE). *)
let default_history_size = 500
let rec take n = function x :: l when n > 0 -> x :: take (n - 1) l | _ -> []

let v ?(prompt = "") ?(history = []) ?(complete = fun _ -> []) ?mask
    ?(max_line = default_max_line) ?(history_size = default_history_size) () =
  {
    left = [];
    right = [];
    len = 0;
    max_line;
    history_size;
    prompt = clean prompt;
    complete;
    mask = Option.map clean mask;
    hist =
      (if Option.is_some mask then []
       else take history_size (List.rev_map clean history));
    pos = None;
    draft = "";
    esc = Normal;
    after_cr = false;
    partial = "";
    widths = Hashtbl.create 64;
  }

let crlf s = String.concat "\r\n" (String.split_on_char '\n' s)
let line t = String.concat "" (List.rev_append t.left t.right)
let pending t = line t
let history t = List.rev t.hist
let add = Buffer.add_string
let clusters = Console_width.clusters
let display_char t c = match t.mask with Some mask -> mask | None -> c
let display t s = String.concat "" (List.map (display_char t) (clusters s))

(* A masked editor measures the replacement glyphs, never the secret
   characters they conceal. *)
let width t s = Console_width.string_width (display t s)
let display_chars t cs = String.concat "" (List.map (display_char t) cs)
let cols t cs = Console_width.string_width (display_chars t cs)

(* Move the cursor back over [t.right] after rewriting it, one backspace per
   cell. *)
let reposition t echo =
  let tail = cols t t.right in
  if tail > 0 then add echo (String.make tail '\b')

(* The cells of the cluster [c], remembered: an edit measures the clusters
   around the cursor several times. *)
let cluster_cells t c =
  match Hashtbl.find_opt t.widths c with
  | Some n -> n
  | None ->
      let n = Console_width.string_width c in
      if Hashtbl.length t.widths >= 1024 then Hashtbl.reset t.widths;
      Hashtbl.add t.widths c n;
      n

(* [cells t cs] is [cols t cs]. Unmasked, it sums the cached cells of each
   cluster instead of measuring their concatenation. *)
let cells t cs =
  match t.mask with
  | None -> List.fold_left (fun n c -> n + cluster_cells t c) 0 cs
  | Some _ -> cols t cs

let rec drop n = function _ :: l when n > 0 -> drop (n - 1) l | l -> l

(* The number of leading clusters of [a] and [b], at most [max], that
   display the same. *)
let rec same t ~max n a b =
  match (a, b) with
  | x :: a, y :: b
    when n < max && String.equal (display_char t x) (display_char t y) ->
      same t ~max (n + 1) a b
  | _ -> n

(* [split cs ~at] splits the clusters [cs] at byte [at] of their text, or
   after the cluster that byte falls inside: the clusters before, nearest
   first, and those after. *)
let split cs ~at =
  let rec go acc off = function
    | c :: rest when off < at -> go (c :: acc) (off + String.length c) rest
    | rest -> (acc, rest)
  in
  go [] 0 cs

(* [rejoin text [] right] is [(cs, old, rest)]. [old] are the first
   clusters of [right], up to the first one that is still a cluster of its
   own after [text]; [cs] are the clusters of [text] followed by [old]; and
   [rest] is the remainder of [right], which the edit leaves as it is. Past
   that cluster the segmenter is in the state it was before the edit, since
   no mark carries state across the start of a cluster. *)
let rec rejoin text taken right =
  let cs = clusters (text ^ String.concat "" (List.rev taken)) in
  match (taken, right) with
  | r :: _, _ when String.equal (List.nth cs (List.length cs - 1)) r ->
      (cs, List.rev taken, right)
  | _, [] -> (cs, List.rev taken, [])
  | _, r :: right -> rejoin text (r :: taken) right

(* [edit t echo ~erased s] replaces [erased], the cluster before the cursor
   or [""], with [s]. The line is segmented again from the cluster before
   them, since [s] can join the clusters on either side of the cursor and
   an erased cluster can let its neighbours join. The cursor goes after
   [s], or after the cluster [s] ends inside, so that it never stands
   inside a cluster.

   The echo draws the line again from its first changed cluster, since a
   terminal does not join a mark written after the text that follows it,
   blanks the cells a narrower line leaves, and steps the cursor back.
   Unless the editor is masked, a cluster of no width with nothing before
   it to join is dropped, as a terminal drops it in the first column. An
   insert that would take the line past [max_line] bytes rings the bell
   instead. *)
let edit t echo ~erased s =
  t.pos <- None;
  let left = if erased = "" then t.left else List.tl t.left in
  let s =
    if left <> [] || t.mask <> None then s
    else
      match clusters s with
      | c :: cs when Console_width.string_width c = 0 -> String.concat "" cs
      | _ -> s
  in
  if s <> "" && t.len - String.length erased + String.length s > t.max_line then
    add echo "\007"
  else if s <> "" || erased <> "" then begin
    t.len <- t.len - String.length erased + String.length s;
    let prev, before =
      match left with c :: l -> ([ c ], l) | [] -> ([], [])
    in
    let prev_text = String.concat "" prev in
    let changed, old_right, rest = rejoin (prev_text ^ s) [] t.right in
    let now_left, now_right =
      split changed ~at:(String.length prev_text + String.length s)
    in
    t.left <- now_left @ before;
    t.right <- now_right @ rest;
    let before_cursor = prev @ if erased = "" then [] else [ erased ] in
    let was = before_cursor @ old_right in
    let n = List.length before_cursor in
    let k = same t ~max:n 0 was changed in
    let redrawn = drop k changed in
    let cursor = cells t before_cursor in
    let kept = if k = n then cursor else cells t (take k was) in
    let blank = max 0 (cursor + cells t old_right - (kept + cells t redrawn)) in
    add echo (String.make (cursor - kept) '\b');
    add echo (display_chars t redrawn);
    add echo (display_chars t rest);
    add echo (String.make blank ' ');
    add echo (String.make (cells t now_right + cells t rest + blank) '\b')
  end

let insert t echo s = edit t echo ~erased:"" s

(* Erase the cluster before the cursor. At the end of the line this is the
   familiar "\b \b". *)
let backspace t echo =
  match t.left with [] -> () | c :: _ -> edit t echo ~erased:c ""

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
  t.len <- 0;
  add echo "\r\n";
  (if String.length line > 0 && t.mask = None then
     match t.hist with
     | last :: _ when String.equal last line -> ()
     | hist -> t.hist <- take t.history_size (line :: hist));
  t.pos <- None;
  t.draft <- "";
  line

(* Replace the whole line with [s] (history recall): move to the end, erase
   every cell, then write [s] with the cursor left at its end. *)
let recall t echo s =
  add echo (display_chars t t.right);
  for _ = 1 to width t (line t) do
    add echo "\b \b"
  done;
  t.left <- List.rev (clusters s);
  t.right <- [];
  t.len <- String.length s;
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

(* Insert [bytes], a character or, if not [valid], a maximal part of one
   that [String.get_utf_8_uchar] reads as no character. Unmasked, such a
   part becomes one replacement character and the result is {!clean}; a
   masked editor inserts the bytes as typed. *)
let character t echo ~valid bytes =
  let c =
    if t.mask <> None then bytes
    else clean (if valid then bytes else "\xef\xbf\xbd")
  in
  if c <> "" then insert t echo c

(* Whether more bytes can complete [s], the start of a character: some
   continuation byte makes the decoder read past [s]. *)
let continues s =
  List.exists
    (fun next ->
      Uchar.utf_decode_length (String.get_utf_8_uchar (s ^ next) 0)
      > String.length s)
    [ "\x80"; "\x90"; "\xa0" ]

let rec normal t echo lines ~after_cr c =
  let code = Char.code c in
  if t.partial <> "" || code >= 0x80 then begin
    let s = t.partial ^ String.make 1 c in
    let d = String.get_utf_8_uchar s 0 in
    let n = Uchar.utf_decode_length d in
    let valid = Uchar.utf_decode_is_valid d in
    if (not valid) && n = String.length s && continues s then t.partial <- s
    else begin
      t.partial <- "";
      character t echo ~valid (String.sub s 0 n);
      (* The byte that ends no character is read again. *)
      if n < String.length s then normal t echo lines ~after_cr c
    end
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
    | c when code >= 0x20 -> insert t echo (String.make 1 c)
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
