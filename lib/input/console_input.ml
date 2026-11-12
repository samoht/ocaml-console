(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Csi_intermediate] is the state after an intermediate byte of a control
   sequence. Only more intermediate bytes and the final byte can follow. [Ss3]
   is the state after ESC O, which a terminal in application cursor mode sends
   before the letter of an arrow. *)
type esc = Normal | Esc | Csi | Csi_intermediate | Ss3

(* The line is a zipper of grapheme clusters, split at the cursor. [left]
   holds the clusters before the cursor, nearest first (reversed). [right]
   holds the clusters after the cursor, in order. Editing a whole cluster at a
   time keeps a character and its marks together, both on the screen and in
   the line. Only [Line] changes the clusters, so [length] always matches
   them. *)
module Line : sig
  type t = private { left : string list; right : string list; length : int }

  val empty : t

  val at_end : string list -> t
  (** [at_end cs] is the clusters [cs], with the cursor after them. *)

  val move_left : t -> (string * t) option
  (** [move_left t] is the cluster before the cursor and [t] with the cursor
      before it, if any. *)

  val move_right : t -> (string * t) option
  (** [move_right t] is the cluster after the cursor and [t] with the cursor
      after it, if any. *)

  val replace :
    t -> before:int -> after:int -> left:string list -> right:string list -> t
  (** [replace t ~before ~after ~left ~right] removes the [before] clusters just
      before the cursor and the [after] clusters just after it. It puts [left]
      (nearest first) and [right] in their place. *)
end = struct
  type t = { left : string list; right : string list; length : int }

  let bytes = List.fold_left (fun n c -> n + String.length c) 0
  let empty = { left = []; right = []; length = 0 }
  let at_end cs = { left = List.rev cs; right = []; length = bytes cs }

  let move_left t =
    match t.left with
    | [] -> None
    | c :: left -> Some (c, { t with left; right = c :: t.right })

  let move_right t =
    match t.right with
    | [] -> None
    | c :: right -> Some (c, { t with right; left = c :: t.left })

  let replace t ~before ~after ~left ~right =
    let gone_left = List.take before t.left in
    let gone_right = List.take after t.right in
    {
      left = left @ List.drop before t.left;
      right = right @ List.drop after t.right;
      length =
        t.length - bytes gone_left - bytes gone_right + bytes left + bytes right;
    }
end

type t = {
  mutable line : Line.t;
  max_line : int; (* the maximum length of a line, in bytes *)
  history_size : int; (* the maximum number of lines in [hist] *)
  prompt : string;
  complete : string -> string list;
  mask : string option; (* echoed instead of each secret cluster *)
  mutable hist : string list; (* submitted lines, newest first, seed included *)
  mutable pos : int option; (* history index being viewed, newest is 0 *)
  mutable draft : string; (* line in progress, saved while navigating *)
  mutable esc : esc;
  mutable after_cr : bool; (* last byte was '\r', so swallow a following '\n' *)
  mutable partial : string; (* the first bytes of an incomplete character *)
  widths : (string, int) Hashtbl.t; (* the cells of recent clusters *)
}

(* What the editor echoes is text, never a control the terminal acts on. *)
let clean = Console_text.sanitize ~keep_newlines:false

(* The canonical-mode line length of the Linux tty line discipline: its
   4096-byte buffer (N_TTY_BUF_SIZE) minus the line terminator. *)
let default_max_line = 4095

(* The number of lines that bash keeps by default (HISTSIZE). *)
let default_history_size = 500
let rec take n = function x :: l when n > 0 -> x :: take (n - 1) l | _ -> []

let v ?(prompt = "") ?(history = []) ?(complete = fun _ -> []) ?mask
    ?(max_line = default_max_line) ?(history_size = default_history_size) () =
  {
    line = Line.empty;
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
let line t = String.concat "" (List.rev_append t.line.left t.line.right)
let pending t = line t
let history t = List.rev t.hist
let add = Buffer.add_string
let clusters = Console_width.clusters
let display_char t c = match t.mask with Some mask -> mask | None -> c
let display t s = String.concat "" (List.map (display_char t) (clusters s))

(* A masked editor measures the mask glyphs, never the secret characters that
   they hide. *)
let width t s = Console_width.string_width (display t s)
let display_chars t cs = String.concat "" (List.map (display_char t) cs)
let cols t cs = Console_width.string_width (display_chars t cs)

(* Move the cursor back over [t.line.right] after rewriting it, one backspace per
   cell. *)
let reposition t echo =
  let tail = cols t t.line.right in
  if tail > 0 then add echo (String.make tail '\b')

(* [cluster_cells t c] is the number of cells of the cluster [c]. It is
   cached, because an edit measures the clusters around the cursor several
   times. *)
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

(* The number of leading clusters that display the same in [a] and [b], at
   most [max]. *)
let rec same t ~max n a b =
  match (a, b) with
  | x :: a, y :: b
    when n < max && String.equal (display_char t x) (display_char t y) ->
      same t ~max (n + 1) a b
  | _ -> n

(* [split cs ~at] splits the clusters [cs] at byte [at] of their text. If that
   byte falls inside a cluster, it splits after that cluster. The result is
   the clusters before, nearest first, and the clusters after. *)
let split cs ~at =
  let rec go acc off = function
    | c :: rest when off < at -> go (c :: acc) (off + String.length c) rest
    | rest -> (acc, rest)
  in
  go [] 0 cs

(* [rejoin text [] right] is [(cs, old, rest)], where:
   - [old] is the first clusters of [right], up to and including the first
     one that stays a cluster of its own after [text];
   - [cs] is the clusters of [text] followed by [old];
   - [rest] is the rest of [right], which the edit does not change.
   After that cluster, the segmenter is in the same state as before the edit,
   because no mark carries state across the start of a cluster. *)
let rec rejoin text taken right =
  let cs = clusters (text ^ String.concat "" (List.rev taken)) in
  match (taken, right) with
  | r :: _, _ when String.equal (List.nth cs (List.length cs - 1)) r ->
      (cs, List.rev taken, right)
  | _, [] -> (cs, List.rev taken, [])
  | _, r :: right -> rejoin text (r :: taken) right

(* [edit t echo ~erased s] replaces [erased] with [s]. [erased] is the cluster
   before the cursor, or [""]. The line is segmented again from the cluster
   before them, because [s] can join the clusters on either side of the
   cursor, and an erased cluster can let its neighbours join. The cursor goes
   after [s], or after the cluster in which [s] ends, so it never stands
   inside a cluster.

   The echo redraws the line from its first changed cluster, because a
   terminal does not join a mark that is written after the text that follows
   it. The echo then blanks the cells that a narrower line leaves, and moves
   the cursor back. Unless the editor is masked, a zero-width cluster with
   nothing before it to join is dropped, as a terminal drops it in the first
   column. If an insert would make the line longer than [max_line] bytes, the
   bell rings instead. *)
let edit t echo ~erased s =
  t.pos <- None;
  let left = if erased = "" then t.line.left else List.tl t.line.left in
  let s =
    if left <> [] || t.mask <> None then s
    else
      match clusters s with
      | c :: cs when Console_width.string_width c = 0 -> String.concat "" cs
      | _ -> s
  in
  if
    s <> ""
    && t.line.length - String.length erased + String.length s > t.max_line
  then add echo "\007"
  else if s <> "" || erased <> "" then begin
    let prev = match left with c :: _ -> [ c ] | [] -> [] in
    let prev_text = String.concat "" prev in
    let changed, old_right, rest = rejoin (prev_text ^ s) [] t.line.right in
    let now_left, now_right =
      split changed ~at:(String.length prev_text + String.length s)
    in
    t.line <-
      Line.replace t.line
        ~before:(List.length prev + if erased = "" then 0 else 1)
        ~after:(List.length old_right) ~left:now_left ~right:now_right;
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
  match t.line.left with [] -> () | c :: _ -> edit t echo ~erased:c ""

let cursor_left t echo =
  match Line.move_left t.line with
  | None -> ()
  | Some (c, line) ->
      t.line <- line;
      add echo (String.make (width t c) '\b')

let cursor_right t echo =
  match Line.move_right t.line with
  | None -> ()
  | Some (c, line) ->
      t.line <- line;
      add echo (display_char t c)

let submit t echo =
  let line = line t in
  t.line <- Line.empty;
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
  add echo (display_chars t t.line.right);
  for _ = 1 to width t (line t) do
    add echo "\b \b"
  done;
  t.line <- Line.at_end (clusters s);
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

(* The longest run of whole clusters that every candidate starts with. *)
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

(* Completion acts on the last word before the cursor: the text after the
   last space. *)
let last_word s =
  match String.rindex_opt s ' ' with
  | None -> s
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)

let complete t echo =
  if t.mask <> None then ()
  else
    let before = String.concat "" (List.rev t.line.left) in
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

(* [character t echo ~valid bytes] inserts [bytes]. [bytes] is a character
   or, if not [valid], a maximal part of a character that
   [String.get_utf_8_uchar] cannot decode. Unmasked, such a part becomes one
   replacement character, and the result goes through {!clean}. A masked
   editor inserts the bytes as typed. *)
let character t echo ~valid bytes =
  let c =
    if t.mask <> None then bytes
    else clean (if valid then bytes else "\xef\xbf\xbd")
  in
  if c <> "" then insert t echo c

(* [continues s] is [true] iff more bytes can complete [s], the start of a
   character: some continuation byte makes the decoder read past [s]. *)
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
      (* The last byte is not part of the character, so read it again. *)
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
   02/00-02/15, then the final byte, 04/00-07/14. With intermediate bytes, the
   final byte names a function other than an arrow. *)

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
