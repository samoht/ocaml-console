(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* matrix 0.1.0 departs from xterm in four ways a test can see: a backspace erases the cell it moves over; SGR
   39 and 49 set the colours to transparent black, not to the defaults; a
   character of no width is dropped, from a cluster or on its own; and
   after a character in the last column it puts the cursor one column
   beyond the screen, where its cursor controls start one column off. [feed]
   reads the bytes itself and hands matrix only what it reads as xterm
   does. *)

type state = Ground | Escape | Csi | String | String_escape

type t = {
  vte : Vte.t;
  raw : bool;
  mutable state : state;
  seq : Buffer.t;  (** The escape sequence being read. *)
  text : Buffer.t;  (** The text not drawn yet. *)
}

let v ?(raw = false) ~rows ~cols () =
  {
    vte = Vte.create ~rows ~cols ();
    raw;
    state = Ground;
    seq = Buffer.create 32;
    text = Buffer.create 256;
  }

let vte t = t.vte
let rows t = Vte.rows t.vte
let cols t = Vte.cols t.vte
let resize t ~rows ~cols = Vte.resize t.vte ~rows ~cols
let cursor_pos t = Vte.cursor_pos t.vte
let cursor_visible t = Vte.cursor_visible t.vte
let to_string t = Vte.to_string t.vte
let scrollback_lines t = Vte.scrollback_lines t.vte
let scrollback_size t = Vte.scrollback_size t.vte
let grid t = Vte.grid t.vte

(* {1 The last column} *)

(* xterm keeps the cursor in the last column with the next character
   pending its wrap, and a cursor control acts from that column. *)
let to_last_column t =
  let row, col = Vte.cursor_pos t.vte in
  let last = Vte.cols t.vte - 1 in
  if col > last then Vte.set_cursor_pos t.vte ~row ~col:last

let control t s =
  to_last_column t;
  Vte.feed_string t.vte s;
  to_last_column t

(* {1 Text} *)

let width t s =
  Glyph.String.measure
    ~width_method:(Grid.width_method (Vte.grid t.vte))
    ~tab_width:2 s

(* The cell that holds the grapheme drawn last before column [col]: the
   start of a wide one that ends there. *)
let rec start g ~row x =
  if x > 0 && Grid.is_continuation g (Grid.idx g ~x ~y:row) then
    start g ~row (x - 1)
  else x

(* [set t ~row ~x text] puts [text] in the cell at [x], at that cell's width
   and in its colours, as the grapheme it holds. *)
let set t ~row ~x text =
  let g = Vte.grid t.vte in
  let i = Grid.idx g ~x ~y:row in
  let width = max 1 (Grid.cell_width g i) in
  let glyph =
    Glyph.Pool.intern_sub (Grid.glyph_pool g)
      ~width_method:(Grid.width_method g) ~tab_width:2 text ~pos:0
      ~len:(String.length text) ~width
  in
  let style = Grid.get_style g i in
  let fg = Option.value style.fg ~default:Ansi.Color.white
  and bg = Option.value style.bg ~default:Ansi.Color.black in
  let attrs = style.attrs and link = style.link in
  Grid.set_cell g ~x ~y:row ~glyph ~fg ~bg ~attrs ?link ~blend:false ();
  for k = 1 to width - 1 do
    Grid.set_cell g ~x:(x + k) ~y:row
      ~glyph:
        (Glyph.make_continuation ~code:glyph ~left:k ~right:(width - 1 - k))
      ~fg ~bg ~attrs ?link ~blend:false ()
  done

(* A character of no width joins the cell before the cursor, as in xterm;
   in the first column there is none, and it is dropped. *)
let attach t s =
  let g = Vte.grid t.vte in
  let row, col = Vte.cursor_pos t.vte in
  if col > 0 && row < Grid.height g then
    let x = start g ~row (min col (Grid.width g) - 1) in
    let i = Grid.idx g ~x ~y:row in
    let text = if Grid.is_empty g i then " " else Grid.get_text g i in
    set t ~row ~x (text ^ s)

(* A cluster of more than one character is drawn whole, its zero-width
   characters with it. *)
let cluster t s =
  Vte.feed_string t.vte s;
  let g = Vte.grid t.vte in
  let row, col = Vte.cursor_pos t.vte in
  let x = start g ~row (min col (Grid.width g) - 1) in
  if x >= 0 && row < Grid.height g then
    let i = Grid.idx g ~x ~y:row in
    if Grid.get_text g i <> s then set t ~row ~x s

let is_single s =
  Uchar.utf_decode_length (String.get_utf_8_uchar s 0) = String.length s

(* The text read so far, but for the first bytes of a character the next
   write ends. *)
let complete s =
  let n = String.length s in
  let rec lead i =
    if i > 0 && Char.code s.[i] land 0xc0 = 0x80 then lead (i - 1) else i
  in
  if n = 0 then 0
  else
    let i = lead (n - 1) in
    let d = String.get_utf_8_uchar s i in
    if
      (not (Uchar.utf_decode_is_valid d))
      && i + 4 > n
      && Char.code s.[i] >= 0xc2
    then i
    else n

(* Runs of clusters of one character go to matrix in one piece. *)
let draw t =
  let s = Buffer.contents t.text in
  let n = complete s in
  Buffer.clear t.text;
  Buffer.add_substring t.text s n (String.length s - n);
  let run = Buffer.create n in
  let flush () =
    if Buffer.length run > 0 then begin
      Vte.feed_string t.vte (Buffer.contents run);
      Buffer.clear run
    end
  in
  Glyph.String.iter_graphemes
    (fun ~offset ~len ->
      let c = String.sub s offset len in
      if width t c = 0 then begin
        flush ();
        attach t c
      end
      else if is_single c then Buffer.add_string run c
      else begin
        flush ();
        cluster t c
      end)
    (String.sub s 0 n);
  flush ()

(* {1 Escape sequences} *)

let default_fg = Ansi.Color.to_rgb Ansi.Color.white
let default_bg = Ansi.Color.to_rgb Ansi.Color.black
let rgb (r, g, b) = Fmt.str "2;%d;%d;%d" r g b

(* SGR 39 and 49 as the explicit default colours; the parameters of an
   extended colour are values, kept as they are. *)
let sgr params =
  let rec go = function
    | [] -> []
    | (("38" | "48" | "58") as p) :: "5" :: n :: rest ->
        p :: "5" :: n :: go rest
    | (("38" | "48" | "58") as p) :: "2" :: r :: g :: b :: rest ->
        p :: "2" :: r :: g :: b :: go rest
    | "39" :: rest -> ("38;" ^ rgb default_fg) :: go rest
    | "49" :: rest -> ("48;" ^ rgb default_bg) :: go rest
    | p :: rest -> p :: go rest
  in
  "\027[" ^ String.concat ";" (go (String.split_on_char ';' params)) ^ "m"

(* The CSI sequences that move the cursor or edit from its column. *)
let moves = "ABCDEFGHJKLMPSTX@`adef"

let sequence t s =
  let n = String.length s in
  if n >= 3 && s.[1] = '[' then
    let final = s.[n - 1] in
    let params = String.sub s 2 (n - 3) in
    if final = 'm' && String.for_all (fun c -> c <> ':') params then
      Vte.feed_string t.vte (sgr params)
    else if String.contains moves final then control t s
    else Vte.feed_string t.vte s
  else
    match s with
    | "\027D" | "\027E" | "\027M" -> control t s
    | _ -> Vte.feed_string t.vte s

(* A line feed on a terminal moves down a row in the same column; a tty
   with output processing on also returns the cursor. *)
let line_feed t c =
  if t.raw then begin
    to_last_column t;
    let _, col = Vte.cursor_pos t.vte in
    Vte.feed_string t.vte (String.make 1 c);
    let row, _ = Vte.cursor_pos t.vte in
    Vte.set_cursor_pos t.vte ~row ~col
  end
  else Vte.feed_string t.vte (String.make 1 c)

let escape = function
  | '[' -> Csi
  | ']' | 'P' | 'X' | '^' | '_' -> String
  | ' ' .. '/' -> Escape
  | _ -> Ground

let ground t c =
  match c with
  | '\027' ->
      draw t;
      Buffer.add_char t.seq c;
      t.state <- Escape
  | '\b' ->
      draw t;
      control t "\027[D"
  | '\n' | '\011' | '\012' ->
      draw t;
      line_feed t c
  | '\t' ->
      draw t;
      control t "\t"
  | '\000' .. '\031' | '\127' ->
      draw t;
      Vte.feed_string t.vte (String.make 1 c)
  | c -> Buffer.add_char t.text c

let byte t c =
  match t.state with
  | Ground -> ground t c
  | state ->
      Buffer.add_char t.seq c;
      let next =
        match (state, c) with
        | Escape, c -> escape c
        | Csi, '@' .. '~' -> Ground
        | Csi, _ -> Csi
        | String, '\007' -> Ground
        | String, '\027' -> String_escape
        | String, _ -> String
        | String_escape, '\\' -> Ground
        | String_escape, c -> escape c
        | Ground, _ -> Ground
      in
      t.state <- next;
      if next = Ground then begin
        let s = Buffer.contents t.seq in
        Buffer.clear t.seq;
        sequence t s
      end

let feed t s =
  String.iter (byte t) s;
  draw t

(* {1 Reading the screen} *)

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
    decr n
  done;
  String.sub s 0 !n

let drop_trailing_blanks rows =
  let rec go = function "" :: tl -> go tl | rows -> rows in
  List.rev (go (List.rev rows))

let screen ?(scrollback = false) t =
  let visible = String.split_on_char '\n' (Vte.to_string t.vte) in
  let rows =
    if scrollback then Vte.scrollback_lines t.vte @ visible else visible
  in
  drop_trailing_blanks (List.map rstrip rows)

(* {1 Recordings} *)

(* One record's header is 24 bytes: the payload length, the wall clock the
   capture stamped it with, and which of the three kinds it is. *)
let header_size = 24
let u64 s off = Int64.to_int (String.get_int64_le s off)
let i32 s off = Int32.to_int (String.get_int32_le s off)

let records recording =
  let n = String.length recording in
  let rec loop off acc =
    (* A capture of a program that was still running ends mid-record, so a
       header that does not fit, or one promising more payload than is left,
       ends the stream rather than raising. *)
    if off + header_size > n then List.rev acc
    else
      let len = u64 recording off in
      if len < 0 || len > n - off - header_size then List.rev acc
      else
        let seconds =
          float_of_int (u64 recording (off + 8))
          +. (float_of_int (i32 recording (off + 16)) /. 1e6)
        in
        let kind = Char.chr (i32 recording (off + 20) land 0xff) in
        let next = off + header_size + len in
        if kind <> 'o' || len = 0 then loop next acc
        else
          loop next
            ((seconds, String.sub recording (off + header_size) len) :: acc)
  in
  loop 0 []

let frames ~rows ~cols recording =
  match records recording with
  | [] -> []
  | (start, _) :: _ as records ->
      let t = v ~rows ~cols () in
      List.map
        (fun (seconds, written) ->
          feed t written;
          (seconds -. start, screen t))
        records
