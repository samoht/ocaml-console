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
  mutable last : int;  (** The last code point drawn, or [-1]. *)
}

let v ?(raw = false) ~rows ~cols () =
  {
    vte = Vte.create ~rows ~cols ();
    raw;
    state = Ground;
    seq = Buffer.create 32;
    text = Buffer.create 256;
    last = -1;
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

(* The characters VS16 can show as an emoji, the bases of
   emoji-variation-sequences.txt (Unicode 16.0), as ranges. *)
let vs16_bases =
  [|
    (0x23, 0x23); (0x2a, 0x2a); (0x30, 0x39); (0xa9, 0xa9); (0xae, 0xae);
    (0x203c, 0x203c); (0x2049, 0x2049); (0x2122, 0x2122); (0x2139, 0x2139);
    (0x2194, 0x2199); (0x21a9, 0x21aa); (0x231a, 0x231b); (0x2328, 0x2328);
    (0x23cf, 0x23cf); (0x23e9, 0x23f3); (0x23f8, 0x23fa); (0x24c2, 0x24c2);
    (0x25aa, 0x25ab); (0x25b6, 0x25b6); (0x25c0, 0x25c0); (0x25fb, 0x25fe);
    (0x2600, 0x2604); (0x260e, 0x260e); (0x2611, 0x2611); (0x2614, 0x2615);
    (0x2618, 0x2618); (0x261d, 0x261d); (0x2620, 0x2620); (0x2622, 0x2623);
    (0x2626, 0x2626); (0x262a, 0x262a); (0x262e, 0x262f); (0x2638, 0x263a);
    (0x2640, 0x2640); (0x2642, 0x2642); (0x2648, 0x2653); (0x265f, 0x2660);
    (0x2663, 0x2663); (0x2665, 0x2666); (0x2668, 0x2668); (0x267b, 0x267b);
    (0x267e, 0x267f); (0x2692, 0x2697); (0x2699, 0x2699); (0x269b, 0x269c);
    (0x26a0, 0x26a1); (0x26a7, 0x26a7); (0x26aa, 0x26ab); (0x26b0, 0x26b1);
    (0x26bd, 0x26be); (0x26c4, 0x26c5); (0x26c8, 0x26c8); (0x26ce, 0x26cf);
    (0x26d1, 0x26d1); (0x26d3, 0x26d4); (0x26e9, 0x26ea); (0x26f0, 0x26f5);
    (0x26f7, 0x26fa); (0x26fd, 0x26fd); (0x2702, 0x2702); (0x2705, 0x2705);
    (0x2708, 0x270d); (0x270f, 0x270f); (0x2712, 0x2712); (0x2714, 0x2714);
    (0x2716, 0x2716); (0x271d, 0x271d); (0x2721, 0x2721); (0x2728, 0x2728);
    (0x2733, 0x2734); (0x2744, 0x2744); (0x2747, 0x2747); (0x274c, 0x274c);
    (0x274e, 0x274e); (0x2753, 0x2755); (0x2757, 0x2757); (0x2763, 0x2764);
    (0x2795, 0x2797); (0x27a1, 0x27a1); (0x27b0, 0x27b0); (0x27bf, 0x27bf);
    (0x2934, 0x2935); (0x2b05, 0x2b07); (0x2b1b, 0x2b1c); (0x2b50, 0x2b50);
    (0x2b55, 0x2b55); (0x3030, 0x3030); (0x303d, 0x303d); (0x3297, 0x3297);
    (0x3299, 0x3299); (0x1f004, 0x1f004); (0x1f170, 0x1f171);
    (0x1f17e, 0x1f17f); (0x1f202, 0x1f202); (0x1f21a, 0x1f21a);
    (0x1f22f, 0x1f22f); (0x1f237, 0x1f237); (0x1f30d, 0x1f30f);
    (0x1f315, 0x1f315); (0x1f31c, 0x1f31c); (0x1f321, 0x1f321);
    (0x1f324, 0x1f32c); (0x1f336, 0x1f336); (0x1f378, 0x1f378);
    (0x1f37d, 0x1f37d); (0x1f393, 0x1f393); (0x1f396, 0x1f397);
    (0x1f399, 0x1f39b); (0x1f39e, 0x1f39f); (0x1f3a7, 0x1f3a7);
    (0x1f3ac, 0x1f3ae); (0x1f3c2, 0x1f3c2); (0x1f3c4, 0x1f3c4);
    (0x1f3c6, 0x1f3c6); (0x1f3ca, 0x1f3ce); (0x1f3d4, 0x1f3e0);
    (0x1f3ed, 0x1f3ed); (0x1f3f3, 0x1f3f3); (0x1f3f5, 0x1f3f5);
    (0x1f3f7, 0x1f3f7); (0x1f408, 0x1f408); (0x1f415, 0x1f415);
    (0x1f41f, 0x1f41f); (0x1f426, 0x1f426); (0x1f43f, 0x1f43f);
    (0x1f441, 0x1f442); (0x1f446, 0x1f449); (0x1f44d, 0x1f44e);
    (0x1f453, 0x1f453); (0x1f46a, 0x1f46a); (0x1f47d, 0x1f47d);
    (0x1f4a3, 0x1f4a3); (0x1f4b0, 0x1f4b0); (0x1f4b3, 0x1f4b3);
    (0x1f4bb, 0x1f4bb); (0x1f4bf, 0x1f4bf); (0x1f4cb, 0x1f4cb);
    (0x1f4da, 0x1f4da); (0x1f4df, 0x1f4df); (0x1f4e4, 0x1f4e6);
    (0x1f4ea, 0x1f4ed); (0x1f4f7, 0x1f4f7); (0x1f4f9, 0x1f4fb);
    (0x1f4fd, 0x1f4fd); (0x1f508, 0x1f508); (0x1f50d, 0x1f50d);
    (0x1f512, 0x1f513); (0x1f549, 0x1f54a); (0x1f550, 0x1f567);
    (0x1f56f, 0x1f570); (0x1f573, 0x1f579); (0x1f587, 0x1f587);
    (0x1f58a, 0x1f58d); (0x1f590, 0x1f590); (0x1f5a5, 0x1f5a5);
    (0x1f5a8, 0x1f5a8); (0x1f5b1, 0x1f5b2); (0x1f5bc, 0x1f5bc);
    (0x1f5c2, 0x1f5c4); (0x1f5d1, 0x1f5d3); (0x1f5dc, 0x1f5de);
    (0x1f5e1, 0x1f5e1); (0x1f5e3, 0x1f5e3); (0x1f5e8, 0x1f5e8);
    (0x1f5ef, 0x1f5ef); (0x1f5f3, 0x1f5f3); (0x1f5fa, 0x1f5fa);
    (0x1f610, 0x1f610); (0x1f687, 0x1f687); (0x1f68d, 0x1f68d);
    (0x1f691, 0x1f691); (0x1f694, 0x1f694); (0x1f698, 0x1f698);
    (0x1f6ad, 0x1f6ad); (0x1f6b2, 0x1f6b2); (0x1f6b9, 0x1f6ba);
    (0x1f6bc, 0x1f6bc); (0x1f6cb, 0x1f6cb); (0x1f6cd, 0x1f6cf);
    (0x1f6e0, 0x1f6e5); (0x1f6e9, 0x1f6e9); (0x1f6f0, 0x1f6f0);
    (0x1f6f3, 0x1f6f3);
  |]
[@@ocamlformat "disable"]

let is_vs16_base c =
  Array.exists (fun (first, last) -> c >= first && c <= last) vs16_bases

(* A VS16 after a character it cannot show as an emoji selects nothing:
   kitty and Ghostty draw the character as it is, and matrix 0.1.0 would
   make it two cells. *)
let without_stray_vs16 t s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i < n then begin
      let d = String.get_utf_8_uchar s i in
      let len = Uchar.utf_decode_length d in
      let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
      if not (c = 0xfe0f && not (is_vs16_base t.last)) then begin
        Buffer.add_string b (String.sub s i len);
        t.last <- c
      end;
      go (i + len)
    end
  in
  go 0;
  Buffer.contents b

(* Runs of clusters of one character go to matrix in one piece. *)
let draw t =
  let s = Buffer.contents t.text in
  let n = complete s in
  Buffer.clear t.text;
  Buffer.add_substring t.text s n (String.length s - n);
  let s = without_stray_vs16 t (String.sub s 0 n) in
  let n = String.length s in
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
    s;
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
