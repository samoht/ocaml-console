(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains affix s = Astring.String.is_infix ~affix s

let fake_clock () =
  let time = ref 0. in
  ((fun () -> !time), fun dt -> time := !time +. dt)

let ctx ?(is_tty = false) ?(width = 80) ?(height = 24) ~ppf ~now () =
  Display.ctx ~ppf ~now ~dimensions:(fun () -> (width, height)) ~is_tty ()

let capture_on ~is_tty ~mode ?header ?bar ?theme f =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let value =
    Display.run ~ctx:(ctx ~is_tty ~ppf ~now ()) ~mode ?header ?bar ?theme
      (fun display -> f display advance)
  in
  Format.pp_print_flush ppf ();
  (value, Buffer.contents buffer)

let capture ?header = capture_on ~is_tty:false ~mode:`History ?header

(* The same session on a live display. [capture] pins [`History], which is the
   appending writer; a terminal gets [`Activity], which paints a region and
   writes what is permanent around it. A result that reaches one and not the
   other is a result a developer does not get. *)
let capture_live ?header = capture_on ~is_tty:true ~mode:`Activity ?header

let check_invalid message f =
  Alcotest.check_raises message (Invalid_argument message) f

let mib n = Int64.mul (Int64.of_int n) 1048576L
let gib n = Int64.mul (Int64.of_int n) 1073741824L

let terminal_run ?(height = 8) ?(width = 40) ?header f =
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = fake_clock () in
  let vte = Vte.create ~rows:height ~cols:width () in
  let feed () =
    Format.pp_print_flush ppf ();
    Vte.feed_string vte (Buffer.contents buffer);
    Buffer.clear buffer
  in
  Display.run ~ctx:(ctx ~is_tty:true ~width ~height ~ppf ~now ())
    ~mode:`Activity ~width ~height ?header (fun display ->
      (* What a reader sees: the driver's tick has painted the snapshot. *)
      let sync () =
        Display.tick display;
        feed ()
      in
      f display advance sync vte;
      sync ());
  feed ();
  vte

let terminal_rows vte =
  Vte.scrollback_lines vte @ String.split_on_char '\n' (Vte.to_string vte)

(* The trailing cells of a row as the terminal received them: its last
   [columns] columns, which are the count cell, two spaces and the elapsed
   cell. Bytes are columns here -- a bar glyph or a wide label sits further
   left, and a count pair and a span are ASCII. *)
let trailing columns row =
  let n = String.length row in
  if n < columns then row else String.sub row (n - columns) columns

(* The rows of a terminal as a reader sees them, blanks trimmed off the right. *)
let screen_rows vte =
  String.split_on_char '\n' (Vte.to_string vte)
  |> List.map (fun row ->
      let n = ref (String.length row) in
      while !n > 0 && row.[!n - 1] = ' ' do
        decr n
      done;
      String.sub row 0 !n)
