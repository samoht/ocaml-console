(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains affix s = Astring.String.is_infix ~affix s

let clock () =
  let time = ref 0. in
  ((fun () -> !time), fun dt -> time := !time +. dt)

let plain ?header ?layout ?theme f =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now, advance = clock () in
  let d = Display.v ?header ?layout ?theme ~now ppf in
  let v = f d advance in
  Display.close d;
  Format.pp_print_flush ppf ();
  (v, Buffer.contents buffer)

type term = {
  vte : Vte.t;
  display : Display.t;
  advance : float -> unit;
  size : (int * int) ref;
  chunks : string list ref;
}

(* A tty with output processing off passes a line feed through as is: down
   one row, same column. The emulator returns to the first column on a line
   feed, so the column is put back after it. *)
let feed_raw vte s =
  List.iteri
    (fun i part ->
      if i > 0 then begin
        let col = min (snd (Vte.cursor_pos vte)) (Vte.cols vte - 1) in
        Vte.feed_string vte "\n";
        if col > 0 then Vte.feed_string vte (Fmt.str "\027[%dG" (col + 1))
      end;
      Vte.feed_string vte part)
    (String.split_on_char '\n' s)

let terminal ?(width = 40) ?(height = 8) ?header ?layout ?theme
    ?(styled = false) ?(raw = false) () =
  let buffer = Buffer.create 512 in
  let vte = Vte.create ~rows:height ~cols:width () in
  let chunks = ref [] and size = ref (width, height) in
  let out_string s o l = Buffer.add_substring buffer s o l in
  let out_flush () =
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    if s <> "" then begin
      chunks := s :: !chunks;
      if raw then feed_raw vte s else Vte.feed_string vte s
    end
  in
  let ppf = Format.make_formatter out_string out_flush in
  if styled then Fmt.set_style_renderer ppf `Ansi_tty;
  let now, advance = clock () in
  let display =
    Display.v ?header ?layout ?theme ~size:(fun () -> !size) ~now ppf
  in
  { vte; display; advance; size; chunks }

let written t = String.concat "" (List.rev !(t.chunks))

let resize t ~width ~height =
  t.size := (width, height);
  Vte.resize t.vte ~rows:height ~cols:width

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let screen t = List.map rstrip (String.split_on_char '\n' (Vte.to_string t.vte))

let lines t =
  let rows = Vte.scrollback_lines t.vte @ screen t in
  let rec drop_blank = function "" :: l -> drop_blank l | l -> l in
  List.rev (drop_blank (List.rev (List.map rstrip rows)))

let check_invalid message f =
  Alcotest.check_raises message (Invalid_argument message) f
