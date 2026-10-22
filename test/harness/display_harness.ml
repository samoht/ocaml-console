(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
module Driver = Display_driver

let contains affix s = Astring.String.is_infix ~affix s

let clock () =
  let time = ref 0. in
  ((fun () -> !time), fun dt -> time := !time +. dt)

let plain ?summary ?layout ?theme f =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Option.iter (Theme.set ppf) theme;
  let now, advance = clock () in
  let d = Display.v ?summary ?layout ~now ppf in
  let v = f d advance in
  Display.close d;
  Format.pp_print_flush ppf ();
  (v, Buffer.contents buffer)

type term = {
  vte : Console_vte.t;
  display : Display.t;
  advance : float -> unit;
  size : (int * int) ref;
  chunks : string list ref;
}

let terminal ?(width = 40) ?(height = 8) ?summary ?layout ?theme
    ?(styled = false) ?(raw = false) ?(before = "") () =
  let buffer = Buffer.create 512 in
  let vte = Console_vte.v ~raw ~rows:height ~cols:width () in
  let chunks = ref [] and size = ref (width, height) in
  let out_string s o l = Buffer.add_substring buffer s o l in
  let out_flush () =
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    if s <> "" then begin
      chunks := s :: !chunks;
      Console_vte.feed vte s
    end
  in
  Console_vte.feed vte before;
  let ppf = Format.make_formatter out_string out_flush in
  if styled then begin
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color
  end;
  Option.iter (Theme.set ppf) theme;
  let now, advance = clock () in
  let display = Display.v ?summary ?layout ~size:(fun () -> !size) ~now ppf in
  { vte; display; advance; size; chunks }

let written t = String.concat "" (List.rev !(t.chunks))

let resize t ~width ~height =
  t.size := (width, height);
  Console_vte.resize t.vte ~rows:height ~cols:width

let screen t = Console_vte.screen t.vte
let lines t = Console_vte.screen ~scrollback:true t.vte

let check_invalid message f =
  Alcotest.check_raises message (Invalid_argument message) f

let skip_on_matrix_0_1 fix _ _ =
  Fmt.pr "Skipped on matrix 0.1.0, which lacks %s.@." fix;
  Alcotest.skip ()
