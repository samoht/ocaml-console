(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let outputs = [ (Fmt.stdout, Unix.stdout); (Fmt.stderr, Unix.stderr) ]

external enable_vt : Unix.file_descr -> bool = "console_tty_enable_vt"
[@@noalloc]

external output_code_page : unit -> int = "console_tty_output_code_page"
[@@noalloc]

external set_output_code_page : int -> bool = "console_tty_set_output_code_page"
[@@noalloc]

let cp_utf_8 = 65001

(* A Windows console shows UTF-8 only in its UTF-8 code page. The console
   keeps its code page after the program, so the program gives the old one
   back at exit. *)
let utf_8_code_page =
  lazy
    (match output_code_page () with
    | -1 -> false
    | cp when cp = cp_utf_8 -> true
    | cp ->
        set_output_code_page cp_utf_8
        && begin
          at_exit (fun () -> ignore (set_output_code_page cp : bool));
          true
        end)

(* On a Windows console, the escape sequences are interpreted and the output
   is in UTF-8, unless the program chose otherwise. Elsewhere, [utf_8] is
   left to the locale. *)
let console_utf_8 ?utf_8 fd =
  match utf_8 with
  | Some _ -> utf_8
  | None ->
      if Sys.win32 && enable_vt fd && Lazy.force utf_8_code_page then Some true
      else None

let setup ?style_renderer ?utf_8 ?theme () =
  Option.iter Console.Theme.set_program theme;
  List.iter
    (fun (ppf, fd) ->
      let utf_8 = console_utf_8 ?utf_8 fd in
      Console.Env.setup ?style_renderer ?utf_8 ~is_tty:(Unix.isatty fd) ppf)
    outputs;
  ignore (Recording.start ~time:(fun () -> 0.) Fmt.stdout : bool)

let ensure () =
  List.iter
    (fun (ppf, fd) ->
      let utf_8 = console_utf_8 fd in
      Console.Env.ensure ?utf_8 ~is_tty:(Unix.isatty fd) ppf)
    outputs

let is_terminal = Terminal.is_terminal
let size = Terminal.size
let width = Terminal.width_of
