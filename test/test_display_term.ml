(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

exception Broken_formatter

let test_failed_install_releases_owner () =
  let broken =
    Format.make_formatter
      (fun _buffer _offset _length -> raise Broken_formatter)
      (fun () -> raise Broken_formatter)
  in
  let now () = 0. in
  Alcotest.check_raises "formatter error is preserved" Broken_formatter
    (fun () ->
      Display.run
        ~ctx:(ctx ~is_tty:true ~ppf:broken ~now ())
        ~mode:`Activity ignore);
  let buffer = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buffer in
  Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ()) ~mode:`Activity
    (fun display ->
      let task = Display.task display "recovered" in
      Display.succeed task)

let hide = "\027[?25l"
let show = "\027[?25h"

(* A signal handler that ends the process is the one exit [run] never sees,
   so it asks the display for the terminal back before it re-raises. What the
   buffer holds at the call is the reading: the close after it shows the
   cursor again on its own. *)
let test_restore_terminal_from_a_handler () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  let at_the_call = ref "" in
  Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ()) ~mode:`Activity
    (fun display ->
      let task = Display.task display "work" in
      Display.restore_terminal ();
      at_the_call := Buffer.contents buffer;
      Display.succeed task);
  Alcotest.(check bool)
    "the activity projection hid the cursor" true
    (contains hide !at_the_call);
  Alcotest.(check bool)
    "restore_terminal shows it, flushed, at the call" true
    (String.ends_with ~suffix:show !at_the_call);
  let (), history =
    capture (fun display _ ->
        let task = Display.task display "work" in
        Display.restore_terminal ();
        Display.succeed task)
  in
  Alcotest.(check bool)
    "a history display hid nothing, so nothing is shown" false
    (contains show history);
  (* Outside a run there is no terminal to give back, and no error either. *)
  Display.restore_terminal ()

(* A close that writes its header line hands the terminal back at the start
   of a line with nothing of the display on it: the cursor is shown before
   that line ends, so what the program writes next (its outcome on standard
   output, a shell prompt) begins with its own first byte, not with the
   display's escape. *)
let test_close_ends_on_a_line_end () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  Display.run ~ctx:(ctx ~is_tty:true ~ppf ~now ())
    ~mode:`Activity ~header:"land" (fun display ->
      let task = Display.task display "work" in
      Display.succeed task);
  Format.pp_print_flush ppf ();
  let bytes = Buffer.contents buffer in
  Alcotest.(check bool) "the cursor came back" true (contains show bytes);
  Alcotest.(check bool)
    "the last byte the display wrote ends a line" true
    (String.ends_with ~suffix:"\r\n" bytes)

let test_differential_redraw_and_right_edge () =
  let width = 48 and height = 8 in
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  let now () = 0. in
  let vte = Vte.create ~rows:height ~cols:width () in
  Display.run ~ctx:(ctx ~is_tty:true ~width ~height ~ppf ~now ())
    ~mode:`Activity ~width ~height (fun display ->
      ignore (Display.task display "[R1] prepare");
      let changed = Display.task display "[R2] download" in
      ignore (Display.task display "[R3] verify");
      Display.tick display;
      Format.pp_print_flush ppf ();
      Vte.feed_string vte (Buffer.contents buffer);
      Buffer.clear buffer;
      let initial = Vte.to_string vte |> String.split_on_char '\n' in
      let first = List.nth initial 0 and last = List.nth initial 2 in
      for cur = 1 to 10 do
        Vte.clear_dirty vte;
        Display.set_count changed ~cur ~total:10;
        Display.tick display;
        Format.pp_print_flush ppf ();
        Vte.feed_string vte (Buffer.contents buffer);
        Buffer.clear buffer;
        Alcotest.(check (list int))
          "only the changed terminal row is repainted" [ 1 ]
          (Vte.dirty_rows vte);
        let rows = Vte.to_string vte |> String.split_on_char '\n' in
        Alcotest.(check string)
          "preceding sibling is stable" first (List.nth rows 0);
        Alcotest.(check string)
          "following sibling is stable" last (List.nth rows 2);
        let row = List.nth rows 1 in
        Alcotest.(check int)
          "elapsed reaches right edge" width (Width.string_width row);
        Alcotest.(check bool)
          "right edge is metadata" true
          (String.ends_with ~suffix:"0.0s" row)
      done;
      Alcotest.(check int)
        "right-edge redraw never scrolls" 0 (Vte.scrollback_size vte);
      Alcotest.(check (pair int int))
        "cursor rests on the final activity row" (2, 0) (Vte.cursor_pos vte))

let suite =
  ( "display_term",
    [
      Alcotest.test_case "failed install releases owner" `Quick
        test_failed_install_releases_owner;
      Alcotest.test_case "restore_terminal from a handler" `Quick
        test_restore_terminal_from_a_handler;
      Alcotest.test_case "close ends on a line end" `Quick
        test_close_ends_on_a_line_end;
      Alcotest.test_case "differential redraw and right edge" `Quick
        test_differential_redraw_and_right_edge;
    ] )
