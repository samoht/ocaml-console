(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let hide_cursor = "\027[?25l"
let show_cursor = "\027[?25h"

(* The restore flushes first: [Format.pp_set_margin] reinitialises the
   formatter and discards what is queued, and under a margin of [max_int]
   nothing is written before a flush, so a body that wrote without flushing
   would lose its output. *)
let with_unlimited_margin ppf f =
  let old_margin = Format.pp_get_margin ppf () in
  Format.pp_set_margin ppf max_int;
  Fun.protect f ~finally:(fun () ->
      Format.pp_print_flush ppf ();
      Format.pp_set_margin ppf old_margin)

let pp_clear ppf n =
  if n > 0 then begin
    Format.pp_print_string ppf "\r\027[K";
    for _ = 1 to n - 1 do
      Format.pp_print_string ppf "\027[1A\027[2K"
    done
  end

let pp_block ppf lines =
  let rec write = function
    | [] -> ()
    | [ last ] -> Format.pp_print_string ppf last
    | line :: rest ->
        Format.pp_print_string ppf line;
        (* A row may occupy the terminal's final column.  An explicit carriage
           return cancels VT pending-wrap before the line feed, independently
           of whether the host tty has ONLCR enabled. *)
        Format.pp_print_string ppf "\r\n";
        write rest
  in
  write lines

(* A row is erased {e after} its replacement is written, so there is no blank
   flash; the erase only removes a stale suffix when the new row is
   shorter. *)
let pp_update ppf ~old_lines ~new_lines =
  let old_lines = Array.of_list old_lines in
  let new_lines = Array.of_list new_lines in
  let n = Array.length new_lines in
  if n > 0 then begin
    Format.pp_print_string ppf "\r";
    for _ = 1 to n - 1 do
      Format.pp_print_string ppf "\027[1A"
    done;
    for i = 0 to n - 1 do
      if not (String.equal old_lines.(i) new_lines.(i)) then begin
        Format.pp_print_string ppf new_lines.(i);
        Format.pp_print_string ppf "\027[K"
      end;
      if i < n - 1 then Format.pp_print_string ppf "\r\n"
    done
  end

let show_cursor_on ppf =
  try
    Format.pp_print_string ppf show_cursor;
    Format.pp_print_flush ppf ()
  with Sys_error _ -> ()

type region = {
  ppf : Format.formatter;
  clear : unit -> unit;
  redraw : unit -> unit;
}

(* Read by the [at_exit] handler. *)
let active_region : region option Atomic.t = Atomic.make None
let active () = Atomic.get active_region
let set_active r = Atomic.set active_region r
let activity_claimed = Atomic.make false
let claim () = Atomic.compare_and_set activity_claimed false true
let release () = Atomic.set activity_claimed false

(* [exit] ends the process without unwinding, so a command that calls it inside
   a display skips the display's teardown; the exit handlers still run. *)
let restore () =
  match active () with
  | None -> ()
  | Some r ->
      (try Format.pp_print_string r.ppf "\027[0m\r\n" with Sys_error _ -> ());
      show_cursor_on r.ppf

let restore_at_exit = lazy (at_exit restore)
let restore_at_exit () = Lazy.force restore_at_exit
