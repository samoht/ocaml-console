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
        Format.pp_print_char ppf '\n';
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
      if i < n - 1 then Format.pp_print_char ppf '\n'
    done
  end

let show_cursor_on ppf =
  try
    Format.pp_print_string ppf show_cursor;
    Format.pp_print_flush ppf ()
  with Sys_error _ -> ()

(* The display running now, and the formatter of its live region if it draws
   one. Read by the [at_exit] handler. *)
let current : Format.formatter option option Atomic.t = Atomic.make None
let claim ~live = Atomic.compare_and_set current None (Some live)
let release () = Atomic.set current None

let restore () =
  match Atomic.get current with
  | Some (Some ppf) ->
      (try Format.pp_print_string ppf "\027[0m\r\n" with Sys_error _ -> ());
      show_cursor_on ppf
  | Some None | None -> ()

let restore_at_exit = lazy (at_exit restore)
let restore_at_exit () = Lazy.force restore_at_exit
