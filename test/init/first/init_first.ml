(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A sample is the source file charged and its words. It also keeps every
   source file on its call stack. *)
let samples : (string * string list * int) list ref = ref []

(* The source files of [stack], the outermost first. *)
let files stack =
  match Printexc.backtrace_slots stack with
  | None -> []
  | Some slots ->
      Array.fold_left
        (fun files slot ->
          match Printexc.Slot.location slot with
          | Some l -> l.Printexc.filename :: files
          | None -> files)
        [] slots

let charge (a : Gc.Memprof.allocation) =
  let files = files a.callstack in
  let outermost = match files with file :: _ -> file | [] -> "" in
  samples := (outermost, files, a.n_samples) :: !samples;
  None

let profile =
  Gc.Memprof.start ~sampling_rate:1. ~callstack_size:max_int
    { Gc.Memprof.null_tracker with alloc_minor = charge; alloc_major = charge }

let stopped = ref false

let words ?through () =
  if not !stopped then (
    Gc.Memprof.stop ();
    stopped := true);
  ignore (Sys.opaque_identity profile);
  let kept (_, files, _) =
    match through with None -> true | Some f -> List.mem f files
  in
  let charged = Hashtbl.create 64 in
  List.iter
    (fun ((file, _, n) as s) ->
      if kept s then
        Hashtbl.replace charged file
          (n + Option.value (Hashtbl.find_opt charged file) ~default:0))
    !samples;
  Hashtbl.fold (fun file n acc -> (file, n) :: acc) charged []
