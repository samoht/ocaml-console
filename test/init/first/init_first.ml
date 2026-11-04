(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Each sample: the source file of the outermost frame with a location, and
   its words. *)
let samples : (string * int) list ref = ref []

let outermost stack =
  match Printexc.backtrace_slots stack with
  | None -> ""
  | Some slots ->
      Array.fold_left
        (fun file slot ->
          match Printexc.Slot.location slot with
          | Some l -> l.Printexc.filename
          | None -> file)
        "" slots

let charge (a : Gc.Memprof.allocation) =
  samples := (outermost a.callstack, a.n_samples) :: !samples;
  None

let profile =
  Gc.Memprof.start ~sampling_rate:1. ~callstack_size:max_int
    { Gc.Memprof.null_tracker with alloc_minor = charge; alloc_major = charge }

let stopped = ref false

let words () =
  if not !stopped then (
    Gc.Memprof.stop ();
    stopped := true);
  ignore (Sys.opaque_identity profile);
  let charged = Hashtbl.create 64 in
  List.iter
    (fun (file, n) ->
      Hashtbl.replace charged file
        (n + Option.value (Hashtbl.find_opt charged file) ~default:0))
    !samples;
  Hashtbl.fold (fun file n acc -> (file, n) :: acc) charged []
