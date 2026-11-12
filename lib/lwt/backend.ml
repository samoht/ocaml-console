(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Lwt cannot be woken from another domain or from a signal handler. A
   notification can be sent from both, and it runs its function in the event
   loop. One notification wakes every wait, and each wait checks its condition
   again. *)
let woken = Lwt_condition.create ()
let notification = Lwt_unix.make_notification (Lwt_condition.broadcast woken)

(* [background daemons f] runs [f] in its own fiber, so that a daemon that
   raises can cancel it. The daemon's fiber ends with the exception, then [f]
   is cancelled. When [f] ends, the daemons are cancelled, and the
   [Lwt.Canceled] that they then raise is not a failure. *)
let background daemons f =
  let stop = ref false in
  let p = Fiber.spawn f in
  let watch daemon () =
    match daemon () with
    | () -> None
    | exception Lwt.Canceled when !stop -> None
    | exception e -> Some (e, Printexc.get_raw_backtrace ())
  in
  let spawn daemon =
    let d = Fiber.spawn (watch daemon) in
    Lwt.on_success d (fun r -> if Option.is_some r then Lwt.cancel p);
    d
  in
  let daemons = List.map spawn daemons in
  let result =
    match Fiber.await p with
    | v -> Ok v
    | exception e -> Error (e, Printexc.get_raw_backtrace ())
  in
  stop := true;
  List.iter Lwt.cancel daemons;
  let failed d = match Lwt.state d with Lwt.Return r -> r | _ -> None in
  match (List.find_map failed daemons, result) with
  | Some (e, bt), _ | None, Error (e, bt) -> Printexc.raise_with_backtrace e bt
  | None, Ok v -> v

include Console_driver.Make (struct
  let clock () =
    let c = Mtime_clock.counter () in
    fun () -> Mtime.Span.to_float_ns (Mtime_clock.count c) *. 1e-9

  let tick d = Fiber.await (Lwt_unix.sleep d)
  let sleep = tick

  type 'a key = 'a Lwt.key

  let key = Lwt.new_key
  let get = Lwt.get
  let with_binding k v f = Lwt.with_value k (Some v) f

  type wake = unit

  let wake () = ()
  let notify () = Lwt_unix.send_notification notification

  let rec wait () f =
    match f () with
    | Some v -> v
    | None ->
        Fiber.await (Lwt_condition.wait woken);
        wait () f

  (* Any code on the Lwt thread can bind values, and has no cancellation to
     check. *)
  let in_fiber () = true
  let background = background

  (* [Lwt.cancel] rejects the promise that a fiber waits on, and the fiber then
     raises [Lwt.Canceled]. It leaves no state on the fiber for [check] to
     raise. *)

  let first a _ = a ()
  let protect f = f ()
  let check () = ()
  let cancelled = function Lwt.Canceled -> true | _ -> false
  let write_error = function Unix.Unix_error _ -> true | _ -> false
end)
