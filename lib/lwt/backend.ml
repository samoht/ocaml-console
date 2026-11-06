(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Lwt cannot be woken from another domain or from a signal handler. A
   notification can be sent from both, and runs its function in the event
   loop. One notification wakes every wait, and each looks again. *)
let woken = Lwt_condition.create ()
let notification = Lwt_unix.make_notification (Lwt_condition.broadcast woken)

(* [background daemons f] runs [f] in a fiber of its own, so that a daemon
   that raises can cancel it. Once [f] ends, a daemon that resumes raises
   [Fiber.Stopped] at once, so none redraws a display that [f]'s caller
   closes, even one whose wait does not take [Lwt.cancel]. *)
let background daemons f =
  let stop = Atomic.make false in
  let failure = ref None in
  let p = Fiber.spawn f in
  let watch daemon () =
    try daemon () with
    | _ when Atomic.get stop -> ()
    | e ->
        let bt = Printexc.get_raw_backtrace () in
        if Option.is_none !failure then failure := Some (e, bt);
        Lwt.cancel p
  in
  let daemons = List.map (fun d -> Fiber.spawn ~stop (watch d)) daemons in
  let result =
    match Fiber.await p with
    | v -> Ok v
    | exception e -> Error (e, Printexc.get_raw_backtrace ())
  in
  Atomic.set stop true;
  List.iter Lwt.cancel daemons;
  match (!failure, result) with
  | Some (e, bt), _ | None, Error (e, bt) -> Printexc.raise_with_backtrace e bt
  | None, Ok v -> v

include Console_driver.Make (struct
  type clock = Clock.t

  let elapsed = Clock.elapsed
  let tick c d = Fiber.await (Clock.sleep c d)
  let recording_time = Clock.elapsed
  let sleep d = Fiber.await (Lwt_unix.sleep d)

  type 'a key = 'a Lwt.key

  let key = Lwt.new_key

  (* Lwt has one storage for the whole program, so another thread or domain
     would read the bindings of whatever Lwt runs at that moment. *)
  let get k = if Fiber.at_home () then Lwt.get k else None
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

  let in_fiber = Fiber.at_home
  let background = background

  (* [Lwt.cancel] rejects the promise a fiber waits on, which then raises
     [Lwt.Canceled]. It leaves no state on the fiber for [check] to raise. *)
  let first a _ = a ()
  let protect f = f ()
  let check () = ()
  let cancelled = function Lwt.Canceled -> true | _ -> false
  let write_error = function Unix.Unix_error _ -> true | _ -> false
end)
