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
   that raises can cancel it: the daemon's fiber ends with what it raised,
   then [f] is cancelled. Once [f] ends, the daemons are cancelled, and the
   [Lwt.Canceled] they raise then is not a failure. *)
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
  type clock = unit

  let elapsed = Virtual_clock.elapsed
  let tick () d = Fiber.await (Lwt_unix.sleep d)
  let recording_time = Virtual_clock.elapsed
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
