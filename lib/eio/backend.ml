(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Console_driver.Make (struct
  type clock = Eio.Time.Mono.ty Eio.Resource.t

  let elapsed = Virtual_clock.elapsed
  let tick = Virtual_clock.tick
  let recording_time = Virtual_clock.recording_time

  type 'a key = 'a Eio.Fiber.key

  let key = Eio.Fiber.create_key
  let get k = try Eio.Fiber.get k with Effect.Unhandled _ -> None
  let with_binding = Eio.Fiber.with_binding

  type wake = Eio.Condition.t

  let wake = Eio.Condition.create
  let notify = Eio.Condition.broadcast
  let wait = Eio.Condition.loop_no_mutex

  let in_fiber () =
    match Eio.Fiber.is_cancelled () with
    | _ -> true
    | exception Effect.Unhandled _ -> false

  let background daemons f =
    Eio.Switch.run @@ fun sw ->
    List.iter
      (fun daemon ->
        Eio.Fiber.fork_daemon ~sw (fun () ->
            daemon ();
            `Stop_daemon))
      daemons;
    f ()

  let first a b = Eio.Fiber.first a b
  let protect = Eio.Cancel.protect
  let check = Eio.Fiber.check
  let cancelled = function Eio.Cancel.Cancelled _ -> true | _ -> false
  let write_error = function Eio.Io _ -> true | _ -> false

  (* Outside an event loop, as in a domain that Eio does not run, there is no
     time to wait on. *)
  let sleep d =
    match Eio_unix.sleep d with
    | () -> ()
    | exception Effect.Unhandled _ -> Eio.Fiber.await_cancel ()
end)
