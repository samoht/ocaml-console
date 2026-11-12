(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The clock of the run, which [Console_eio.run] binds. *)
let clock : Eio.Time.Mono.ty Eio.Resource.t Eio.Fiber.key =
  Eio.Fiber.create_key ()

let run_clock () =
  match Eio.Fiber.get clock with
  | Some c -> c
  | None -> invalid_arg "Console_eio: a display outside Console_eio.run"

include Console_driver.Make (struct
  let clock () = Virtual_clock.time (run_clock ())
  let tick d = Virtual_clock.tick (run_clock ()) d

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

  (* Outside an event loop, for example in a domain that Eio does not run,
     there is no time to wait on, so [sleep] waits for a cancellation. *)

  let sleep d =
    match Eio_unix.sleep d with
    | () -> ()
    | exception Effect.Unhandled _ -> Eio.Fiber.await_cancel ()
end)
