(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Effect.Deep

type _ Effect.t += Await : 'a Lwt.t -> 'a Effect.t

exception Stopped

(* Lwt keeps one storage for the whole program, and [Lwt.with_value] gives it
   back only when its function returns. A fiber that waits inside
   [Lwt.with_value] would leave its bindings in place for whatever runs next.
   So each stretch of a fiber runs inside a [Lwt.with_value] of its own, which
   puts the storage back once the fiber waits or ends. The callback that
   resumes the fiber is registered before that, so it captures the fiber's
   bindings and restores them when it runs. *)
let isolation : unit Lwt.key = Lwt.new_key ()
let isolated f = Lwt.with_value isolation None f

let handler stop =
  let resume k v =
    isolated (fun () ->
        if Atomic.get stop then discontinue k Stopped else continue k v)
  in
  let fail k e = isolated (fun () -> discontinue k e) in
  {
    retc = Lwt.return;
    exnc = Lwt.fail;
    effc =
      (fun (type a) (e : a Effect.t) ->
        match e with
        | Await p ->
            Some
              (fun (k : (a, _) continuation) ->
                Lwt.try_bind (fun () -> p) (resume k) (fail k))
        | _ -> None);
  }

let spawn ?(stop = Atomic.make false) f =
  isolated (fun () -> match_with f () (handler stop))

let await p =
  match Lwt.state p with
  | Lwt.Return v -> v
  | Lwt.Fail e -> raise e
  | Lwt.Sleep -> (
      try Effect.perform (Await p)
      with Effect.Unhandled _ ->
        invalid_arg "Console_lwt: a wait outside Console_lwt.run")

let here () = ((Domain.self () :> int), Thread.id (Thread.self ()))
let home : (int * int) option Atomic.t = Atomic.make None
let enter () = Atomic.set home (Some (here ()))

let at_home () =
  match Atomic.get home with
  | Some (d, t) ->
      let d', t' = here () in
      Int.equal d d' && Int.equal t t'
  | None -> false
