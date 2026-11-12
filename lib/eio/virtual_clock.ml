(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let deterministic = Cast.deterministic

(* The clock of deterministic mode (see Console_eio, "Deterministic mode").
   [sleeps] holds the program's pending sleeps, sorted by wake-up time, then by
   start order. [used] counts the uses of the clock. A change in [used] tells
   {!settle} that a fiber is running. [moved] wakes the redraws when the time
   moves. *)
module Virtual = struct
  type t = {
    owner : Domain.id;
    moved : Eio.Condition.t;
    mutable now : Mtime.t;
    mutable sleeps : (Mtime.t * int) list;
    mutable next : int;
    mutable used : int;
  }

  type time = Mtime.t

  let check t =
    if Domain.self () <> t.owner then
      invalid_arg
        "Console_eio.clock: used from another domain in deterministic mode; \
         start domains with Console_eio.domain_mgr"

  let now t =
    check t;
    t.used <- t.used + 1;
    t.now

  let before (a, i) (b, j) =
    match Mtime.compare a b with 0 -> Int.compare i j | c -> c

  (* Eio does not tell when nothing can run, so [settle] guesses. It assumes
     that nothing can run once two yields in a row see no use of the clock. A
     fiber that yields without using the clock can be overtaken. The number of
     rounds is bounded, so a fiber that polls the clock in a loop of yields
     cannot block [settle]. https://github.com/ocaml-multicore/eio/issues/965
     asks for a hook. *)
  let settle t =
    let rec go quiet rounds =
      if quiet < 2 && rounds < 64 then begin
        let used = t.used in
        Eio.Fiber.yield ();
        go (if t.used = used then quiet + 1 else 0) (rounds + 1)
      end
    in
    go 0 0

  (* Each pending sleep calls {!settle} in a loop. Then the earliest sleep sets
     the time to its wake-up time and returns. *)

  let sleep_until t time =
    check t;
    if Mtime.is_later time ~than:t.now then begin
      let id = t.next in
      t.next <- id + 1;
      t.used <- t.used + 1;
      t.sleeps <- List.merge before t.sleeps [ (time, id) ];
      let rec turn () =
        settle t;
        match t.sleeps with
        | (_, first) :: rest when first = id ->
            t.sleeps <- rest;
            t.now <- time;
            t.used <- t.used + 1;
            Eio.Condition.broadcast t.moved
        | _ -> turn ()
      in
      Fun.protect turn ~finally:(fun () ->
          t.sleeps <- List.filter (fun (_, x) -> x <> id) t.sleeps)
    end

  (* A redraw waits until the program's sleeps move the time [d] on. *)
  let redraw t d =
    check t;
    let span = Mtime.Span.of_uint64_ns (Int64.of_float (d *. 1e9)) in
    match Mtime.add_span t.now span with
    | None -> Eio.Fiber.await_cancel ()
    | Some until ->
        while Mtime.is_earlier t.now ~than:until do
          Eio.Condition.await_no_mutex t.moved
        done
end

type (_, _, _) Eio.Resource.pi +=
  | Virtual : ('t, 't -> Virtual.t, [> `Console_virtual ]) Eio.Resource.pi

let handler =
  Eio.Resource.handler
    (H (Virtual, Fun.id)
    :: Eio.Resource.bindings (Eio.Time.Pi.clock (module Virtual)))

let clock c =
  if deterministic () then
    let t =
      {
        Virtual.owner = Domain.self ();
        moved = Eio.Condition.create ();
        now = Mtime.of_uint64_ns 0L;
        sleeps = [];
        next = 0;
        used = 0;
      }
    in
    Eio.Resource.T (t, handler)
  else (c :> Eio.Time.Mono.ty Eio.Resource.t)

let state (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Virtual)

let time clock =
  if deterministic () && Option.is_none (state clock) then fun () -> 0.
  else fun () ->
    Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now clock)) *. 1e-9

let tick clock =
  match state clock with
  | Some t -> Virtual.redraw t
  | None -> Eio.Time.Mono.sleep clock
