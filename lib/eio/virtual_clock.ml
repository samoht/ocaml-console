(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let deterministic = Cast.deterministic

(* The seconds since [seconds clock] was applied, on [clock]. *)
let seconds clock =
  let start = Eio.Time.Mono.now clock in
  fun () ->
    Mtime.Span.to_float_ns (Mtime.span start (Eio.Time.Mono.now clock)) *. 1e-9

(* The clock of recording mode (Console_eio, "Deterministic runs"). [sleeps]
   holds the pending sleeps, by wake-up time and then by start. [used]
   counts the uses of the clock, which tell {!settle} that a fiber runs. *)
module Virtual = struct
  type t = {
    mutable now : Mtime.t;
    mutable sleeps : (Mtime.t * int) list;
    mutable next : int;
    mutable used : int;
  }

  type time = Mtime.t

  let now t =
    t.used <- t.used + 1;
    t.now

  let before (a, i) (b, j) =
    match Mtime.compare a b with 0 -> Int.compare i j | c -> c

  (* Eio does not say when nothing can run, so this guesses: nothing can
     once two yields in a row see no use of the clock. A fiber that yields
     without using the clock can be overtaken. The rounds are bounded, so
     that a fiber that polls the clock in a loop of yields cannot stop it.
     https://github.com/ocaml-multicore/eio/issues/965 asks for a hook. *)
  let settle t =
    let rec go quiet rounds =
      if quiet < 2 && rounds < 64 then begin
        let used = t.used in
        Eio.Fiber.yield ();
        go (if t.used = used then quiet + 1 else 0) (rounds + 1)
      end
    in
    go 0 0

  (* Each pending sleep settles in a loop. The earliest one then sets the
     time to its own and returns. *)
  let sleep_until t time =
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
            t.used <- t.used + 1
        | _ -> turn ()
      in
      Fun.protect turn ~finally:(fun () ->
          t.sleeps <- List.filter (fun (_, x) -> x <> id) t.sleeps)
    end
end

type (_, _, _) Eio.Resource.pi +=
  | Virtual : ('t, 't -> Virtual.t, [> `Console_virtual ]) Eio.Resource.pi

let handler =
  Eio.Resource.handler
    (H (Virtual, Fun.id)
    :: Eio.Resource.bindings (Eio.Time.Pi.clock (module Virtual)))

let clock c =
  if deterministic () then
    let zero = Mtime.of_uint64_ns 0L in
    Eio.Resource.T
      ({ Virtual.now = zero; sleeps = []; next = 0; used = 0 }, handler)
  else (c :> Eio.Time.Mono.ty Eio.Resource.t)

let state (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Virtual)

let elapsed clock =
  if deterministic () && Option.is_none (state clock) then fun () -> 0.
  else seconds clock

let recording_time clock =
  match state clock with
  | Some t -> fun () -> Int64.to_float (Mtime.to_uint64_ns t.now) *. 1e-9
  | None -> fun () -> 0.
