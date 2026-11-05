(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The seconds since the display started, on [clock]. *)
let seconds clock =
  let start = Eio.Time.Mono.now clock in
  fun () ->
    Mtime.Span.to_float_ns (Mtime.span start (Eio.Time.Mono.now clock)) *. 1e-9

let deterministic = Cast.deterministic

(* The virtual clock of CONSOLE_DETERMINISTIC_CLOCK. A sleep of [d] started
   at time [t] wakes at [t + d], once it has also lasted [d] on [real].
   Sleeps wake one at a time, by wake-up time and then by start, and each
   sets the time to its wake-up time as its fiber resumes. Before the next
   one wakes, {!settle} lets the fibers that the last one made runnable run,
   until two yields in a row see no use of the clock. So the time does not
   depend on the order in which the real durations end.

   A passive sleep, that of a redraw, wakes in the same order, but only while
   an active sleep, one of the program, comes after it, so that a redraw
   never moves the time further than the program would. It stops waiting on
   [real] as soon as an active sleep after it has lasted its real duration,
   so that it never holds the program up. For a recording, which has its own
   times, no sleep waits on [real]. *)
module Sleeps = struct
  type sleep = {
    until : Mtime.t;
    seq : int;
    active : bool;
    mutable elapsed : bool;
    mutable pending : bool;
    mutable waiting : unit Eio.Promise.u option;
    overtaken : unit Eio.Promise.t * unit Eio.Promise.u;
  }

  type t = {
    real : Eio.Time.Mono.ty Eio.Resource.t;
    waits : bool;
    mutable now : Mtime.t;
    mutable sleeps : sleep list;
    mutable next : int;
    mutable used : int;
  }

  type time = Mtime.t

  let now t =
    t.used <- t.used + 1;
    t.now

  let before a b =
    match Mtime.compare a.until b.until with 0 -> a.seq < b.seq | c -> c < 0

  let rec insert s = function
    | x :: rest when before x s -> x :: insert s rest
    | l -> s :: l

  let due t =
    match t.sleeps with
    | s :: rest
      when s.elapsed && (s.active || List.exists (fun x -> x.active) rest) ->
        Some s
    | _ -> None

  let is_due t s = match due t with Some x -> x == s | None -> false

  let wake t =
    match due t with
    | Some ({ waiting = Some u; _ } as s) ->
        s.waiting <- None;
        Eio.Promise.resolve u ()
    | _ -> ()

  let remove t s =
    if s.pending then begin
      s.pending <- false;
      t.sleeps <- List.filter (fun x -> x != s) t.sleeps;
      wake t
    end

  (* The rounds are bounded, so that a fiber that polls the clock in a loop
     of yields cannot stop it. *)
  let settle t =
    let rec go quiet rounds =
      if quiet < 2 && rounds < 64 then begin
        let used = t.used in
        Eio.Fiber.yield ();
        go (if t.used = used then quiet + 1 else 0) (rounds + 1)
      end
    in
    go 0 0

  let rec turn t s =
    if is_due t s then begin
      settle t;
      if is_due t s then begin
        t.now <- s.until;
        t.used <- t.used + 1;
        remove t s
      end
      else turn t s
    end
    else begin
      let p, u = Eio.Promise.create () in
      s.waiting <- Some u;
      Eio.Promise.await p;
      turn t s
    end

  let overtake t s =
    let rec go = function
      | x :: rest when x != s ->
          if not x.active then
            ignore (Eio.Promise.try_resolve (snd x.overtaken) ());
          go rest
      | _ -> ()
    in
    go t.sleeps

  let overtaken t s =
    List.exists (fun x -> x.active && x.elapsed && before s x) t.sleeps

  let elapse t s span =
    if not t.waits then ()
    else if s.active then begin
      Eio.Time.Mono.sleep_span t.real span;
      overtake t s
    end
    else if not (overtaken t s) then
      Eio.Fiber.first
        (fun () -> Eio.Time.Mono.sleep_span t.real span)
        (fun () -> Eio.Promise.await (fst s.overtaken))

  let sleep t ~active time =
    if Mtime.is_later time ~than:t.now then begin
      let s =
        {
          until = time;
          seq = t.next;
          active;
          elapsed = false;
          pending = true;
          waiting = None;
          overtaken = Eio.Promise.create ();
        }
      in
      t.next <- t.next + 1;
      t.used <- t.used + 1;
      t.sleeps <- insert s t.sleeps;
      wake t;
      Fun.protect
        ~finally:(fun () -> remove t s)
        (fun () ->
          elapse t s (Mtime.span t.now time);
          s.elapsed <- true;
          turn t s)
    end

  let sleep_until t time = sleep t ~active:true time

  let redraw t d =
    let span = Mtime.Span.of_uint64_ns (Int64.of_float (d *. 1e9)) in
    Option.iter (sleep t ~active:false) (Mtime.add_span t.now span)

  (* The time, read without counting as a use. *)
  let seconds t () = Int64.to_float (Mtime.to_uint64_ns t.now) *. 1e-9
end

type (_, _, _) Eio.Resource.pi +=
  | Redraw : ('t, 't -> float -> unit, [> `Console_sleeps ]) Eio.Resource.pi
  | Seconds : ('t, 't -> unit -> float, [> `Console_sleeps ]) Eio.Resource.pi

let sleeps =
  Eio.Resource.handler
    (H (Redraw, Sleeps.redraw)
    :: H (Seconds, Sleeps.seconds)
    :: Eio.Resource.bindings (Eio.Time.Pi.clock (module Sleeps)))

let clock c =
  let real = (c :> Eio.Time.Mono.ty Eio.Resource.t) in
  if deterministic () then
    Eio.Resource.T
      ( {
          Sleeps.real;
          waits = Option.is_none (Cast.file ());
          now = Mtime.of_uint64_ns 0L;
          sleeps = [];
          next = 0;
          used = 0;
        },
        sleeps )
  else real

(* The sleep of a redraw on a virtual clock of {!clock}, which waits on its
   time; [None] for another clock. *)
let redraw (Eio.Resource.T (t, ops)) =
  Option.map (fun redraw -> redraw t) (Eio.Resource.get_opt ops Redraw)

(* The time of a virtual clock of {!clock}; zero for another clock. *)
let virtual_time clock =
  let (Eio.Resource.T (t, ops)) = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  match Eio.Resource.get_opt ops Seconds with
  | Some seconds -> seconds t
  | None -> fun () -> 0.

(* The time the display reads and the sleep its redraws wait on. Under
   CONSOLE_DETERMINISTIC_CLOCK, a virtual clock of {!clock} gives both, and a
   redraw does not move its time; with any other clock the time stays at
   zero. *)
let reading_now clock =
  if not (deterministic ()) then seconds clock
  else match redraw clock with Some _ -> seconds clock | None -> fun () -> 0.

let reading_sleep clock =
  if not (deterministic ()) then Eio.Time.Mono.sleep clock
  else
    match redraw clock with
    | Some redraw -> redraw
    | None -> Eio.Time.Mono.sleep clock
