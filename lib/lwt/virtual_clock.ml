(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The clock of recording mode, which moves as that of Console_eio.clock.
   It is an engine that keeps the timers of Lwt_engine.on_timer, such as
   those of Lwt_unix.sleep, and passes every other event to the engine it
   wraps. [timers] holds the pending timers, by time and then by start. Any
   domain may read the time; only the thread of Lwt touches the timers. *)

type timer = { until : Mtime.t; id : int; fire : unit -> unit }
type Lwt_engine.engine_id += Virtual

let time = Atomic.make (Mtime.of_uint64_ns 0L)
let timers = ref []
let next = ref 0
let real = ref None

let before a b =
  match Mtime.compare a.until b.until with 0 -> Int.compare a.id b.id | c -> c

let remove id = timers := List.filter (fun t -> t.id <> id) !timers

let add d id fire =
  let span = Mtime.Span.of_uint64_ns (Int64.of_float (Float.max 0. d *. 1e9)) in
  let until =
    Option.value ~default:Mtime.max_stamp
      (Mtime.add_span (Atomic.get time) span)
  in
  timers := List.merge before !timers [ { until; id; fire } ]

(* Lwt_main.run asks to block only when nothing can run. If the wrapped
   engine has no event either, the time jumps to the earliest timer. *)
class engine (wrapped : Lwt_engine.t) =
  object (self)
    inherit Lwt_engine.abstract
    val mutable woken = false
    method id = Virtual
    method private cleanup = ()

    method private wake f _ =
      woken <- true;
      f ()

    method private register_readable fd f =
      let ev = wrapped#on_readable fd (self#wake f) in
      lazy (Lwt_engine.stop_event ev)

    method private register_writable fd f =
      let ev = wrapped#on_writable fd (self#wake f) in
      lazy (Lwt_engine.stop_event ev)

    method private register_timer d repeat f =
      let id = !next in
      incr next;
      let rec fire () =
        if repeat then add d id fire;
        f ()
      in
      add d id fire;
      lazy (remove id)

    method iter block =
      woken <- false;
      wrapped#iter false;
      if block && (not woken) && Lwt.paused_count () = 0 then
        match !timers with
        | t :: rest ->
            timers := rest;
            Atomic.set time t.until;
            t.fire ()
        | [] -> wrapped#iter true
  end

let installed () = match Lwt_engine.id () with Virtual -> true | _ -> false

let use recording =
  match (recording, installed (), !real) with
  | true, false, _ ->
      let wrapped = Lwt_engine.get () in
      real := Some wrapped;
      Lwt_engine.set ~destroy:false (new engine wrapped)
  | false, true, Some wrapped -> Lwt_engine.set ~destroy:false wrapped
  | _ -> ()

let elapsed () =
  let now =
    if installed () then fun () -> Atomic.get time else Mtime_clock.now
  in
  let start = now () in
  fun () -> Mtime.Span.to_float_ns (Mtime.span start (now ())) *. 1e-9
