(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A signal the program left at its default disposition ends the process
   without running a finaliser. While something holds the terminal, such a
   signal is caught: what holds it gives it back, the default disposition is
   put back and the signal sent again, so the process still ends as the
   signal says. SIGTSTP is caught the same way but stops the process: what
   holds the terminal gives it back around the stop. A disposition the
   program chose is never replaced.

   A handler runs at any safe point, in the middle of a frame's write
   included: what holds the terminal gives it back at its next idle point,
   and the signal is sent again from there. *)

type holder = {
  id : int;
  owner : Domain.id option;
  idle : (unit -> unit) -> unit;
  restore : unit -> unit;
  suspend : (unit -> unit) -> unit;
  resume : unit -> unit;
  terminal : Unix.file_descr option;
}

let ends = [ Sys.sigint; Sys.sigterm ]
let holders : holder list Atomic.t = Atomic.make []
let caught = Atomic.make []
let ending = Atomic.make false
let next = Atomic.make 0

(* How long a signal waits for an idle point. *)
let grace = 1.

let rec update a f =
  let old = Atomic.get a in
  if not (Atomic.compare_and_set a old (f old)) then update a f

(* A holder with an idle point is reached from its own domain only. *)
let here h =
  match h.owner with
  | None -> true
  | Some d -> Int.equal (d :> int) (Domain.self () :> int)

(* Nothing a restoration raises keeps the signal from going on. *)
let attempt f = try f () with e when not (Task_driver.is_async e) -> ()

(* What gives a terminal back with no state of the display: CAN and ST end
   an escape code cut in half, then the end of a link, of the colours, of a
   synchronized update, and the cursor shown. *)
let sequence = "\024\027\\\027]8;;\027\\\027[0m\027[?2026l\027[?25h"
let dead = Atomic.make false
let written = Atomic.make false
let is_dead () = Atomic.get dead

(* Once [dead] is set, the display writes nothing more, so that no frame
   follows the sequence; a write already under way in the kernel cannot be
   stopped. The terminal is non-blocking: the sequence is written at most
   once, or not at all. *)
let give_up hs =
  Atomic.set dead true;
  if not (Atomic.exchange written true) then
    List.iter
      (fun h ->
        Option.iter
          (fun fd ->
            try
              ignore
                (Unix.single_write_substring fd sequence 0
                   (String.length sequence))
            with Unix.Unix_error _ -> ())
          h.terminal)
      hs

let last_resort () = give_up (Atomic.get holders)

let reopen () =
  Atomic.set written false;
  Atomic.set dead false

let after_grace f =
  ignore
    (Thread.create
       (fun () ->
         Thread.delay grace;
         f ())
       ()
      : Thread.t)

(* The process stops as the default disposition stops it, inside every
   suspension, innermost outermost; [kill] returns once it is continued. The
   runtime blocks a signal while its handler runs, so it is unblocked for the
   stop; in a process group that no shell controls, the system discards it. A
   suspension that does not start within [grace], or one in another domain,
   gives way to {!give_up} around the stop, and to [resume] once continued. *)
let rec stop signo =
  let stop () =
    Sys.set_signal signo Sys.Signal_default;
    let mask = Unix.sigprocmask Unix.SIG_UNBLOCK [ signo ] in
    Unix.kill (Unix.getpid ()) signo;
    ignore (Unix.sigprocmask Unix.SIG_SETMASK mask : int list);
    Sys.set_signal signo (Sys.Signal_handle stop)
  in
  let hs = Atomic.get holders in
  let state = Atomic.make `Waiting in
  let taken h =
    Option.is_none h.owner
    || Atomic.compare_and_set state `Waiting `Taken
    || Atomic.get state = `Taken
  in
  let around =
    List.fold_left
      (fun k h () -> h.idle (fun () -> if taken h then h.suspend k))
      stop (List.rev hs)
  in
  let abandon () =
    if Atomic.compare_and_set state `Waiting `Abandoned then begin
      give_up hs;
      Fun.protect stop ~finally:(fun () ->
          reopen ();
          List.iter (fun h -> attempt h.resume) hs)
    end
  in
  if List.for_all here hs then begin
    attempt around;
    if Atomic.get state = `Waiting && List.exists (fun h -> h.owner <> None) hs
    then after_grace (fun () -> attempt abandon)
  end
  else attempt abandon

(* Our handler gives way to the default disposition, and one the program
   installed since stays. *)
let rec release () =
  List.iter
    (fun s ->
      match Sys.signal s Sys.Signal_default with
      | Sys.Signal_handle h when h == handle || h == stop -> ()
      | theirs -> Sys.set_signal s theirs)
    (Atomic.exchange caught [])

(* The signal is sent again once, after the last restoration or once
   [grace] has passed, whichever comes first, armed before any restoration
   runs. A holder of another domain is given up at once. An ending signal
   that comes before then is the same ending. *)
and handle signo =
  if not (Atomic.exchange ending true) then begin
    let sent = Atomic.make false in
    let send () =
      if not (Atomic.exchange sent true) then begin
        release ();
        Atomic.set ending false;
        Unix.kill (Unix.getpid ()) signo
      end
    in
    let hs = Atomic.exchange holders [] in
    after_grace (fun () ->
        if not (Atomic.get sent) then begin
          give_up hs;
          send ()
        end);
    let rec close = function
      | [] -> send ()
      | h :: rest when not (here h) ->
          give_up [ h ];
          close rest
      | h :: rest ->
          h.idle (fun () ->
              attempt h.restore;
              close rest)
    in
    attempt (fun () -> close hs)
  end

let catch () =
  List.iter
    (fun (s, h) ->
      match Sys.signal s (Sys.Signal_handle h) with
      | Sys.Signal_default -> update caught (List.cons s)
      | chosen -> Sys.set_signal s chosen)
    ((Sys.sigtstp, stop) :: List.map (fun s -> (s, handle)) ends)

let protect ?idle ?(suspend = fun stop -> stop ()) ?(resume = ignore) ?terminal
    ~restore f =
  let id = Atomic.fetch_and_add next 1 in
  let owner = Option.map (fun _ -> Domain.self ()) idle in
  let idle = Option.value idle ~default:(fun k -> k ()) in
  if Atomic.get holders = [] then catch ();
  update holders
    (List.cons { id; owner; idle; restore; suspend; resume; terminal });
  Fun.protect f ~finally:(fun () ->
      update holders (List.filter (fun h -> h.id <> id));
      if Atomic.get holders = [] && not (Atomic.get ending) then release ())
