(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A signal at its default disposition ends the process without running any
   finaliser. While a holder has the terminal, we catch such a signal. The
   holder gives the terminal back. Then we put back the default disposition
   and send the signal again, so the process still ends by that signal.
   SIGTSTP is caught the same way, but it stops the process, and the holder
   gives the terminal back for the time of the stop. We never replace a
   disposition that the program chose.

   A handler can run at any safe point, even in the middle of writing a frame.
   So the holder gives the terminal back at its next idle point, and the
   signal is sent again from there. *)

type holder = {
  id : int;
  owner : Domain.id option;
  idle : (unit -> unit) -> unit;
  restore : unit -> unit;
  suspend : (unit -> unit) -> unit;
  resume : unit -> unit;
  terminal : Unix.file_descr option;
}

(* Windows has SIGINT and SIGTERM, but no SIGTSTP. *)
let ends = [ Sys.sigint; Sys.sigterm ]
let stops = if Sys.win32 then [] else [ Sys.sigtstp ]
let holders : holder list Atomic.t = Atomic.make []
let caught = Atomic.make []
let ending = Atomic.make false
let next = Atomic.make 0

(* How long, in seconds, a signal waits for an idle point. *)
let grace = 1.

let rec update a f =
  let old = Atomic.get a in
  if not (Atomic.compare_and_set a old (f old)) then update a f

(* [here h] is [true] iff the calling domain can reach [h]. A holder with an
   idle point is reachable only from its own domain. *)
let here h =
  match h.owner with
  | None -> true
  | Some d -> Int.equal (d :> int) (Domain.self () :> int)

(* [attempt f] runs [f] and drops any exception that is not asynchronous, so
   that a failed restoration does not stop the signal. *)
let attempt f = try f () with e when not (Task_driver.is_async e) -> ()

(* Bytes that give a terminal back without knowing the display's state. CAN
   and ST end a half-written escape code. The rest ends a link, resets the
   colours, ends a synchronized update and shows the cursor. *)
let sequence = "\024\027\\\027]8;;\027\\\027[0m\027[?2026l\027[?25h"
let dead = Atomic.make false
let written = Atomic.make false
let is_dead () = Atomic.get dead

(* [give_up hs] sets [dead], so the display writes nothing more and no frame
   follows the sequence. A write already in the kernel cannot be stopped. It
   then writes the sequence on the terminal of each of [hs], only once until
   {!reopen}. The terminal is non-blocking, so bytes it does not take at once
   are dropped. *)
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

(* [stop signo] stops the process as the default disposition would. The stop
   runs inside the [suspend] of every holder. The [suspend] of the innermost
   holder is the outermost call. [kill] returns when the process continues.
   The runtime blocks a signal while its handler runs, so [stop] unblocks it
   for the stop. In a process group that no shell controls, Linux and macOS
   discard the signal, and FreeBSD stops the process anyway. If the
   suspensions do not start within [grace], or a holder belongs to another
   domain, the terminal is given up ({!give_up}) for the stop instead. Each
   holder's [resume] then runs when the process continues. *)
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

external raise_signal : int -> unit = "console_driver_raise"

(* [resend signo] sends [signo] to this process again. The runtime blocks
   [signo] while its handler runs. Other threads may block it too: the
   display's thread does when another domain handles it. So the sending thread
   unblocks it first. POSIX then delivers it to that thread before [kill]
   returns, unless another thread takes it. Windows has no [kill] for signals
   and no signal mask, so there the C [raise] sends it. Its default action
   exits with code 3. *)
let resend signo =
  if Sys.win32 then raise_signal signo
  else begin
    let mask = Unix.sigprocmask Unix.SIG_UNBLOCK [ signo ] in
    Unix.kill (Unix.getpid ()) signo;
    ignore (Unix.sigprocmask Unix.SIG_SETMASK mask : int list)
  end

(* [disposition s] is the address of the handler that the kernel holds for
   [s], or [0n] for the default disposition. [Sys.signal] reports a handler
   installed outside OCaml, such as one from [Lwt_unix.on_signal], as the
   default. [ours] is the address of the handlers that {!catch} installs. *)
external disposition : int -> nativeint = "console_driver_disposition"

let ours = Atomic.make 0n

(* [release ()] puts back the default disposition of the caught signals,
   unless the program has installed its own handler since. *)
let rec release () =
  List.iter
    (fun s ->
      if Nativeint.equal (disposition s) (Atomic.get ours) then
        match Sys.signal s Sys.Signal_default with
        | Sys.Signal_handle h when h == handle || h == stop -> ()
        | theirs -> Sys.set_signal s theirs)
    (Atomic.exchange caught [])

(* [handle signo] sends [signo] again, once: after the last restoration or
   after [grace], whichever comes first. The timer starts before any
   restoration runs. The terminal of a holder in another domain is given up at
   once. An ending signal that arrives before [signo] is sent again is part of
   the same ending. *)

and handle signo =
  if not (Atomic.exchange ending true) then begin
    let sent = Atomic.make false in
    let send () =
      if not (Atomic.exchange sent true) then begin
        release ();
        Atomic.set ending false;
        resend signo
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
      if Nativeint.equal (disposition s) 0n then
        match Sys.signal s (Sys.Signal_handle h) with
        | Sys.Signal_default ->
            Atomic.set ours (disposition s);
            update caught (List.cons s)
        | chosen -> Sys.set_signal s chosen)
    (List.map (fun s -> (s, stop)) stops @ List.map (fun s -> (s, handle)) ends)

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
