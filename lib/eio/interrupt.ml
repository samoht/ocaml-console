(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A signal the program left at its default disposition ends the process
   without running a finaliser. While something holds the terminal, such a
   signal is caught: what holds it gives it back, the default disposition is
   put back and the signal sent again, so the process still ends as the
   signal says. A disposition the program chose is never replaced. *)

let signals = [ Sys.sigint; Sys.sigterm ]
let restores : (int * (unit -> unit)) list Atomic.t = Atomic.make []
let caught : int list Atomic.t = Atomic.make []
let next = Atomic.make 0

let rec update a f =
  let old = Atomic.get a in
  if not (Atomic.compare_and_set a old (f old)) then update a f

(* Our handler gives way to the default disposition, and one the program
   installed since stays. *)
let rec release () =
  List.iter
    (fun s ->
      match Sys.signal s Sys.Signal_default with
      | Sys.Signal_handle h when h == handle -> ()
      | theirs -> Sys.set_signal s theirs)
    (Atomic.exchange caught [])

and handle signo =
  List.iter
    (fun (_, f) -> try f () with _ -> ())
    (Atomic.exchange restores []);
  release ();
  Unix.kill (Unix.getpid ()) signo

let catch () =
  List.iter
    (fun s ->
      match Sys.signal s (Sys.Signal_handle handle) with
      | Sys.Signal_default -> update caught (List.cons s)
      | chosen -> Sys.set_signal s chosen)
    signals

let protect ~restore f =
  let id = Atomic.fetch_and_add next 1 in
  if Atomic.get restores = [] then catch ();
  update restores (List.cons (id, restore));
  Fun.protect f ~finally:(fun () ->
      update restores (List.filter (fun (i, _) -> i <> id));
      if Atomic.get restores = [] then release ())
