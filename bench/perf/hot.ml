(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The cost of one update, from the display's own domain: nanoseconds and
   words per call, and the frames the calls drew. With [--check], fails if
   a call allocates more words than its ratchet, in the contexts where no
   frame is drawn during the calls and the count depends on console's code
   alone; the times are only printed. Usage: hot.exe [n] [--check] *)

open Console

let n =
  let given = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  Option.value ~default:1_000_000 (int_of_string_opt given)

let check = Array.exists (String.equal "--check") Sys.argv

type op = { name : string; prime : Task.t -> unit; call : Task.t -> unit }

let total = 1_000_000_000

let ops =
  [
    {
      name = "advance (items)";
      prime = (fun t -> Task.set_items t ~count:0 ~total);
      call = (fun t -> Task.advance t);
    };
    {
      name = "advance ~by (bytes)";
      prime =
        (fun t -> Task.set_bytes t ~count:0L ~total:1_000_000_000_000_000L);
      call = (fun t -> Task.advance ~by:65536 t);
    };
    {
      name = "set_label";
      prime = ignore;
      call = (fun t -> Task.set_label t "hashing chunk");
    };
    {
      name = "log";
      prime = ignore;
      call = (fun t -> Task.log t "a line of output from the task");
    };
  ]

(* The words a call may allocate: [ratchet] is what the code allocates now,
   on flambda or not, whichever is more, so that [--check] catches a
   regression; [target] is the budget to reach. A context with no ratchet
   draws frames as the machine's clock says, and is only printed. *)
let ratchet ctx op =
  match (ctx, op) with
  | "no display", "advance (items)" -> Some 15.
  | "no display", "advance ~by (bytes)" -> Some 18.
  | "no display", "set_label" -> Some 6.
  | "no display", "log" -> Some 61.
  | "show `Tty, clock still", "advance (items)" -> Some 11.
  | "show `Tty, clock still", "advance ~by (bytes)" -> Some 14.
  | "show `Tty, clock still", "set_label" -> Some 6.
  | "show `Tty, clock still", "log" -> Some 83.
  | "show `Plain", "advance (items)" -> Some 15.
  | "show `Plain", "advance ~by (bytes)" -> Some 18.
  | "show `Plain", "set_label" -> Some 6.
  | "show `Plain", "log" -> Some 964.
  | "eio `Tty, clock still", "advance (items)" -> Some 35.
  | "eio `Tty, clock still", "advance ~by (bytes)" -> Some 38.
  | "eio `Tty, clock still", "set_label" -> Some 18.
  | "eio `Tty, clock still", "log" -> Some 107.
  | "eio `Plain, clock still", "advance (items)" -> Some 23.
  | "eio `Plain, clock still", "advance ~by (bytes)" -> Some 26.
  | "eio `Plain, clock still", "set_label" -> Some 6.
  | "eio `Plain, clock still", "log" -> Some 974.
  | _ -> None

(* Under Console_eio a call reads Eio's clock, and a line printed on
   [`Plain] goes through Format and Fmt: those counts change with the
   releases of eio and fmt, so they are printed beside the count recorded
   but do not fail the check. *)
let gated ctx op =
  not
    (String.starts_with ~prefix:"eio" ctx
    || (String.equal ctx "show `Plain" && String.equal op "log"))

(* The budget of [op] in [ctx]: an update allocates only the clock's
   reading, and a line of output only its own record as well. A line printed
   on [`Plain] has no target. *)
let target ctx op =
  let clock = if String.starts_with ~prefix:"eio" ctx then 4. else 0. in
  let printed =
    String.ends_with ~suffix:"`Plain" ctx
    || String.ends_with ~suffix:"`Plain, clock still" ctx
  in
  match op with
  | "log" when printed -> None
  | "log" -> Some (32. +. clock)
  | _ -> Some clock

let report ctx (sink : Measure.sink option) root =
  List.iter
    (fun op ->
      let t = Task.start root "hashing" in
      op.prime t;
      let f0 = match sink with Some s -> !(s.frames) | None -> 0 in
      let w0 = Measure.wall () in
      let ns, words = Measure.per_call ~n (fun _ -> op.call t) in
      let secs = Measure.wall () -. w0 in
      let frames = match sink with Some s -> !(s.frames) - f0 | None -> 0 in
      Task.succeed t;
      let verdict =
        match ratchet ctx op.name with
        | Some ratchet ->
            Measure.verdict ~gated:(gated ctx op.name) ~ratchet
              ?target:(target ctx op.name) words
        | None -> ""
      in
      Fmt.pr "%-24s %-20s %8.1f ns %7.2f w %6d frames %5.1f/s  %s@." ctx op.name
        ns words frames
        (float frames /. secs)
        verdict)
    ops

let real_clock () =
  let t0 = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. t0

(* A clock that moves by a microsecond at each read: the rate is measured
   at each update, the same on every machine. *)
let ticking () =
  let t = ref 0. in
  fun () ->
    t := !t +. 1e-6;
    !t

let bare () =
  let root = Task.root ~label:"bench" ~now:(ticking ()) () in
  report "no display" None root;
  let show ~ctx ~now progress =
    let s = Measure.sink () in
    let root = Task.root ~label:"bench" ~now () in
    let d = Progress.show ~progress ~size:(fun () -> (100, 24)) s.ppf root in
    report ctx (Some s) root;
    Display.close d
  in
  show ~ctx:"show `Tty, clock still" ~now:(fun () -> 0.) `Tty;
  show ~ctx:"show `Plain" ~now:(ticking ()) `Plain;
  if not check then show ~ctx:"show `Tty, real clock" ~now:(real_clock ()) `Tty

(* The same calls under [Console_eio.run], on the machine's clock or on the
   deterministic clock, which stays at zero while the program runs. *)
let eio env =
  let mono = Eio.Stdenv.mono_clock env in
  let run ~ctx ?(still = false) progress =
    let s = Measure.sink () in
    if still then Unix.putenv "CONSOLE_DETERMINISTIC_CLOCK" "1";
    let clock = Console_eio.clock mono in
    Console_eio.run ~clock ~ppf:s.ppf ~progress (fun root ->
        report ctx (Some s) root);
    if still then Unix.putenv "CONSOLE_DETERMINISTIC_CLOCK" ""
  in
  run ~ctx:"eio `Tty, clock still" ~still:true `Tty;
  run ~ctx:"eio `Plain, clock still" ~still:true `Plain;
  if not check then begin
    run ~ctx:"eio `Tty" `Tty;
    run ~ctx:"eio `Plain" `Plain
  end

let () =
  Unix.putenv "COLUMNS" "100";
  Unix.putenv "LINES" "24";
  Fmt.pr "%-24s %-20s %11s %9s %13s@." "context" "call" "time" "alloc"
    "frames drawn";
  bare ();
  Eio_main.run eio;
  if check then Measure.exit_on_failures ()
