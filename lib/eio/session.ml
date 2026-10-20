(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Display = Console.Display

(* {1 The one display}

   A terminal has one live region at a time. The display that holds it is a
   fiber-local value, so a [run] in a fiber under another one finds it and
   adds to it, and a global one, so a [run] elsewhere is refused and an exit
   in the middle of the work still closes it. *)

let current : Display.t Eio.Fiber.key = Eio.Fiber.create_key ()
let task : Display.task Eio.Fiber.key = Eio.Fiber.create_key ()
let busy = Atomic.make false
let running : Display.t option Atomic.t = Atomic.make None
let find () = Eio.Fiber.get current

(* [exit] runs the [at_exit] functions but no [finally]: the display is
   closed there, its tasks cancelled, so the cursor is shown again and the
   terminal left at the start of a line. *)
let at_exit =
  lazy
    (Stdlib.at_exit (fun () ->
         match Atomic.get running with
         | Some d -> (
             try Display.close ~running:`Cancel d with Sys_error _ -> ())
         | None -> ()))

(* A task is current in the fibers its function forks, as a binding is. *)
let scope =
  Display.scope
    ~current:(fun () -> Eio.Fiber.get task)
    ~within:(fun t f -> Eio.Fiber.with_binding task t f)
    ~cancelled:(function Eio.Cancel.Cancelled _ -> true | _ -> false)

let claim () =
  Lazy.force at_exit;
  if not (Atomic.compare_and_set busy false true) then
    invalid_arg "Console_eio.run: a display is already running"

let release () =
  Atomic.set running None;
  Atomic.set busy false

(* {1 The program's output}

   While a display draws a region on a terminal, what the program writes on
   the standard formatters of that terminal is output, a whole line at a
   time; a line without its end waits for it. A line goes to the task
   current in the fiber that writes or flushes it, without its escape codes,
   or above the region as it is when none is. Inside {!with_terminal} the
   bytes go straight out. *)

type intercepted = {
  ppf : Format.formatter;
  out : Format.formatter_out_functions;
  pending : Buffer.t;
}

let intercepted : intercepted list Atomic.t = Atomic.make []
let direct = Atomic.make 0

(* The whole lines [pending] holds, taken out of it. *)
let whole_lines pending =
  let s = Buffer.contents pending in
  match String.rindex_opt s '\n' with
  | None -> None
  | Some k ->
      Buffer.clear pending;
      Buffer.add_substring pending s (k + 1) (String.length s - k - 1);
      Some (String.sub s 0 (k + 1))

let intercept display ppf =
  Format.pp_print_flush ppf ();
  let out = Format.pp_get_formatter_out_functions ppf () in
  let i = { ppf; out; pending = Buffer.create 256 } in
  let to_task task =
    Option.iter
      (fun s -> Console.Display.log task (Console_text.strip s))
      (whole_lines i.pending)
  in
  let add s o l =
    if Atomic.get direct > 0 then out.out_string s o l
    else begin
      Buffer.add_substring i.pending s o l;
      match String.index_from_opt s o '\n' with
      | Some k when k < o + l -> Option.iter to_task (Display.current display)
      | _ -> ()
    end
  in
  let flush () =
    if Atomic.get direct > 0 then out.out_flush ()
    else
      match Display.current display with
      | Some task -> to_task task
      | None ->
          Option.iter
            (fun s ->
              Display.with_output display (fun _ ->
                  out.out_string s 0 (String.length s);
                  out.out_flush ()))
            (whole_lines i.pending)
  in
  Format.pp_set_formatter_out_functions ppf
    {
      out with
      out_string = add;
      out_flush = flush;
      out_newline = (fun () -> add "\n" 0 1);
      out_spaces = (fun n -> add (String.make (max 0 n) ' ') 0 (max 0 n));
      out_indent = (fun n -> add (String.make (max 0 n) ' ') 0 (max 0 n));
    };
  let rec remember () =
    let l = Atomic.get intercepted in
    if not (Atomic.compare_and_set intercepted l (i :: l)) then remember ()
  in
  remember ()

let restore () =
  List.iter
    (fun i ->
      (try Format.pp_print_flush i.ppf () with Sys_error _ -> ());
      Format.pp_set_formatter_out_functions i.ppf i.out;
      let rest = Buffer.contents i.pending in
      if rest <> "" then
        try
          i.out.out_string rest 0 (String.length rest);
          i.out.out_flush ()
        with Sys_error _ -> ())
    (Atomic.exchange intercepted [])

let with_terminal f =
  match find () with
  | None -> f ()
  | Some d ->
      Display.with_output d (fun _ ->
          Atomic.incr direct;
          Fun.protect f ~finally:(fun () -> Atomic.decr direct))

(* {1 Logs} *)

let level : Logs.level -> Display.level = function
  | Logs.App -> `App
  | Logs.Error -> `Error
  | Logs.Warning -> `Warning
  | Logs.Info -> `Info
  | Logs.Debug -> `Debug

type request = {
  level : Display.level;
  message : string;
  reply : (unit, exn) result Eio.Promise.t Eio.Promise.u;
}

let closed_reporter () =
  invalid_arg "Console_eio: the display reporter is closed"

(* A message from another domain is handed to the display's own, which is the
   only one that writes on it. *)
let reporter ~owner ~closed ~requests display =
  let emit level message =
    if Option.is_some (Eio.Promise.peek closed) then closed_reporter ();
    if Int.equal (Domain.self () :> int) (owner : Domain.id :> int) then
      Display.report display level message
    else
      Eio.Fiber.first
        (fun () ->
          let answer, reply = Eio.Promise.create () in
          Eio.Stream.add requests { level; message; reply };
          Eio.Promise.await answer |> Eio.Promise.await_exn)
        (fun () ->
          Eio.Promise.await closed;
          closed_reporter ())
  in
  (* A message from a source of its own is prefixed with the source's name,
     as the reporter of the Logs documentation does. *)
  let report src l ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Fmt.kstr
          (fun message ->
            let message =
              if src == Logs.default || message = "" then message
              else "[" ^ Logs.Src.name src ^ "] " ^ message
            in
            if message <> "" then emit (level l) message;
            over ();
            k ())
          fmt)
  in
  { Logs.report }

let receive ~sw ~closed requests display =
  let rec loop () =
    let { level; message; reply } = Eio.Stream.take requests in
    let output =
      Eio.Fiber.fork_promise ~sw (fun () ->
          Eio.Fiber.first
            (fun () ->
              if Option.is_some (Eio.Promise.peek closed) then
                closed_reporter ();
              Display.report display level message)
            (fun () ->
              Eio.Promise.await closed;
              closed_reporter ()))
    in
    Eio.Promise.resolve reply output;
    ignore (Eio.Promise.await output : (unit, exn) result);
    loop ()
  in
  loop ()

(* {1 Running} *)

(* The display redraws on its clock, so that the spinners turn and the
   timers run while no task reports. *)
let animate ~sw ~sleep display =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        sleep 0.1;
        Display.redraw display;
        loop ()
      in
      loop ())

(* Stopped, the program gives the terminal back as it is without a region,
   the cursor shown; continued, it hides the cursor and draws the region
   again. *)
let suspend ~cursor display stop =
  Display.with_output display (fun _ ->
      cursor true;
      Fun.protect stop ~finally:(fun () -> cursor false))

let run ?(cursor = ignore) ~sleep ~intercept:formatters display f =
  Atomic.set running (Some display);
  Fun.protect ~finally:(fun () ->
      restore ();
      release ())
  @@ fun () ->
  List.iter (intercept display) formatters;
  let previous = Logs.reporter () in
  let closed, close = Eio.Promise.create () in
  let value =
    match
      Eio.Switch.run @@ fun sw ->
      let requests = Eio.Stream.create 1 in
      Eio.Fiber.fork_daemon ~sw (fun () -> receive ~sw ~closed requests display);
      Logs.set_reporter
        (reporter ~owner:(Domain.self ()) ~closed ~requests display);
      animate ~sw ~sleep display;
      Fun.protect ~finally:(fun () ->
          Logs.set_reporter previous;
          Eio.Promise.resolve close ())
      @@ fun () ->
      Interrupt.protect ~suspend:(suspend ~cursor display) ~restore:(fun () ->
          Display.close ~running:`Cancel display)
      @@ fun () -> Eio.Fiber.with_binding current display (fun () -> f display)
    with
    | v -> v
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Display.close ~running:`Cancel display;
        Printexc.raise_with_backtrace e bt
  in
  Display.close display;
  value
