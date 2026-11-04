(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Display = Console.Display
module Task = Console.Task

type run = { display : Display.t; root : Task.t; json : bool }

(* A terminal has one live region at a time. The display that holds it is
   bound in the fibers of its run ([current]), so that a [run] in a fiber
   under another finds it and adds to it. It is also global ([shared]), so
   that a [run] in another fiber of its domain joins it, a [run] in another
   domain is refused, and an exit in the middle of the work still closes
   it. *)

let current : run Eio.Fiber.key = Eio.Fiber.create_key ()
let task : Task.t Eio.Fiber.key = Eio.Fiber.create_key ()
let busy = Atomic.make false
let ending : (unit -> unit) option Atomic.t = Atomic.make None

(* The runs that share the display of a domain: the one that opened it and
   those that joined it. It closes once the last of them ends. *)
type shared = {
  run : run;
  owner : Domain.id;
  mutable users : int;
  mutable closing : bool;
  mutable idle : unit Eio.Promise.u option;
  released : unit Eio.Promise.t * unit Eio.Promise.u;
}

let shared : shared option Atomic.t = Atomic.make None

(* A fiber that outlives its run keeps the binding of a display that has
   closed, so the binding counts only while it is the shared display and that
   display is not closing. *)
let find () =
  match (Eio.Fiber.get current, Atomic.get shared) with
  | Some r, Some s when r == s.run && not s.closing -> Some r.root
  | _ -> None

let display () =
  match (Eio.Fiber.get current, Atomic.get shared) with
  | Some r, Some s when r == s.run && not s.closing -> Some r.display
  | _ -> None
  | exception Effect.Unhandled _ -> None

let given root = Option.value (Task_driver.current root) ~default:root

let join f =
  match Atomic.get shared with
  | Some s
    when Int.equal (Domain.self () :> int) (s.owner :> int) && not s.closing ->
      s.users <- s.users + 1;
      let leave () =
        s.users <- s.users - 1;
        if s.users = 0 then
          Option.iter (fun u -> ignore (Eio.Promise.try_resolve u ())) s.idle
      in
      Some
        (Fun.protect ~finally:leave (fun () ->
             Eio.Fiber.with_binding current s.run (fun () ->
                 f (given s.run.root))))
  | _ -> None

(* A run that starts while the display of its domain closes opens its own
   once that one is gone. *)
let await_release () =
  match Atomic.get shared with
  | Some s when Int.equal (Domain.self () :> int) (s.owner :> int) && s.closing
    ->
      Eio.Promise.await (fst s.released)
  | _ -> ()

(* The run that opened the display waits for those that joined it. *)
let last_out s =
  s.users <- s.users - 1;
  Fun.protect ~finally:(fun () -> s.closing <- true) @@ fun () ->
  if s.users > 0 then begin
    let idle, resolver = Eio.Promise.create () in
    s.idle <- Some resolver;
    Eio.Promise.await idle
  end

(* [exit] runs the [at_exit] functions but no [finally], so the end of a run
   ([ending]) is registered there. The first run registers it, so that the
   [at_exit] functions registered before it run after the display is closed.
   [registered] is an atomic flag rather than a lazy value, since two domains
   may start their first run at once. *)
let registered = Atomic.make false

let at_exit () =
  if not (Atomic.exchange registered true) then
    Stdlib.at_exit (fun () -> Option.iter (fun f -> f ()) (Atomic.get ending))

let rec cancelled = function
  | Eio.Cancel.Cancelled _ -> true
  | Fun.Finally_raised e -> cancelled e
  | _ -> false

(* The current task is a fiber binding, so it stays current in the fibers
   that its function forks. Outside any fiber, as in a thread or a signal
   handler, no task is current. *)
let scope =
  Task_driver.scope
    ~current:(fun () ->
      try Eio.Fiber.get task with Effect.Unhandled _ -> None)
    ~within:(fun t f -> Eio.Fiber.with_binding task t f)
    ~cancelled

let write_error = function Eio.Io _ -> true | _ -> false

let in_fiber () =
  match Eio.Fiber.is_cancelled () with
  | _ -> true
  | exception Effect.Unhandled _ -> false

(* A [Cancelled] raised in a fiber that is not cancelled, or outside any
   fiber, is not a cancellation of the caller. *)
let cancelling e = cancelled e && in_fiber () && Eio.Fiber.is_cancelled ()

(* How long, in seconds, a write waits for its output in a cancelled run or
   after a cancellation cut a write: the output may be why the run was
   cancelled. A write that nothing cancels waits as any write does, so that a
   reader that pauses, such as a pager, loses nothing. *)
let grace = 1.

(* The time at which the writes that a cancellation waits for give up on the
   output. The first of them sets it and the first that goes out clears it,
   so that an output that takes nothing holds them all up, nested or not, for
   [grace] in total. *)
let giving_up = Atomic.make None

let deadline () =
  match Atomic.get giving_up with
  | Some d -> d
  | None ->
      let d = Unix.gettimeofday () +. grace in
      Atomic.set giving_up (Some d);
      d

(* [sleep_until] waits on the real time, since the program's clock may be one
   that does not move. *)
let sleep_until deadline =
  match Eio_unix.sleep (Float.max 0. (deadline -. Unix.gettimeofday ())) with
  | () -> ()
  | exception Effect.Unhandled _ -> Eio.Fiber.await_cancel ()

(* [shielded ~armed f] runs [f ()] protected from cancellation, which an Eio
   flow needs to write, until the {!deadline} at most, taken once [armed ()]
   returns (defaults to at once); then [f] is cancelled. It is [true] iff [f]
   finished in time. *)
let shielded ?(armed = ignore) f =
  let finished = ref false and late = ref false in
  Eio.Cancel.protect (fun () ->
      Eio.Fiber.first
        (fun () ->
          f ();
          finished := true)
        (fun () ->
          armed ();
          sleep_until (deadline ());
          late := true));
  let in_time = !finished && not !late in
  if in_time then Atomic.set giving_up None;
  in_time

(* An update written in a cancelled fiber goes out whole, as in a fiber that
   is not cancelled, but waits for its output until the {!deadline} at most;
   if the deadline cuts it short, the cancellation propagates. Cut half way
   by the cancellation, the update would leave rows of the region on the
   screen. *)
let atomic f =
  if not (in_fiber () && Eio.Fiber.is_cancelled ()) then f ()
  else if not (shielded f) then Eio.Fiber.check ()

(* [whole f] is [f ()], which a cancellation that arrives after it started
   does not cut. From the cancellation on, [f] waits for the output until the
   {!deadline} at most; if the deadline cuts it short, the cancellation
   propagates. *)
let whole f =
  let cancelled, cancel = Eio.Promise.create () in
  let finished = ref false in
  Eio.Fiber.first
    (fun () ->
      finished := shielded ~armed:(fun () -> Eio.Promise.await cancelled) f)
    (fun () ->
      Fun.protect Eio.Fiber.await_cancel ~finally:(fun () ->
          Eio.Promise.resolve cancel ()));
  if not !finished then Eio.Fiber.check ()

(* [safely f] runs [f] and drops what it raises, unless that is
   asynchronous, so that a broken output does not stop the formatters being
   given back. *)
let safely f =
  match f () with
  | () -> ()
  | exception e when not (Task_driver.is_async e) -> ()

let claim () =
  at_exit ();
  if not (Atomic.compare_and_set busy false true) then
    invalid_arg "Console_eio.run: a display is already running"

let release () =
  Option.iter
    (fun s -> ignore (Eio.Promise.try_resolve (snd s.released) ()))
    (Atomic.exchange shared None);
  Atomic.set ending None;
  Atomic.set busy false

(* While a display runs, what the program writes on the standard formatters
   that it takes becomes output, a whole line at a time. Bytes belong to the
   task current in the fiber that writes them: each task collects its own
   lines, without their escape codes, and its unended line is ended when the
   task ends. Bytes written outside any task go above the region as they are,
   in whole lines held until a flush, and the unended line there goes out
   when the display closes; with JSON events, they are output that belongs to
   no task. An unended line waits for its line feed up to {!max_pending}
   bytes, and is cut there. Inside {!with_terminal}, the bytes of the fibers
   it runs go straight out. *)

(* An unended line, and whether the line before it was cut short rather
   than ended. *)
type line = { pending : Buffer.t; mutable cut : bool }

let line () = { pending = Buffer.create 256; cut = false }

type intercepted = {
  by : run; (* the run whose display takes [ppf] *)
  ppf : Format.formatter;
  mutable out : Format.formatter_out_functions; (* given back by {!restore} *)
  mutable pass : Format.formatter_out_functions;
      (* [out], watched by the display *)
  held : Buffer.t; (* whole lines for above the region *)
  root : line; (* the unended line outside any task *)
}

let intercepted : intercepted list Atomic.t = Atomic.make []
let direct : unit Eio.Fiber.key = Eio.Fiber.create_key ()

(* Outside any fiber, as in an [at_exit] function, no bytes go straight
   out. *)
let is_direct () =
  try Option.is_some (Eio.Fiber.get direct) with Effect.Unhandled _ -> false

(* The unended line of each task that wrote one, by the task's id. *)
let lines : (int, line) Hashtbl.t = Hashtbl.create 16

(* The most bytes that an unended line holds, the size of a channel's
   buffer. Past it, the line is written out, cut between two characters and
   outside an escape code, unless one escape code takes it all. *)
let max_pending = 65536

(* [take_from b k] removes the first [k all] bytes of [b], where [all] is the
   contents of [b], and returns them. *)
let take_from b k =
  let all = Buffer.contents b in
  let k = k all in
  Buffer.clear b;
  Buffer.add_substring b all k (String.length all - k);
  String.sub all 0 k

(* [take l s o len emit] adds [s] to [l], then calls [emit ~cut:false] with
   the whole lines that this ends, or [emit ~cut:true] with the piece cut off
   a line grown past {!max_pending}. *)
let take l s o len emit =
  Buffer.add_substring l.pending s o len;
  let ends_a_line =
    match String.index_from_opt s o '\n' with
    | Some k -> k < o + len
    | None -> false
  in
  if ends_a_line then
    emit ~cut:false
      (take_from l.pending (fun all -> String.rindex all '\n' + 1))
  else if Buffer.length l.pending > max_pending then
    emit ~cut:true (take_from l.pending Console_text.boundary)

(* [give l ~cut task s] logs [s] as output of [task]. The line feed that
   ends a line cut short does not make an empty line of its own. *)
let give l ~cut task s =
  let s =
    if l.cut && String.starts_with ~prefix:"\n" s then
      String.sub s 1 (String.length s - 1)
    else s
  in
  l.cut <- cut;
  if s <> "" then Task.log task (Console_text.strip s)

let send_held display i =
  if Buffer.length i.held > 0 then begin
    let s = Buffer.contents i.held in
    Buffer.clear i.held;
    Display.with_output display (fun _ ->
        i.pass.out_string s 0 (String.length s);
        i.pass.out_flush ())
  end

(* [outside run i ~cut s] takes [s], written outside any task: it is held
   for above the region, or with JSON events it is logged on the root as
   output that belongs to no task. *)
let outside run i ~cut s =
  if run.json then give i.root ~cut run.root s
  else begin
    i.root.cut <- false;
    Buffer.add_string i.held s;
    if cut || Buffer.length i.held > max_pending then send_held run.display i
  end

let task_line task =
  let id = Task_driver.id task in
  match Hashtbl.find_opt lines id with
  | Some l -> l
  | None ->
      let l = line () in
      Hashtbl.replace lines id l;
      l

let take_over ({ by = run; _ } as i) =
  let add s o len =
    if is_direct () then i.pass.out_string s o len
    else
      match Task_driver.current run.root with
      | Some task ->
          let l = task_line task in
          take l s o len (fun ~cut piece ->
              send_held run.display i;
              give l ~cut task piece)
      | None -> take i.root s o len (outside run i)
  in
  let flush () =
    if is_direct () then i.pass.out_flush () else send_held run.display i
  in
  Format.pp_set_formatter_out_functions i.ppf
    {
      i.out with
      out_string = add;
      out_flush = flush;
      out_newline = (fun () -> add "\n" 0 1);
      out_spaces = (fun n -> add (String.make (max 0 n) ' ') 0 (max 0 n));
      out_indent = (fun n -> add (String.make (max 0 n) ' ') 0 (max 0 n));
    }

let intercept run ppf =
  Format.pp_print_flush ppf ();
  let out = Format.pp_get_formatter_out_functions ppf () in
  let pass = Display_driver.watch run.display out in
  let i =
    { by = run; ppf; out; pass; held = Buffer.create 256; root = line () }
  in
  take_over i;
  let rec remember () =
    let l = Atomic.get intercepted in
    if not (Atomic.compare_and_set intercepted l (i :: l)) then remember ()
  in
  remember ()

(* Before the program's first domain starts, Format replaces the
   [out_string] and [out_flush] of [Fmt.stdout] and [Fmt.stderr] with those
   of a buffer per domain, and keeps their other functions. This function is
   registered after Format's, so it runs after it: a formatter that a run
   took is taken again over the new pair, which {!restore} gives back. If a
   domain started before this module was loaded, there is nothing to
   register. *)
let () =
  let rebase i =
    if i.ppf == Format.std_formatter || i.ppf == Format.err_formatter then begin
      let now = Format.pp_get_formatter_out_functions i.ppf () in
      i.out <-
        { i.out with out_string = now.out_string; out_flush = now.out_flush };
      i.pass <- Display_driver.watch i.by.display i.out;
      take_over i
    end
  in
  try
    Domain.before_first_spawn (fun () ->
        List.iter rebase (Atomic.get intercepted))
  with Invalid_argument _ -> ()

let flush_all () =
  List.iter
    (fun i -> safely (fun () -> Format.pp_print_flush i.ppf ()))
    (Atomic.get intercepted)

(* When a task ends, what the formatters still hold is flushed as its
   output, and its unended line is ended. Outside any fiber, as in an
   [at_exit] function, no task can be made current, so what they hold is
   flushed with no task current. *)
let task_ended task =
  (match Atomic.get intercepted with
  | [] -> ()
  | _ -> (
      try Task.within task flush_all with Effect.Unhandled _ -> flush_all ()));
  let id = Task_driver.id task in
  match Hashtbl.find_opt lines id with
  | None -> ()
  | Some l ->
      Hashtbl.remove lines id;
      let s = Buffer.contents l.pending in
      if s <> "" then give l ~cut:false task s

(* With JSON events, what the formatters still hold outside any task is
   output before the display closes, so that every line the program wrote is
   an event. *)
let settle run =
  if run.json then begin
    flush_all ();
    List.iter
      (fun i ->
        let s = Buffer.contents i.root.pending in
        Buffer.clear i.root.pending;
        if s <> "" then outside run i ~cut:false s)
      (Atomic.get intercepted)
  end

(* [close_display ~first run] runs [first ()] then {!settle}, and closes the
   display whatever either raises. *)
let close_display ?(first = ignore) run =
  Task_driver.protect
    (fun () ->
      first ();
      settle run)
    ~finally:(fun () -> Display.close run.display)

(* [close_quietly ~first ~urgent run] closes the display while the program
   ends for another reason, which propagates. An exception that a view or a
   hook raises while closing is dropped, unless it is asynchronous and the
   reason is not [urgent] already. *)
let close_quietly ?first ?(urgent = false) run =
  try close_display ?first run
  with e when urgent || not (Task_driver.is_async e) -> ()

(* [close_last run] closes the display from a signal handler or an [at_exit]
   function, where nothing runs after it, so it also drops an interrupt or an
   exhausted heap or stack. *)
let close_last run =
  try close_quietly run with Out_of_memory | Stack_overflow | Sys.Break -> ()

(* [restore ()] gives the formatters their functions back and writes out
   what they still hold. Without [flush], as from another thread, they only
   get their functions back: what they hold belongs to the display, whose
   state is not read from there. *)
let restore ?(flush = true) () =
  List.iter
    (fun i ->
      if flush then safely (fun () -> Format.pp_print_flush i.ppf ());
      Format.pp_set_formatter_out_functions i.ppf i.out;
      let rest = Buffer.contents i.held ^ Buffer.contents i.root.pending in
      if flush && rest <> "" then
        safely (fun () ->
            i.out.out_string rest 0 (String.length rest);
            i.out.out_flush ()))
    (Atomic.exchange intercepted []);
  if flush then Hashtbl.reset lines

let with_terminal f =
  match (Eio.Fiber.get current, Atomic.get shared) with
  | Some r, Some s when r == s.run && not s.closing ->
      Display.with_output r.display (fun _ ->
          Eio.Fiber.with_binding direct () f)
  | _ -> f ()

let level : Logs.level -> Task_driver.level = function
  | Logs.App -> `App
  | Logs.Error -> `Error
  | Logs.Warning -> `Warning
  | Logs.Info -> `Info
  | Logs.Debug -> `Debug

(* A message for the display, answered once the display wrote it or closed
   without writing it. [within] is the task current where it was logged,
   since that task is not current in the fiber that writes it. *)
type request = {
  within : Task.t option;
  level : Task_driver.level;
  message : string;
  reply : ([ `Written | `Closed ], exn) result Eio.Promise.t Eio.Promise.u;
}

(* [deliver] gives a message to the display, at once from a fiber of the
   domain that runs it, or through [requests] from another domain. It is
   [`Closed] if the display closes first. A thread of that domain, a signal
   handler outside any fiber or a domain that Eio does not run cannot wait
   for the answer, so the display does not take its message. *)
let deliver ~owner ~closed ~requests root level message =
  if Option.is_some (Eio.Promise.peek closed) then `Closed
  else if Int.equal (Domain.self () :> int) (owner : Domain.id :> int) then
    if in_fiber () then begin
      Task_driver.report root level message;
      `Written
    end
    else `Closed
  else
    try
      Eio.Fiber.first
        (fun () ->
          let answer, reply = Eio.Promise.create () in
          let within = Eio.Fiber.get task in
          Eio.Stream.add requests { within; level; message; reply };
          Eio.Promise.await_exn (Eio.Promise.await answer))
        (fun () ->
          Eio.Promise.await closed;
          `Closed)
    with Effect.Unhandled _ -> `Closed

(* A message that the display cannot take, because it is closed or its
   output fails, goes to the reporter it replaced, so that a log call never
   fails. A message from a source other than [Logs.default] is prefixed with
   the source's name, which tells apart the libraries whose messages
   interleave. An [App] message is the program's own output and has no
   header, as with [Logs_fmt.reporter]. *)
let reporter ~previous ~owner ~closed ~requests root =
  let report src l ~over k msgf =
    if Option.is_some (Eio.Promise.peek closed) then
      previous.Logs.report src l ~over k msgf
    else
      msgf (fun ?header ?tags fmt ->
          Fmt.kstr
            (fun message ->
              let shown =
                if src == Logs.default || l = Logs.App || message = "" then
                  message
                else "[" ^ Logs.Src.name src ^ "] " ^ message
              in
              let delivered =
                if shown = "" then `Written
                else
                  match
                    deliver ~owner ~closed ~requests root (level l) shown
                  with
                  | d -> d
                  | exception
                      ( Failure _ | Invalid_argument _ | End_of_file
                      | Sys_error _ | Eio.Io _ ) ->
                      `Closed
              in
              match delivered with
              | `Written ->
                  over ();
                  k ()
              | `Closed ->
                  previous.Logs.report src l ~over k (fun m ->
                      m ?header ?tags "%s" message))
            fmt)
  in
  { Logs.report }

let receive ~sw ~closed requests root =
  let rec loop () =
    let { within; level; message; reply } = Eio.Stream.take requests in
    let report () = Task_driver.report root level message in
    let output =
      Eio.Fiber.fork_promise ~sw (fun () ->
          Eio.Fiber.first
            (fun () ->
              if Option.is_some (Eio.Promise.peek closed) then `Closed
              else begin
                (match within with
                | Some task -> Task.within task report
                | None -> report ());
                `Written
              end)
            (fun () ->
              Eio.Promise.await closed;
              `Closed))
    in
    Eio.Promise.resolve reply output;
    ignore (Eio.Promise.await output : ([ `Written | `Closed ], exn) result);
    loop ()
  in
  loop ()

(* The display redraws every tenth of a second on its clock, so that the
   spinners turn and the timers run while no task reports, and it draws
   afresh once [repaint] is set. The end of the run stops the redraws, but
   never in the middle of one ({!whole}). *)
let animate ~sw ~sleep ~repaint display =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        sleep 0.1;
        if Atomic.get repaint && not (Display_driver.writing display) then begin
          Atomic.set repaint false;
          Display_driver.restart display
        end;
        whole (fun () -> Display.redraw display);
        loop ()
      in
      loop ())

(* While the program is stopped, the terminal is as it would be without a
   region, with the cursor shown. Once the program continues, the cursor is
   hidden and the region drawn again. *)
let suspend ~cursor display stop =
  if Display_driver.closed display then stop ()
  else
    Display.with_output display (fun _ ->
        cursor true;
        Fun.protect stop ~finally:(fun () -> cursor false))

(* [first_run s f] runs [f] for the run that opened the display of [s]. When
   [f] raises, it still waits for the runs that joined, as when it returns,
   unless it is cancelled and cannot wait. *)
let first_run s f =
  match Eio.Fiber.with_binding current s.run (fun () -> f s.run.root) with
  | v ->
      last_out s;
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try last_out s with Eio.Cancel.Cancelled _ -> ());
      Printexc.raise_with_backtrace e bt

(* [share run] makes the display of [run] the domain's, for the runs that
   join it. *)
let share run =
  let s =
    {
      run;
      owner = Domain.self ();
      users = 1;
      closing = false;
      idle = None;
      released = Eio.Promise.create ();
    }
  in
  Atomic.set shared (Some s);
  s

(* The work ends as a task does: it succeeds when [f] returns, fails when
   [f] raises, and is cancelled when its fiber is, unless [f] ended it. The
   exception propagates to the caller, which reports it, so the work ends
   with no message. *)
let end_work root = function
  | Ok _ -> Task.succeed root
  | Error e -> if cancelled e then Task.cancel root else Task.fail root

(* The end of a cancelled run, or of one whose output a cancellation cut,
   waits for the output until the {!deadline} at most. *)
let last_writes display f =
  if Eio.Fiber.is_cancelled () || Display_driver.unwritten display then
    ignore (shielded f : bool)
  else f ()

(* A signal handler that runs outside any fiber cannot write on an Eio flow,
   so a fiber of the run does what it asks, while that fiber runs. *)
type relay = {
  pending : (unit -> unit) list Atomic.t;
  wake : Eio.Condition.t;
  listening : bool Atomic.t;
}

let relay () =
  {
    pending = Atomic.make [];
    wake = Eio.Condition.create ();
    listening = Atomic.make false;
  }

let start_relay ~sw relay =
  let next () =
    match Atomic.exchange relay.pending [] with [] -> None | l -> Some l
  in
  let rec loop () =
    List.iter safely (List.rev (Eio.Condition.loop_no_mutex relay.wake next));
    loop ()
  in
  Eio.Fiber.fork_daemon ~sw loop;
  Atomic.set relay.listening true;
  Eio.Switch.on_release sw (fun () -> Atomic.set relay.listening false)

let rec relayed relay k =
  let l = Atomic.get relay.pending in
  if Atomic.compare_and_set relay.pending l (k :: l) then
    Eio.Condition.broadcast relay.wake
  else relayed relay k

let idle relay ~flow display k =
  let k () = Display_driver.when_idle display k in
  if flow && Atomic.get relay.listening && not (in_fiber ()) then
    relayed relay k
  else k ()

(* [exit] ends the run as a return, a raise or a cancellation does, so that
   what a later [at_exit] function logs or prints goes out as before. The
   cursor is shown first: the last frame runs a view, which may call [exit]
   again, and that call ends the program at once. From another thread or
   domain, or while a write is under way, the display's state is not
   touched, and the terminal is given up instead. *)
let ends_at_exit (run : run) ~thread ~give_back =
  let owner = Domain.self () in
  Atomic.set ending
    (Some
       (fun () ->
         if
           Int.equal (Domain.self () :> int) (owner :> int)
           && Int.equal (Thread.id (Thread.self ())) thread
           && not (Display_driver.writing run.display)
         then begin
           safely (fun () -> Display_driver.reveal run.display);
           close_last run;
           give_back ();
           restore ()
         end
         else begin
           Interrupt.last_resort ();
           give_back ();
           restore ~flush:false ()
         end))

let run ?(cursor = ignore) ?(flow = false) ?terminal ~sleep
    ~intercept:formatters run f =
  let display = run.display in
  let repaint = Atomic.make false in
  let previous = Logs.reporter () in
  let closed, close = Eio.Promise.create () in
  let give_back () =
    Logs.set_reporter previous;
    ignore (Eio.Promise.try_resolve close ())
  in
  ends_at_exit run ~thread:(Thread.id (Thread.self ())) ~give_back;
  let s = share run in
  Task_driver.protect ~finally:(fun () ->
      Task_driver.protect
        (fun () -> last_writes display restore)
        ~finally:(fun () ->
          Atomic.set giving_up None;
          release ()))
  @@ fun () ->
  List.iter (intercept run) formatters;
  Task_driver.ending run.root task_ended;
  let relay = relay () in
  Interrupt.protect ~idle:(idle relay ~flow display)
    ~suspend:(suspend ~cursor display)
    ~resume:(fun () -> Atomic.set repaint true)
    ?terminal
    ~restore:(fun () -> close_last run)
  @@ fun () ->
  match
    Eio.Switch.run @@ fun sw ->
    let requests = Eio.Stream.create 1 in
    Eio.Fiber.fork_daemon ~sw (fun () -> receive ~sw ~closed requests run.root);
    Logs.set_reporter
      (reporter ~previous ~owner:(Domain.self ()) ~closed ~requests run.root);
    animate ~sw ~sleep ~repaint display;
    start_relay ~sw relay;
    Fun.protect ~finally:give_back @@ fun () -> first_run s f
  with
  | value ->
      last_writes display (fun () ->
          close_display run ~first:(fun () -> end_work run.root (Ok ())));
      value
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      last_writes display (fun () ->
          close_quietly run ~urgent:(Task_driver.is_async e) ~first:(fun () ->
              end_work run.root (Error e)));
      Printexc.raise_with_backtrace e bt
