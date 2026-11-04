(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Display = Console.Display
module Task = Console.Task

type run = { display : Display.t; root : Task.t; json : bool }

(* A terminal has one live region at a time. The display that holds it is a
   fiber-local value, so a [run] in a fiber under another one finds it and
   adds to it, and a global one, so a [run] in another fiber of its domain
   joins it, a [run] in another domain is refused, and an exit in the middle
   of the work still closes it. *)

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

(* A fiber that outlives its run keeps the binding of a display that
   closed: that one is no longer found. *)
let find () =
  match (Eio.Fiber.get current, Atomic.get shared) with
  | Some r, Some s when r == s.run && not s.closing -> Some r.root
  | _ -> None

let display () =
  match (Eio.Fiber.get current, Atomic.get shared) with
  | Some r, Some s when r == s.run && not s.closing -> Some r.display
  | _ -> None
  | exception Effect.Unhandled _ -> None

(* The task a run inside another is given: the one current where it is
   called, or the root. *)
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

(* [exit] runs the [at_exit] functions but no [finally]: the run's ending
   is run there. It is registered by the first run, so that the [at_exit]
   functions registered before it run once the display is closed; an atomic
   flag, not a lazy value, since two domains may start their first run at
   once. *)
let registered = Atomic.make false

let at_exit () =
  if not (Atomic.exchange registered true) then
    Stdlib.at_exit (fun () -> Option.iter (fun f -> f ()) (Atomic.get ending))

let rec cancelled = function
  | Eio.Cancel.Cancelled _ -> true
  | Fun.Finally_raised e -> cancelled e
  | _ -> false

(* A task is current in the fibers its function forks, as a binding is. *)
let scope =
  Task_driver.scope
    ~current:(fun () -> Eio.Fiber.get task)
    ~within:(fun t f -> Eio.Fiber.with_binding task t f)
    ~cancelled

let write_error = function Eio.Io _ -> true | _ -> false

let in_fiber () =
  match Eio.Fiber.is_cancelled () with
  | _ -> true
  | exception Effect.Unhandled _ -> false

(* A [Cancelled] where nothing cancelled the fiber, or out of any fiber, is no
   cancellation of it. *)
let cancelling e = cancelled e && in_fiber () && Eio.Fiber.is_cancelled ()

(* How long a write that a cancellation waits for gives its output, when the
   run is cancelled or a cancellation cut a write: the output may be why. A
   write that nothing cancels waits as any write does: a reader that pauses,
   such as a pager, loses nothing. *)
let grace = 1.

(* When the writes that a cancellation waits for give up on the output: set by
   the first of them, and cleared by the first that goes out, so that an output
   that takes nothing holds up all of them, nested or not, for [grace] in all. *)
let giving_up = Atomic.make None

let deadline () =
  match Atomic.get giving_up with
  | Some d -> d
  | None ->
      let d = Unix.gettimeofday () +. grace in
      Atomic.set giving_up (Some d);
      d

(* The program's clock may be one that does not move. *)
let sleep_until deadline =
  match Eio_unix.sleep (Float.max 0. (deadline -. Unix.gettimeofday ())) with
  | () -> ()
  | exception Effect.Unhandled _ -> Eio.Fiber.await_cancel ()

(* [f ()] where no cancellation stops it, as an Eio flow requires to write,
   until the {!deadline} that [armed] (defaults to returning) leads to at most:
   then [f] is cancelled. It is [true] iff [f] finished before. *)
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

(* An update written in a cancelled fiber goes out whole, as one that nothing
   cancelled does, but waits for its output until the {!deadline} at most: cut
   short, the cancellation goes on. Cut by the cancellation half way, it would
   leave rows of the region on the screen. *)
let atomic f =
  if not (in_fiber () && Eio.Fiber.is_cancelled ()) then f ()
  else if not (shielded f) then Eio.Fiber.check ()

(* [f ()], which a cancellation that comes once it started does not cut: from
   then on it waits for the output until the {!deadline} at most, and cut short
   there, the cancellation goes on. *)
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

(* Nothing a broken output raises stops the formatters being given back. *)
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
   it takes is output, a whole line at a time. Bytes belong to the task
   current in the fiber that writes them: each task collects its own lines,
   without their escape codes, and its line under way is ended when it ends.
   Bytes written outside any task go above the region as they are, a whole
   line at a time that waits for a flush, and the line under way there goes
   out when the display closes; as events, they are the output of no task.
   A line under way waits for its end up to {!max_pending} bytes, and is cut
   there. Inside {!with_terminal}, the bytes of the fibers it runs go
   straight out. *)

(* A line under way, and whether the one before it was cut short rather
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
  root : line; (* the line under way outside any task *)
}

let intercepted : intercepted list Atomic.t = Atomic.make []
let direct : unit Eio.Fiber.key = Eio.Fiber.create_key ()

(* Out of a fiber, as from an [at_exit] function, no bytes are direct. *)
let is_direct () =
  try Option.is_some (Eio.Fiber.get direct) with Effect.Unhandled _ -> false

(* The line under way of each task that wrote one, by the task's id. *)
let lines : (int, line) Hashtbl.t = Hashtbl.create 16

(* What a line under way holds at most, as a channel's buffer: more is
   written out, cut between two characters, outside an escape code if one
   does not take it all. *)
let max_pending = 65536

(* The first [k all] bytes of [b], which holds [all], taken out of it. *)
let take_from b k =
  let all = Buffer.contents b in
  let k = k all in
  Buffer.clear b;
  Buffer.add_substring b all k (String.length all - k);
  String.sub all 0 k

(* [s] added to [l]: [emit ~cut piece] for the whole lines it ends, or for
   the piece cut off a line grown past {!max_pending}. *)
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

(* [s] as [task]'s output. The line feed that ends a line cut short ends
   no line of its own. *)
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

(* [s], written outside any task: held for above the region, or as events
   the output of no task. *)
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

(* Before the program's first domain starts, Format gives [Fmt.stdout] and
   [Fmt.stderr] the [out_string] and [out_flush] of a buffer per domain, and
   keeps the rest of the functions they have. This function, registered
   after Format's, runs after it: a formatter taken is taken again over
   those two, which {!restore} gives back. A domain started before this
   module was loaded leaves nothing to register. *)
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

(* What a task wrote and its formatters still hold is its own, and its line
   under way is ended. Out of a fiber, as from an [at_exit] function, no task
   can be made current, and what they hold goes as written then. *)
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

(* As events, what the formatters still hold outside any task is output
   before the display closes, so that every line it wrote is an event. *)
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

(* [first ()] then what the program wrote, whatever either raises, and the
   display closes. *)
let close_display ?(first = ignore) run =
  Task_driver.protect
    (fun () ->
      first ();
      settle run)
    ~finally:(fun () -> Display.close run.display)

(* Closing while the program ends for another reason: that reason goes on, and
   what a view or a hook raised on the way out gives way to it, unless the
   program must see it and the reason is not [urgent] already. *)
let close_quietly ?first ?(urgent = false) run =
  try close_display ?first run
  with e when urgent || not (Task_driver.is_async e) -> ()

(* From a signal handler or an [at_exit] function, nothing goes on after it. *)
let close_last run =
  try close_quietly run with Out_of_memory | Stack_overflow | Sys.Break -> ()

(* Without [flush], as from another thread, the formatters only get their
   functions back: what they hold is the display's, whose state is not
   read there. *)
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

(* A message is answered once the display wrote it, or once the display
   closed without writing it. It keeps the task current where it was
   logged, which the fiber that writes it does not have. *)
type request = {
  within : Task.t option;
  level : Task_driver.level;
  message : string;
  reply : ([ `Written | `Closed ], exn) result Eio.Promise.t Eio.Promise.u;
}

(* A message for the display, from the domain that runs it or handed to
   that domain: [`Closed] if the display closes first. A domain that Eio
   does not run cannot wait for the answer, so its message is not taken. *)
let deliver ~owner ~closed ~requests root level message =
  if Option.is_some (Eio.Promise.peek closed) then `Closed
  else if Int.equal (Domain.self () :> int) (owner : Domain.id :> int) then begin
    Task_driver.report root level message;
    `Written
  end
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

(* A message the display cannot take, closed or refusing its output, goes
   to the reporter it replaced, so that a log call never fails. A diagnostic
   from a source of its own is prefixed with the source's name, which tells
   apart the libraries whose messages interleave; an App message is the
   program's own output and has no header, as with Logs_fmt.reporter. *)
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

(* The display redraws on its clock, so that the spinners turn and the
   timers run while no task reports, and draws afresh after [repaint] is
   set. The end of the run, which stops the redraws, cuts none half way. *)
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

(* Stopped, the program gives the terminal back as it is without a region,
   the cursor shown; continued, it hides the cursor and draws the region
   again. *)
let suspend ~cursor display stop =
  if Display_driver.closed display then stop ()
  else
    Display.with_output display (fun _ ->
        cursor true;
        Fun.protect stop ~finally:(fun () -> cursor false))

(* [f] for the run that opened [s]'s display. A function that raised waits
   for the others as one that returned does; a cancelled one cannot wait. *)
let first_run s f =
  match Eio.Fiber.with_binding current s.run (fun () -> f s.run.root) with
  | v ->
      last_out s;
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try last_out s with Eio.Cancel.Cancelled _ -> ());
      Printexc.raise_with_backtrace e bt

(* [display] as the domain's, for the runs that join it. *)
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

(* The work ends as a task's does: it succeeds when [f] returns, fails when
   it raises, and is cancelled when its fiber is, unless [f] ended it. The
   exception goes on to the caller, which says it: the work ends with no
   message. *)
let end_work root = function
  | Ok _ -> Task.succeed root
  | Error e -> if cancelled e then Task.cancel root else Task.fail root

(* The end of a cancelled run, or of one whose output a cancellation cut,
   waits for the output until the {!deadline} at most. *)
let last_writes display f =
  if Eio.Fiber.is_cancelled () || Display_driver.unwritten display then
    ignore (shielded f : bool)
  else f ()

(* A signal handler that runs out of any fiber cannot write on an Eio flow: a
   fiber of the run does what it asks, as long as there is one. *)
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

(* What a return, a raise or a cancellation does, [exit] does too, so that what
   a later [at_exit] function logs or prints goes out as before. The cursor
   comes first: the last frame runs a view, which may call [exit] again, and
   that one ends the program at once. From another thread or domain, or while a
   write is under way, the display's state is not touched: the terminal is
   given up. *)
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
