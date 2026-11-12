(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module type S = sig
  val clock : unit -> unit -> float
  val tick : float -> unit
  val sleep : float -> unit

  type 'a key

  val key : unit -> 'a key
  val get : 'a key -> 'a option
  val with_binding : 'a key -> 'a -> (unit -> 'b) -> 'b

  type wake

  val wake : unit -> wake
  val notify : wake -> unit
  val wait : wake -> (unit -> 'a option) -> 'a
  val in_fiber : unit -> bool
  val background : (unit -> unit) list -> (unit -> 'a) -> 'a
  val first : (unit -> 'a) -> (unit -> 'a) -> 'a
  val protect : (unit -> 'a) -> 'a
  val check : unit -> unit
  val cancelled : exn -> bool
  val write_error : exn -> bool
end

module type T = sig
  val run :
    ?ppf:Format.formatter ->
    ?progress:Console.Progress.t ->
    ?layout:Console.Progress.Layout.t ->
    ?label:string ->
    ?view:(Console.Task.t -> Console.View.t) ->
    ?keep_ended:bool ->
    ?commit:(Console.Task.t -> Console.View.t option) ->
    ?plain:(Console.Task.Event.t -> Console.View.t option) ->
    ?delay:float ->
    (Console.Task.t -> 'a) ->
    'a

  val with_output : (Format.formatter -> 'a) -> 'a
  val print : Console.View.t -> unit
  val redraw : unit -> unit
  val with_terminal : (unit -> 'a) -> 'a
end

module Display = Console.Display
module Task = Console.Task

(* A program has one terminal, but it may apply [Make] once for each backend it
   links. [holder] is the application whose run holds the terminal. *)
let applications = Atomic.make 0
let holder : int option Atomic.t = Atomic.make None

module Make (B : S) = struct
  let application = Atomic.fetch_and_add applications 1

  type run = Capture.run = { display : Display.t; root : Task.t; json : bool }

  (* A terminal has one live region at a time. The display that holds it is
     bound in the fibers of its run ([current]). So a [run] in a fiber under
     another run finds that display and adds to it. The display is also global
     ([shared]). So a [run] in another fiber or domain joins it, and an exit in
     the middle of the work still closes it. *)

  (* The driver calls [B.notify] from anywhere, but the rest of [B] only on the
     home thread of a domain. The home thread is the first thread that starts a
     run or makes a task current in that domain. *)
  let home : int option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)
  let self () = Thread.id (Thread.self ())

  let enter () =
    match Domain.DLS.get home with
    | Some t -> Int.equal t (self ())
    | None ->
        Domain.DLS.set home (Some (self ()));
        true

  let at_home () =
    match Domain.DLS.get home with
    | Some t -> Int.equal t (self ())
    | None -> false

  let get k = if at_home () then B.get k else None
  let in_fiber () = at_home () && B.in_fiber ()

  (* [B] with each call that must run on the home thread checked, so that a
     call from elsewhere raises instead of running on the wrong scheduler.
     Creating a key or a wake, notifying, and the two exception tests are
     allowed anywhere. *)
  module B = struct
    include B

    let home name =
      if not (at_home ()) then
        invalid_arg ("Console_driver: B." ^ name ^ " called off the home thread")

    let clock () =
      home "clock";
      B.clock ()

    let tick d =
      home "tick";
      B.tick d

    let sleep d =
      home "sleep";
      B.sleep d

    let with_binding k v f =
      home "with_binding";
      B.with_binding k v f

    let wait w f =
      home "wait";
      B.wait w f

    let background fs f =
      home "background";
      B.background fs f

    let first a b =
      home "first";
      B.first a b

    let protect f =
      home "protect";
      B.protect f

    let check () =
      home "check";
      B.check ()
  end

  let current : run B.key = B.key ()
  let task : Task.t B.key = B.key ()
  let ending : (unit -> unit) option Atomic.t = Atomic.make None

  (* A one-time event that fibers wait for. *)
  type latch = { reached : bool Atomic.t; wake : B.wake }

  let latch () = { reached = Atomic.make false; wake = B.wake () }

  let reach l =
    Atomic.set l.reached true;
    B.notify l.wake

  let await l =
    B.wait l.wake (fun () -> if Atomic.get l.reached then Some () else None)

  (* The runs that share the display of a domain: the run that opened it and
     the runs that joined it. The display closes when the last of them ends. *)
  type shared = {
    run : run;
    owner : Domain.id;
    users : int Atomic.t;
    closing : bool Atomic.t;
    idle : latch;
    released : latch;
  }

  let shared : shared option Atomic.t = Atomic.make None

  (* A fiber that outlives its run keeps the binding of a closed display. So the
     binding counts only while it is the shared display and is not closing. *)
  let bound () =
    match (get current, Atomic.get shared) with
    | Some r, Some s when r == s.run && not (Atomic.get s.closing) -> Some r
    | _ -> None

  let elsewhere s = not (Int.equal (Domain.self () :> int) (s.owner :> int))

  (* A fiber in another domain has no binding. It uses the shared run. *)
  let display_of_run () =
    match (bound (), get current, Atomic.get shared) with
    | Some r, _, _ -> Some r.display
    | None, None, Some s when elsewhere s && not (Atomic.get s.closing) ->
        Some s.run.display
    | _ -> None

  let given root = Option.value (Task_driver.current root) ~default:root

  let join f =
    match Atomic.get shared with
    | Some s when not (Atomic.get s.closing) ->
        Atomic.incr s.users;
        let leave () =
          if Atomic.fetch_and_add s.users (-1) = 1 then reach s.idle
        in
        Some
          (Fun.protect ~finally:leave (fun () ->
               B.with_binding current s.run (fun () -> f (given s.run.root))))
    | _ -> None

  (* A run that starts while its domain's display is closing waits until that
     display is gone, then opens its own. *)
  let await_release () =
    match Atomic.get shared with
    | Some s when Atomic.get s.closing -> await s.released
    | _ -> ()

  (* The run that opened the display waits for those that joined it. *)
  let last_out s =
    Fun.protect ~finally:(fun () -> Atomic.set s.closing true) @@ fun () ->
    if Atomic.fetch_and_add s.users (-1) > 1 then await s.idle

  (* [exit] runs the [at_exit] functions but not the [finally] functions. So the
     end of a run ([ending]) is registered with [at_exit]. The first run
     registers it, so that [at_exit] functions registered earlier run after the
     display is closed. [registered] is an atomic flag, not a lazy value,
     because two domains may start their first run at the same time. *)
  let registered = Atomic.make false

  let at_exit () =
    if not (Atomic.exchange registered true) then
      Stdlib.at_exit (fun () -> Option.iter (fun f -> f ()) (Atomic.get ending))

  let rec cancelled = function
    | Fun.Finally_raised e -> cancelled e
    | e -> B.cancelled e

  (* The current task is a fiber binding, so it stays current in the fibers
     that its function forks. Outside any fiber, for example in a thread or a
     signal handler, no task is current. *)
  let scope =
    {
      Task_driver.current = (fun () -> get task);
      within = (fun t f -> if enter () then B.with_binding task t f else f ());
      cancelled;
    }

  let is_cancelled () =
    match B.check () with () -> false | exception e when B.cancelled e -> true

  (* Nothing notifies [never], so only a cancellation ends a wait on it. *)
  let never = B.wake ()
  let await_cancel () = B.wait never (fun () -> None)

  (* A [Cancelled] raised in a fiber that is not cancelled, or outside any fiber,
     is not a cancellation of the caller. *)
  let cancelling e = cancelled e && in_fiber () && is_cancelled ()

  (* How long, in seconds, a write waits for its output in a cancelled run, or
     after a cancellation interrupted a write. The output itself may be the
     reason for the cancellation. A write that is not cancelled waits as long as
     needed, so that a reader that pauses, such as a pager, loses nothing. *)
  let grace = 1.

  (* The time at which the writes that wait during a cancellation give up on the
     output. The first such write sets it, and the first write that goes out
     clears it. So an output that takes nothing delays all these writes, nested
     or not, by [grace] in total. *)
  let giving_up = Atomic.make None

  let deadline () =
    match Atomic.get giving_up with
    | Some d -> d
    | None ->
        let d = Unix.gettimeofday () +. grace in
        Atomic.set giving_up (Some d);
        d

  (* [sleep_until] uses real time, because the program's clock may not move. *)
  let sleep_until deadline =
    B.sleep (Float.max 0. (deadline -. Unix.gettimeofday ()))

  (* [shielded ~armed f] runs [f ()] protected from cancellation, because a flow
     may need that to finish a write. The protection lasts at most until the
     {!deadline}, read when [armed ()] returns (by default, at once). Then [f] is
     cancelled. The result is [true] iff [f] finished in time. *)
  let shielded ?(armed = ignore) f =
    let finished = ref false and late = ref false in
    B.protect (fun () ->
        B.first
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

  (* An update written in a cancelled fiber still goes out whole, as in a fiber
     that is not cancelled. But it waits for its output at most until the
     {!deadline}. If the deadline stops it, the cancellation propagates. An
     update stopped half way would leave rows of the region on the screen. *)
  let atomic f =
    if not (in_fiber () && is_cancelled ()) then f ()
    else if not (shielded f) then B.check ()

  (* [whole f] is [f ()]. A cancellation that arrives after [f] started does not
     stop it. From the cancellation on, [f] waits for the output at most until
     the {!deadline}. If the deadline stops it, the cancellation propagates. *)
  let whole f =
    let cut = latch () in
    let finished = ref false in
    B.first
      (fun () -> finished := shielded ~armed:(fun () -> await cut) f)
      (fun () -> Fun.protect await_cancel ~finally:(fun () -> reach cut));
    if not !finished then B.check ()

  (* [claim ()] is [true] iff this run takes the terminal, and [false] iff a
     run of this backend holds it already. A run of another backend raises. *)
  let claim () =
    at_exit ();
    let rec go () =
      match Atomic.get holder with
      | Some a when Int.equal a application -> false
      | Some _ ->
          invalid_arg
            "Console_driver.run: a run of another backend holds the terminal"
      | None -> Atomic.compare_and_set holder None (Some application) || go ()
    in
    go ()

  let release () =
    Option.iter (fun s -> reach s.released) (Atomic.exchange shared None);
    Atomic.set ending None;
    Atomic.set holder None

  let direct : unit B.key = B.key ()

  (* Outside any fiber, for example in an [at_exit] function, no bytes go
     straight out. *)
  let is_direct () = Option.is_some (get direct)
  let capture = Capture.take ~direct:is_direct

  (* When a task ends, the formatters are flushed into its output, and its
     unended line is ended. Outside any fiber, no task can be made current. *)
  let task_ended task =
    if Capture.taking () then
      if in_fiber () then Task.within task Capture.flush_all
      else Capture.flush_all ();
    Capture.end_line task

  (* [close_display ~first run] runs [first ()], then closes the display whatever
     that raises. *)
  let close_display ?(first = ignore) run =
    Task_driver.protect
      (fun () ->
        first ();
        Capture.settle run)
      ~finally:(fun () -> Display.close run.display)

  (* [close_quietly run] closes the display while the program ends for another
     reason, and that reason propagates. It drops any exception from a view or
     a hook, except an asynchronous one when the reason is not already
     [urgent]. *)
  let close_quietly ?first ?(urgent = false) run =
    try close_display ?first run
    with e when urgent || not (Task_driver.is_async e) -> ()

  (* [close_last run] is for a signal handler or an [at_exit] function, where
     nothing runs afterwards. It also drops [Sys.Break], [Out_of_memory] and
     [Stack_overflow]. *)
  let close_last run =
    try close_quietly run
    with Out_of_memory | Stack_overflow | Sys.Break -> ()

  let with_terminal f =
    match bound () with
    | Some r ->
        Display.with_output r.display (fun _ -> B.with_binding direct () f)
    | None -> f ()

  let level : Logs.level -> Task_driver.level = function
    | Logs.App -> `App
    | Logs.Error -> `Error
    | Logs.Warning -> `Warning
    | Logs.Info -> `Info
    | Logs.Debug -> `Debug

  (* The display takes a message from a fiber of its own domain, or from another
     domain, which hands it over to the display's domain. In the display's
     domain, a thread or a signal handler outside any fiber must not touch the
     display. *)
  let deliver ~owner ~refused root level message =
    if
      (not (Int.equal (Domain.self () :> int) (owner : Domain.id :> int)))
      || in_fiber ()
    then begin
      Task_driver.report ~refused root level message;
      `Written
    end
    else `Closed

  (* If the display cannot take a message, because it is closed or its output
     fails, the message goes to the reporter that the display replaced. So a log
     call never fails. A message from a source other than [Logs.default] starts
     with the source's name, so that the reader can tell interleaved libraries
     apart. An [App] message is the program's own output and has no header, as
     with [Logs_fmt.reporter]. *)
  let reporter ~previous ~owner ~closed root =
    let report src l ~over k msgf =
      if Atomic.get closed then previous.Logs.report src l ~over k msgf
      else
        msgf (fun ?header ?tags fmt ->
            Fmt.kstr
              (fun message ->
                let shown =
                  if src == Logs.default || l = Logs.App || message = "" then
                    message
                  else "[" ^ Logs.Src.name src ^ "] " ^ message
                in
                let to_previous ~over k =
                  previous.Logs.report src l ~over k (fun m ->
                      m ?header ?tags "%s" message)
                in
                let refused _ = to_previous ~over:ignore ignore in
                let delivered =
                  if shown = "" then `Written
                  else
                    match deliver ~owner ~refused root (level l) shown with
                    | d -> d
                    | exception
                        ( Failure _ | Invalid_argument _ | End_of_file
                        | Sys_error _ ) ->
                        `Closed
                    | exception e when B.write_error e -> `Closed
                in
                match delivered with
                | `Written ->
                    over ();
                    k ()
                | `Closed -> to_previous ~over k)
              fmt)
    in
    { Logs.report }

  (* The display redraws every tenth of a second on its clock, so that spinners
     turn and timers run while no task reports. When [repaint] is set, it draws
     from scratch. The end of the run stops the redraws, but never in the middle
     of one ({!whole}). The daemon checks [stopped] before each wait, because a
     cancellation can miss a running fiber. *)
  let rec animate ~stopped ~sleep ~repaint display () =
    if not (stopped ()) then begin
      sleep 0.1;
      if not (stopped ()) then begin
        if Atomic.get repaint && not (Display_driver.writing display) then begin
          Atomic.set repaint false;
          Display_driver.restart display
        end;
        whole (fun () -> Display.redraw display);
        animate ~stopped ~sleep ~repaint display ()
      end
    end

  (* While the program is stopped, the terminal looks as if there were no
     region, and the cursor is shown. When the program continues, the cursor is
     hidden and the region is drawn again. *)
  let suspend ~cursor display stop =
    if Display_driver.closed display then stop ()
    else
      Display.with_output display (fun _ ->
          cursor true;
          Fun.protect stop ~finally:(fun () -> cursor false))

  (* [first_run s f] runs [f] for the run that opened the display of [s]. Then
     it waits for the runs that joined, whether [f] returns or raises. If [f]
     raises and the wait is cancelled, it does not wait. *)
  let first_run s f =
    match B.with_binding current s.run (fun () -> f s.run.root) with
    | v ->
        last_out s;
        v
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        (try last_out s with e when cancelled e -> ());
        Printexc.raise_with_backtrace e bt

  (* [share run] makes the display of [run] the shared display of the domain,
     so that other runs can join it. *)
  let share run =
    let s =
      {
        run;
        owner = Domain.self ();
        users = Atomic.make 1;
        closing = Atomic.make false;
        idle = latch ();
        released = latch ();
      }
    in
    Atomic.set shared (Some s);
    s

  (* The work ends as a task does. It succeeds when [f] returns, fails when [f]
     raises, and is cancelled when its fiber is cancelled, unless [f] already
     ended it. The exception propagates to the caller, which reports it, so the
     work ends with no message. *)
  let end_work root = function
    | Ok _ -> Task.succeed root
    | Error e -> if cancelled e then Task.cancel root else Task.fail root

  (* At the end of a cancelled run, or of a run whose output a cancellation
     interrupted, the last writes wait for the output at most until the
     {!deadline}. *)
  let last_writes display f =
    if is_cancelled () || Display_driver.unwritten display then
      ignore (shielded f : bool)
    else f ()

  (* A signal handler outside any fiber may not be able to write on a flow. So
     it passes its work to a fiber of the run, which does it while it runs. *)
  type relay = {
    pending : (unit -> unit) list Atomic.t;
    wake : B.wake;
    listening : bool Atomic.t;
  }

  let relay () =
    {
      pending = Atomic.make [];
      wake = B.wake ();
      listening = Atomic.make false;
    }

  let rec listen ~stopped relay () =
    let next () =
      match Atomic.exchange relay.pending [] with [] -> None | l -> Some l
    in
    if not (stopped ()) then begin
      List.iter Capture.safely (List.rev (B.wait relay.wake next));
      listen ~stopped relay ()
    end

  let rec relayed relay k =
    let l = Atomic.get relay.pending in
    if Atomic.compare_and_set relay.pending l (k :: l) then B.notify relay.wake
    else relayed relay k

  let idle relay ~flow display k =
    let k () = Display_driver.when_idle display k in
    if flow && Atomic.get relay.listening && not (in_fiber ()) then
      relayed relay k
    else k ()

  (* [exit] ends the run as a return, a raise or a cancellation does. So what a
     later [at_exit] function logs or prints goes out as usual. The cursor is
     shown first, because the last frame runs a view. That view may call [exit]
     again, which ends the program at once. From another thread or domain, or
     during a write, the display's state is not touched: the terminal is given
     up instead. *)
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
             Capture.safely (fun () -> Display_driver.reveal run.display);
             close_last run;
             give_back ();
             Capture.restore ()
           end
           else begin
             Interrupt.last_resort ();
             give_back ();
             Capture.restore ~flush:false ()
           end))

  let start ?(cursor = ignore) ?(flow = false) ?terminal ~sleep
      ~intercept:formatters run f =
    let display = run.display in
    let repaint = Atomic.make false in
    let previous = Logs.reporter () in
    let closed = Atomic.make false in
    let give_back () =
      Logs.set_reporter previous;
      Atomic.set closed true
    in
    ends_at_exit run ~thread:(Thread.id (Thread.self ())) ~give_back;
    let s = share run in
    Task_driver.protect ~finally:(fun () ->
        Task_driver.protect
          (fun () -> last_writes display Capture.restore)
          ~finally:(fun () ->
            Atomic.set giving_up None;
            release ()))
    @@ fun () ->
    List.iter (capture run) formatters;
    Task_driver.ending run.root task_ended;
    let relay = relay () in
    Interrupt.protect ~idle:(idle relay ~flow display)
      ~suspend:(suspend ~cursor display)
      ~resume:(fun () -> Atomic.set repaint true)
      ?terminal
      ~restore:(fun () -> close_last run)
    @@ fun () ->
    match
      Atomic.set relay.listening true;
      Fun.protect ~finally:(fun () -> Atomic.set relay.listening false)
      @@ fun () ->
      let stop = Atomic.make false in
      let stopped () = Atomic.get stop in
      let work () =
        Fun.protect ~finally:(fun () -> Atomic.set stop true) @@ fun () ->
        Logs.set_reporter
          (reporter ~previous ~owner:(Domain.self ()) ~closed run.root);
        Fun.protect ~finally:give_back @@ fun () -> first_run s f
      in
      B.background
        [ animate ~stopped ~sleep ~repaint display; listen ~stopped relay ]
        work
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

  (* [own ppf] is a formatter for the display alone. It has the output and tag
     functions of [ppf], as [Fmt.with_buffer ~like] does. The tag functions
     carry the colours, the theme and whether [ppf] is UTF-8. With its own
     formatter, the display can route what is written on [ppf] above its
     region. Once the terminal is given up, the formatter writes nothing. *)
  let own ppf =
    let out = Format.pp_get_formatter_out_functions ppf () in
    let alive f = if not (Interrupt.is_dead ()) then f () in
    let own =
      Format.formatter_of_out_functions
        {
          out with
          out_string = (fun s o l -> alive (fun () -> out.out_string s o l));
          out_flush = (fun () -> alive out.out_flush);
        }
    in
    Format.pp_set_formatter_stag_functions own
      (Format.pp_get_formatter_stag_functions ppf ());
    own

  (* The display of [root], the formatters it captures, and the function that
     shows or hides the cursor. The display captures:
     - on a terminal: the standard formatters on that terminal;
     - quiet or as JSON events: the standard formatters that write where [ppf]
     writes, so that a task's output is printed only if the task fails, or
     becomes an event;
     - live on another output: [ppf], if it is a standard formatter;
     - live and [recorded]: both standard formatters, if [ppf] is one, because
     they share the recorded terminal;
     - as plain lines: none. *)
  let display ~recorded ~progress ?layout ?view ?keep_ended ?commit ?plain
      ?delay ppf root =
    let show ?size progress ppf =
      let view = Option.map (fun f -> f root) view in
      Progress_driver.show ~write_error:B.write_error ~cancelled:cancelling
        ~atomic ~progress ?layout ?view ?keep_ended ?commit ?plain ?delay ?size
        ppf root
    in
    let live ~size ~intercept ~cursor =
      (show ~size `Tty (own ppf), intercept, cursor)
    in
    let on_terminal () =
      Option.map
        (fun unix ->
          live
            ~size:(fun () -> Terminal.dimensions (Some unix))
            ~intercept:(Terminal.routed unix) ~cursor:(Terminal.cursor unix))
        (Terminal.of_ppf ppf)
    in
    (* In plain lines, the last frame is as wide as the terminal or the
     recording. On a pipe, it is as wide as $COLUMNS, or not cut if $COLUMNS
     is unset. *)
    let lines form =
      match (form, Terminal.descriptor ppf) with
      | (`Json | `Quiet), Some unix ->
          ( show (form :> Console.Progress.t) (own ppf),
            Terminal.sharing unix,
            ignore )
      | `Plain, _ when recorded ->
          (show ~size:Recording.size `Plain ppf, [], ignore)
      | `Plain, _ -> (
          match Terminal.of_ppf ppf with
          | Some unix ->
              let size () = Terminal.dimensions (Some unix) in
              (show ~size `Plain ppf, [], ignore)
          | None -> (show ?size:(Terminal.asked ()) `Plain ppf, [], ignore))
      | _ -> (show (form :> Console.Progress.t) ppf, [], ignore)
    in
    let std = Option.is_some (Terminal.descriptor ppf) in
    match progress with
    | (`Auto | `Tty) when recorded ->
        live ~size:Recording.size
          ~intercept:(if std then [ Fmt.stdout; Fmt.stderr ] else [])
          ~cursor:ignore
    | `Auto -> ( match on_terminal () with Some d -> d | None -> lines `Plain)
    | `Tty -> (
        match on_terminal () with
        | Some d -> d
        | None ->
            let cursor =
              match Terminal.descriptor ppf with
              | Some unix -> Terminal.cursor unix
              | None -> ignore
            in
            let intercept =
              match Terminal.descriptor ppf with
              | Some _ -> [ ppf ]
              | None -> []
            in
            live ~size:(fun () -> Terminal.dimensions None) ~intercept ~cursor)
    | (`Plain | `Quiet | `Json) as form -> lines form

  (* Open a new display and run [f] on its root task. *)
  let open_display ~ppf ?layout ?label ?view ?keep_ended ?commit ?plain ?delay
      ~progress f =
    let now = B.clock () in
    let sleep = B.tick in
    match
      let recorded = Recording.start ~time:now ppf in
      let root = Task_driver.root ?label ~scope ~now () in
      let display, intercept, cursor =
        display ~recorded ~progress ?layout ?view ?keep_ended ?commit ?plain
          ?delay ppf root
      in
      ({ display; root; json = progress = `Json }, intercept, cursor)
    with
    | run, intercept, cursor ->
        let flow = Option.is_none (Terminal.descriptor ppf) in
        let terminal =
          if Option.is_some (Cast.file ()) then None
          else Option.bind (Terminal.of_ppf ppf) Terminal.reopened
        in
        Fun.protect ~finally:(fun () -> Option.iter Unix.close terminal)
        @@ fun () -> start ~cursor ~flow ?terminal ~sleep ~intercept run f
    | exception e ->
        release ();
        raise e

  let run ?(ppf = Fmt.stderr) ?progress ?layout ?label ?view ?keep_ended ?commit
      ?plain ?delay f =
    ignore (enter () : bool);
    let progress = Option.value progress ~default:`Auto in
    match bound () with
    | Some r -> f (given r.root)
    | None ->
        (* Another fiber or domain may be opening or closing the display. This
         run then joins the next display, or opens it. *)
        let rec first () =
          match join f with
          | Some v -> v
          | None ->
              await_release ();
              if claim () then begin
                Console_tty.ensure ();
                open_display ~ppf ?layout ?label ?view ?keep_ended ?commit
                  ?plain ?delay ~progress f
              end
              else begin
                B.sleep 0.001;
                first ()
              end
        in
        first ()

  let with_output f =
    match display_of_run () with
    | Some d -> Display.with_output d f
    | None -> f Fmt.stderr

  let redraw () =
    match display_of_run () with Some d -> Display.redraw d | None -> ()

  let print v =
    match display_of_run () with
    | Some d -> Display.print d v
    | None -> Fmt.epr "%a@." Console.View.pp v
end

let on_signals ?suspend ~restore f = Interrupt.protect ?suspend ~restore f
