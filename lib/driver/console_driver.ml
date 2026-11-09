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

module Make (B : S) = struct
  type run = Capture.run = { display : Display.t; root : Task.t; json : bool }

  (* A terminal has one live region at a time. The display that holds it is
     bound in the fibers of its run ([current]), so that a [run] in a fiber
     under another finds it and adds to it. It is also global ([shared]), so
     that a [run] in another fiber or domain joins it, and an exit in the middle
     of the work still closes it. *)

  (* The driver calls [B] only on the home thread of a domain, the first that
     starts a run or makes a task current there, and [B.notify] anywhere. *)
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
  let current : run B.key = B.key ()
  let task : Task.t B.key = B.key ()
  let busy = Atomic.make false
  let ending : (unit -> unit) option Atomic.t = Atomic.make None

  (* What fibers wait for once. *)
  type latch = { reached : bool Atomic.t; wake : B.wake }

  let latch () = { reached = Atomic.make false; wake = B.wake () }

  let reach l =
    Atomic.set l.reached true;
    B.notify l.wake

  let await l =
    B.wait l.wake (fun () -> if Atomic.get l.reached then Some () else None)

  (* The runs that share the display of a domain: the one that opened it and
     those that joined it. It closes once the last of them ends. *)
  type shared = {
    run : run;
    owner : Domain.id;
    users : int Atomic.t;
    closing : bool Atomic.t;
    idle : latch;
    released : latch;
  }

  let shared : shared option Atomic.t = Atomic.make None

  (* A fiber that outlives its run keeps the binding of a closed display, so the
     binding counts only while it is the shared display and not closing. *)
  let bound () =
    match (get current, Atomic.get shared) with
    | Some r, Some s when r == s.run && not (Atomic.get s.closing) -> Some r
    | _ -> None

  let elsewhere s = not (Int.equal (Domain.self () :> int) (s.owner :> int))

  (* A fiber of another domain has no binding, and reaches the program's run. *)
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

  (* A run that starts while the display of its domain closes opens its own
     once that one is gone. *)
  let await_release () =
    match Atomic.get shared with
    | Some s when Atomic.get s.closing -> await s.released
    | _ -> ()

  (* The run that opened the display waits for those that joined it. *)
  let last_out s =
    Fun.protect ~finally:(fun () -> Atomic.set s.closing true) @@ fun () ->
    if Atomic.fetch_and_add s.users (-1) > 1 then await s.idle

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
    | Fun.Finally_raised e -> cancelled e
    | e -> B.cancelled e

  (* The current task is a fiber binding, so it stays current in the fibers
     that its function forks. Outside any fiber, as in a thread or a signal
     handler, no task is current. *)
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
    B.sleep (Float.max 0. (deadline -. Unix.gettimeofday ()))

  (* [shielded ~armed f] runs [f ()] protected from cancellation, which a flow
     may need to write, until the {!deadline} at most, taken once [armed ()]
     returns (defaults to at once); then [f] is cancelled. It is [true] iff [f]
     finished in time. *)
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

  (* An update written in a cancelled fiber goes out whole, as in a fiber that
     is not cancelled, but waits for its output until the {!deadline} at most;
     if the deadline cuts it short, the cancellation propagates. Cut half way
     by the cancellation, the update would leave rows of the region on the
     screen. *)
  let atomic f =
    if not (in_fiber () && is_cancelled ()) then f ()
    else if not (shielded f) then B.check ()

  (* [whole f] is [f ()], which a cancellation that arrives after it started
     does not cut. From the cancellation on, [f] waits for the output until the
     {!deadline} at most; if the deadline cuts it short, the cancellation
     propagates. *)
  let whole f =
    let cut = latch () in
    let finished = ref false in
    B.first
      (fun () -> finished := shielded ~armed:(fun () -> await cut) f)
      (fun () -> Fun.protect await_cancel ~finally:(fun () -> reach cut));
    if not !finished then B.check ()

  let claim () =
    at_exit ();
    Atomic.compare_and_set busy false true

  let release () =
    Option.iter (fun s -> reach s.released) (Atomic.exchange shared None);
    Atomic.set ending None;
    Atomic.set busy false

  let direct : unit B.key = B.key ()

  (* Outside any fiber, as in an [at_exit] function, no bytes go straight out. *)
  let is_direct () = Option.is_some (get direct)
  let capture = Capture.take ~direct:is_direct

  (* When a task ends, what the formatters hold is flushed as its output, and its
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

  (* Closes the display while the program ends for another reason, which
     propagates: what a view or a hook raises is dropped, unless it is
     asynchronous and the reason is not [urgent] already. *)
  let close_quietly ?first ?(urgent = false) run =
    try close_display ?first run
    with e when urgent || not (Task_driver.is_async e) -> ()

  (* From a signal handler or an [at_exit] function, where nothing runs after it,
     it also drops an interrupt or an exhausted heap or stack. *)
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

  (* A message from a fiber of the domain that runs the display, or from another
     domain, which leaves it there. A thread of that domain or a signal handler
     outside any fiber must not touch the display. *)
  let deliver ~owner ~refused root level message =
    if
      (not (Int.equal (Domain.self () :> int) (owner : Domain.id :> int)))
      || in_fiber ()
    then begin
      Task_driver.report ~refused root level message;
      `Written
    end
    else `Closed

  (* A message that the display cannot take, because it is closed or its
     output fails, goes to the reporter it replaced, so that a log call never
     fails. A message from a source other than [Logs.default] is prefixed with
     the source's name, which tells apart the libraries whose messages
     interleave. An [App] message is the program's own output and has no
     header, as with [Logs_fmt.reporter]. *)
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

  (* The display redraws every tenth of a second on its clock, so that the
     spinners turn and the timers run while no task reports, and it draws
     afresh once [repaint] is set. The end of the run stops the redraws, but
     never in the middle of one ({!whole}). A daemon checks [stopped] before
     each wait, since a cancellation can miss a fiber that runs. *)
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
    match B.with_binding current s.run (fun () -> f s.run.root) with
    | v ->
        last_out s;
        v
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        (try last_out s with e when cancelled e -> ());
        Printexc.raise_with_backtrace e bt

  (* [share run] makes the display of [run] the domain's, for the runs that
     join it. *)
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
    if is_cancelled () || Display_driver.unwritten display then
      ignore (shielded f : bool)
    else f ()

  (* A signal handler that runs outside any fiber may not be able to write on
     a flow, so a fiber of the run does what it asks, while that fiber runs. *)
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

  (* A formatter for the display alone, with the output and the tag
     functions of [ppf], as [Fmt.with_buffer ~like] makes one. The tag
     functions carry the colours, the theme and whether [ppf] is UTF-8. With a
     formatter of its own, the display can route what is written on [ppf]
     above its region. Once the terminal is given up, it writes nothing. *)
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

  (* The display of [root], the formatters whose lines it takes, and the
     function that shows or hides the cursor. The display takes:
     - on a terminal, the standard formatters on that terminal;
     - quiet or as JSON events, the standard formatters that write where [ppf]
     does, so that a task's output is printed only if it fails, or becomes
     an event;
     - live on another output, [ppf] if it is a standard formatter;
     - live when [recorded], both standard formatters if [ppf] is one, since
     they share the recorded terminal;
     - as plain lines, none. *)
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
     recording; on a pipe, as wide as $COLUMNS asks, or not cut without
     it. *)
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
        (* Another fiber or domain may be opening or closing the display: this
         run then joins the next, or opens it. *)
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
