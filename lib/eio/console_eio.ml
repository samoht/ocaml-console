(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

(* The descriptor that [ppf] writes to, when [ppf] is {!Fmt.stdout} or
   {!Fmt.stderr}. *)
let descriptor ppf =
  if ppf == Fmt.stdout then Some (Eio_unix.Fd.stdout, Unix.stdout)
  else if ppf == Fmt.stderr then Some (Eio_unix.Fd.stderr, Unix.stderr)
  else None

let terminal ppf =
  match descriptor ppf with
  | Some (fd, unix) when Console_tty.is_terminal unix -> Some (fd, unix)
  | _ -> None

(* Two descriptors on one terminal have the same device. *)
let device unix =
  match Unix.fstat unix with
  | st -> Some st.Unix.st_rdev
  | exception Unix.Unix_error _ -> None

(* The standard formatters on the same terminal as [unix], whose lines the
   display prints above its region. A standard formatter on another terminal
   writes straight out. *)
let routed unix =
  let on_display std =
    match terminal std with
    | Some (_, u) -> Option.equal Int.equal (device u) (device unix)
    | None -> false
  in
  List.filter on_display [ Fmt.stdout; Fmt.stderr ]

(* The paths a terminal device may have: the controlling terminal, then the
   pseudo-terminals of Linux and of BSD and macOS. *)
let terminal_names () =
  let entries dir keep =
    match Sys.readdir dir with
    | names ->
        List.filter_map
          (fun n -> if keep n then Some (Filename.concat dir n) else None)
          (Array.to_list names)
    | exception Sys_error _ -> []
  in
  ("/dev/tty" :: entries "/dev/pts" (Fun.const true))
  @ entries "/dev" (String.starts_with ~prefix:"ttys")

(* [reopened unix] is the terminal of [unix] opened again, found by its
   device, so that the last resort is written on it without blocking and
   without changing the flags of a descriptor that the program shares with
   its shell. *)
let reopened unix =
  let rdev name =
    match Unix.stat name with
    | st when st.Unix.st_kind = Unix.S_CHR -> Some st.st_rdev
    | _ | (exception Unix.Unix_error _) -> None
  in
  let opened d name =
    match
      Unix.openfile name
        [ Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_NOCTTY; Unix.O_CLOEXEC ]
        0
    with
    | exception Unix.Unix_error _ -> None
    | tty when (Unix.fstat tty).st_rdev = d -> Some tty
    | tty ->
        Unix.close tty;
        None
  in
  match device unix with
  | None -> None
  | Some d ->
      (* [stat] on /dev/tty gives the device of /dev/tty itself: only an
         opened descriptor tells the device of the controlling terminal. *)
      List.find_map
        (fun name ->
          if name = "/dev/tty" || rdev name = Some d then opened d name
          else None)
        (terminal_names ())

let file unix =
  match Unix.fstat unix with
  | st -> Some (st.Unix.st_dev, st.Unix.st_ino)
  | exception Unix.Unix_error _ -> None

(* The standard formatters that write where [unix] does. *)
let sharing unix =
  let shares std =
    match (descriptor std, file unix) with
    | Some (_, u), Some f -> file u = Some f
    | _ -> false
  in
  List.filter shares [ Fmt.stdout; Fmt.stderr ]

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

let cursor unix visible =
  let s = if visible then "\027[?25h" else "\027[?25l" in
  try ignore (Unix.write_substring unix s 0 (String.length s))
  with Unix.Unix_error _ -> ()

(* The seconds since the display started, on [clock]. *)
let seconds clock =
  let start = Eio.Time.Mono.now clock in
  fun () ->
    Mtime.Span.to_float_ns (Mtime.span start (Eio.Time.Mono.now clock)) *. 1e-9

let deterministic = Cast.deterministic

(* The virtual clock of CONSOLE_DETERMINISTIC_CLOCK. A sleep of [d] started
   at time [t] wakes at [t + d], once it has also lasted [d] on [real].
   Sleeps wake one at a time, by wake-up time and then by start, and each
   sets the time to its wake-up time as its fiber resumes. Before the next
   one wakes, {!settle} lets the fibers that the last one made runnable run,
   until two yields in a row see no use of the clock. So the time does not
   depend on the order in which the real durations end.

   A passive sleep, that of a redraw, wakes in the same order, but only while
   an active sleep, one of the program, comes after it, so that a redraw
   never moves the time further than the program would. It stops waiting on
   [real] as soon as an active sleep after it has lasted its real duration,
   so that it never holds the program up. For a recording, which has its own
   times, no sleep waits on [real]. *)
module Sleeps = struct
  type sleep = {
    until : Mtime.t;
    seq : int;
    active : bool;
    mutable elapsed : bool;
    mutable pending : bool;
    mutable waiting : unit Eio.Promise.u option;
    overtaken : unit Eio.Promise.t * unit Eio.Promise.u;
  }

  type t = {
    real : Eio.Time.Mono.ty Eio.Resource.t;
    waits : bool;
    mutable now : Mtime.t;
    mutable sleeps : sleep list;
    mutable next : int;
    mutable used : int;
  }

  type time = Mtime.t

  let now t =
    t.used <- t.used + 1;
    t.now

  let before a b =
    match Mtime.compare a.until b.until with 0 -> a.seq < b.seq | c -> c < 0

  let rec insert s = function
    | x :: rest when before x s -> x :: insert s rest
    | l -> s :: l

  let due t =
    match t.sleeps with
    | s :: rest
      when s.elapsed && (s.active || List.exists (fun x -> x.active) rest) ->
        Some s
    | _ -> None

  let is_due t s = match due t with Some x -> x == s | None -> false

  let wake t =
    match due t with
    | Some ({ waiting = Some u; _ } as s) ->
        s.waiting <- None;
        Eio.Promise.resolve u ()
    | _ -> ()

  let remove t s =
    if s.pending then begin
      s.pending <- false;
      t.sleeps <- List.filter (fun x -> x != s) t.sleeps;
      wake t
    end

  (* The rounds are bounded, so that a fiber that polls the clock in a loop
     of yields cannot stop it. *)
  let settle t =
    let rec go quiet rounds =
      if quiet < 2 && rounds < 64 then begin
        let used = t.used in
        Eio.Fiber.yield ();
        go (if t.used = used then quiet + 1 else 0) (rounds + 1)
      end
    in
    go 0 0

  let rec turn t s =
    if is_due t s then begin
      settle t;
      if is_due t s then begin
        t.now <- s.until;
        t.used <- t.used + 1;
        remove t s
      end
      else turn t s
    end
    else begin
      let p, u = Eio.Promise.create () in
      s.waiting <- Some u;
      Eio.Promise.await p;
      turn t s
    end

  let overtake t s =
    let rec go = function
      | x :: rest when x != s ->
          if not x.active then
            ignore (Eio.Promise.try_resolve (snd x.overtaken) ());
          go rest
      | _ -> ()
    in
    go t.sleeps

  let overtaken t s =
    List.exists (fun x -> x.active && x.elapsed && before s x) t.sleeps

  let elapse t s span =
    if not t.waits then ()
    else if s.active then begin
      Eio.Time.Mono.sleep_span t.real span;
      overtake t s
    end
    else if not (overtaken t s) then
      Eio.Fiber.first
        (fun () -> Eio.Time.Mono.sleep_span t.real span)
        (fun () -> Eio.Promise.await (fst s.overtaken))

  let sleep t ~active time =
    if Mtime.is_later time ~than:t.now then begin
      let s =
        {
          until = time;
          seq = t.next;
          active;
          elapsed = false;
          pending = true;
          waiting = None;
          overtaken = Eio.Promise.create ();
        }
      in
      t.next <- t.next + 1;
      t.used <- t.used + 1;
      t.sleeps <- insert s t.sleeps;
      wake t;
      Fun.protect
        ~finally:(fun () -> remove t s)
        (fun () ->
          elapse t s (Mtime.span t.now time);
          s.elapsed <- true;
          turn t s)
    end

  let sleep_until t time = sleep t ~active:true time

  let redraw t d =
    let span = Mtime.Span.of_uint64_ns (Int64.of_float (d *. 1e9)) in
    Option.iter (sleep t ~active:false) (Mtime.add_span t.now span)

  (* The time, read without counting as a use. *)
  let seconds t () = Int64.to_float (Mtime.to_uint64_ns t.now) *. 1e-9
end

type (_, _, _) Eio.Resource.pi +=
  | Redraw : ('t, 't -> float -> unit, [> `Console_sleeps ]) Eio.Resource.pi
  | Seconds : ('t, 't -> unit -> float, [> `Console_sleeps ]) Eio.Resource.pi

let sleeps =
  Eio.Resource.handler
    (H (Redraw, Sleeps.redraw)
    :: H (Seconds, Sleeps.seconds)
    :: Eio.Resource.bindings (Eio.Time.Pi.clock (module Sleeps)))

let clock c =
  let real = (c :> Eio.Time.Mono.ty Eio.Resource.t) in
  if deterministic () then
    Eio.Resource.T
      ( {
          Sleeps.real;
          waits = Option.is_none (Cast.file ());
          now = Mtime.of_uint64_ns 0L;
          sleeps = [];
          next = 0;
          used = 0;
        },
        sleeps )
  else real

(* The sleep of a redraw on a virtual clock of {!clock}, which waits on its
   time; [None] for another clock. *)
let redraw (Eio.Resource.T (t, ops)) =
  Option.map (fun redraw -> redraw t) (Eio.Resource.get_opt ops Redraw)

(* The time of a virtual clock of {!clock}; zero for another clock. *)
let virtual_time clock =
  let (Eio.Resource.T (t, ops)) = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  match Eio.Resource.get_opt ops Seconds with
  | Some seconds -> seconds t
  | None -> fun () -> 0.

(* The time the display reads and the sleep its redraws wait on. Under
   CONSOLE_DETERMINISTIC_CLOCK, a virtual clock of {!clock} gives both, and a
   redraw does not move its time; with any other clock the time stays at
   zero. *)
let reading clock =
  let clock = (clock :> Eio.Time.Mono.ty Eio.Resource.t) in
  if not (deterministic ()) then (seconds clock, Eio.Time.Mono.sleep clock)
  else
    match redraw clock with
    | Some redraw -> (seconds clock, redraw)
    | None -> ((fun () -> 0.), Eio.Time.Mono.sleep clock)

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
let display ~recorded ~progress ?layout ?view ?keep_ended ?commit ?plain ?delay
    ppf root =
  let show ?size progress ppf =
    let view = Option.map (fun f -> f root) view in
    Progress_driver.show ~write_error:Session.write_error
      ~cancelled:Session.cancelling ~atomic:Session.atomic ~progress ?layout
      ?view ?keep_ended ?commit ?plain ?delay ?size ppf root
  in
  let live ~size ~intercept ~cursor =
    (show ~size `Tty (own ppf), intercept, cursor)
  in
  let on_terminal () =
    Option.map
      (fun (fd, unix) ->
        live
          ~size:(fun () -> Terminal.dimensions (Some fd))
          ~intercept:(routed unix) ~cursor:(cursor unix))
      (terminal ppf)
  in
  (* In plain lines, the last frame is as wide as the terminal or the
     recording; on a pipe, as wide as $COLUMNS asks, or not cut without
     it. *)
  let lines form =
    match (form, descriptor ppf) with
    | (`Json | `Quiet), Some (_, unix) ->
        (show (form :> Console.Progress.t) (own ppf), sharing unix, ignore)
    | `Plain, _ when recorded ->
        (show ~size:Recording.size `Plain ppf, [], ignore)
    | `Plain, _ -> (
        match terminal ppf with
        | Some (fd, _) ->
            let size () = Terminal.dimensions (Some fd) in
            (show ~size `Plain ppf, [], ignore)
        | None -> (show ?size:(Terminal.asked ()) `Plain ppf, [], ignore))
    | _ -> (show (form :> Console.Progress.t) ppf, [], ignore)
  in
  let std = Option.is_some (descriptor ppf) in
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
            match descriptor ppf with
            | Some (_, unix) -> cursor unix
            | None -> ignore
          in
          let intercept =
            match descriptor ppf with Some _ -> [ ppf ] | None -> []
          in
          live ~size:(fun () -> Terminal.dimensions None) ~intercept ~cursor)
  | (`Plain | `Quiet | `Json) as form -> lines form

(* Open a new display and run [f] on its root task. *)
let open_display ~clock ~ppf ?layout ?label ?view ?keep_ended ?commit ?plain
    ?delay ~progress f =
  let now, sleep = reading clock in
  match
    let recorded = Recording.start ~time:(virtual_time clock) ppf in
    let root = Task_driver.root ?label ~scope:Session.scope ~now () in
    let display, intercept, cursor =
      display ~recorded ~progress ?layout ?view ?keep_ended ?commit ?plain
        ?delay ppf root
    in
    ({ Session.display; root; json = progress = `Json }, intercept, cursor)
  with
  | run, intercept, cursor ->
      let flow = Option.is_none (descriptor ppf) in
      let terminal =
        if Option.is_some (Cast.file ()) then None
        else Option.bind (terminal ppf) (fun (_, unix) -> reopened unix)
      in
      Fun.protect ~finally:(fun () -> Option.iter Unix.close terminal)
      @@ fun () -> Session.run ~cursor ~flow ?terminal ~sleep ~intercept run f
  | exception e ->
      Session.release ();
      raise e

let run ~clock ?(ppf = Fmt.stderr) ?progress ?layout ?label ?view ?keep_ended
    ?commit ?plain ?delay f =
  let progress = Option.value progress ~default:`Auto in
  match Session.find () with
  | Some root -> f (Session.given root)
  | None ->
      (* Another fiber or domain may be opening or closing the display: this
         run then joins the next, or opens it. *)
      let rec first () =
        match Session.join f with
        | Some v -> v
        | None ->
            Session.await_release ();
            if Session.claim () then begin
              Console_tty.ensure ();
              open_display ~clock ~ppf ?layout ?label ?view ?keep_ended ?commit
                ?plain ?delay ~progress f
            end
            else begin
              Eio.Fiber.yield ();
              first ()
            end
      in
      first ()

let with_output f =
  match Session.display () with
  | Some d -> Console.Display.with_output d f
  | None -> f Fmt.stderr

let redraw () =
  match Session.display () with
  | Some d -> Console.Display.redraw d
  | None -> ()

let size ppf =
  Option.map (fun (fd, _) () -> Terminal.dimensions (Some fd)) (terminal ppf)

(* The widest a display draws, whatever the terminal says. *)
let max_width = 4096

let width ppf =
  let first (size : unit -> int * int) = fst (size ()) in
  if Option.is_some (Cast.file ()) then Some (first Recording.size)
  else
    match size ppf with
    | Some size -> Some (Int.min max_width (first size))
    | None -> Option.map first (Terminal.asked ())

let print v =
  match Session.display () with
  | Some d -> Console.Display.print d v
  | None -> Fmt.epr "%a@." Console.View.pp v
