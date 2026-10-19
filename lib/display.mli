(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Durable progress events and a semantic view of current activity.

    A display has two deliberately separate projections. {!history} is an
    append-only sequence of immutable events: task starts, logs, completions,
    failures, cancellations and session messages. {!activity} is an immutable
    snapshot of work that is running now. [`Activity] mode renders that snapshot
    in one bounded terminal region; [`History] mode emits only permanent events
    and never moves the cursor.

    {!run} scopes terminal ownership. It always restores the terminal, succeeds
    work still running when the callback returns normally, and cancels it when
    the callback raises. Applications cannot forget task or terminal teardown.

    {[
    let build ~ctx =
      Console.Display.run ~ctx ~header:"Building" @@ fun display ->
      let resolve = Console.Display.task display "resolve image" in
      let layer = Console.Display.task ~parent:resolve display "sha256:abcd" in
      Console.Display.set_bytes layer ~cur:1024L ~total:1048576L;
      Console.Display.succeed layer;
      Console.Display.succeed resolve
    ]} *)

(** {1:task_layout Task layout} *)

module Line : sig
  type piece
  (** The type for the pieces of a task line. *)

  val spinner : piece
  (** [spinner] is the animated activity marker, replaced by the outcome marker
      when the task finishes. *)

  val label : piece
  (** [label] is the task label, or its completion message, in a cell of
      flexible width. *)

  val bar : int -> piece
  (** [bar width] is a progress bar occupying [width] terminal cells. *)

  val percent : piece
  (** [percent] is the completion percentage of a task with a known total. *)

  val count : piece
  (** [count] is the item or byte count and its total, right-aligned in one cell
      for the whole display. The cell is sized from the totals the rows
      announce, never from what they have reached, so a count holds its columns
      as it climbs and every row of a block ends its pair in the same place. A
      row with no metric leaves the cell empty. *)

  val rate : piece
  (** [rate] is the transfer rate of a task that counts bytes. *)

  val elapsed : piece
  (** [elapsed] is the time since the task started, right-aligned in a cell wide
      enough for the widest {!pp_span} below a hundred hours, so nothing to its
      left moves when the timer crosses ten seconds, a minute or an hour. *)

  val text : string -> piece
  (** [text s] is a sanitized literal separator. *)

  val accent : string -> piece
  (** [accent s] is sanitized literal text in the task accent colour. *)

  val spacer : piece
  (** [spacer] is flexible space that pushes the pieces after it to the right
      edge. *)

  val marker : (frame:int -> Span.t) -> piece
  (** [marker f] is an activity marker drawn as [f ~frame:n] at every paint,
      where [n] is the display's {!val-frame}, so its glyphs and colours may
      change from one frame to the next. Once the task finishes it is the
      outcome marker {!spinner} draws. *)

  type t
  (** The type for task-line layouts and their accent colour. *)

  val v : ?color:Color.t -> piece list -> t
  (** [v pieces] is a layout assembled left to right.

      @raise Invalid_argument
        if [pieces] contains more than one label or spacer. *)

  val default : t
  (** [default] is the layout of a label, a bar, a count, a rate and the
      right-aligned elapsed time. *)
end

(** {1:rendering_context Rendering context} *)

type mode = [ `Auto | `Activity | `History ]
(** The type for display modes. [`Activity] renders permanent history plus a
    bounded projection of running tasks. [`History] is strictly append-only.
    [`Auto] selects [`Activity] on a terminal and [`History] otherwise. *)

type ctx
(** The type for display contexts, the capability to start a display. It is
    deliberately not obtainable from an active display; ordinary Eio
    applications let [Console_eio.run] construct it. *)

val ctx :
  ?depth:Color.depth ->
  ppf:Format.formatter ->
  now:(unit -> float) ->
  wait:(float -> unit) ->
  dimensions:(unit -> int * int) ->
  is_tty:bool ->
  unit ->
  ctx
(** [ctx ~ppf ~now ~wait ~dimensions ~is_tty ()] is a display context on the
    output [ppf], the clock [now], the terminal geometry [dimensions] and the
    terminal answer [is_tty]. Driver libraries normally build it. Every colour
    the display draws is {!Color.downsample}d to [depth] (defaults to
    [`True_color]).

    [now ()] is the time the display reads, in seconds, and [wait s] returns
    once [now] has advanced by [s]: they are one clock, and whatever paces the
    display or the work drawn on it waits through {!val-wait} rather than on a
    clock of its own, so a context whose clock a test moves by hand moves every
    reader of it.

    [is_tty] says whether a live region can be drawn at all; whether that region
    carries colour is [ppf]'s own answer, [Fmt.style_renderer], which is what
    {!Span.pp} and every other surface here renders by, and which
    [Console_eio.run] and [Console_eio.setup] set from [--color], [NO_COLOR] and
    [TERM]. So a terminal whose reader turned colour off still gets the region,
    drawn in place and with no ANSI in it, and a formatter nobody configured
    draws plain the way an unconfigured [Fmt.styled] prints plain. It is read
    once, when the display opens. *)

(** {1:immutable_event_history Immutable event history} *)

module Event : sig
  type level = [ `Info | `Warning | `Error ]
  (** The type for message severities. *)

  type outcome = [ `Succeeded | `Failed | `Cancelled ]
  (** The type for task outcomes. *)

  type result = [ `Ok | `Fail ]
  (** The type for session verdicts. *)

  type kind =
    [ `Message of level  (** A session message, {!event}. *)
    | `Started  (** A task started. *)
    | `Log of level  (** A task's log line, {!log}. *)
    | `Finished of outcome  (** A task finished. *)
    | `Result of result  (** The session's verdict, {!set_result}. *) ]
  (** The type for event kinds. *)

  type t
  (** The type for events. *)

  val elapsed : t -> float
  (** [elapsed e] is the time of [e] in seconds since the session started. *)

  val task : t -> int option
  (** [task e] is the numeric identity of the task of [e], or [None] for a
      session-level event. *)

  val key : t -> string option
  (** [key e] is the stable identity the caller gave the task of [e], if any. *)

  val path : t -> string list
  (** [path e] are the labels of the tasks from the root task down to the task
      of [e]. *)

  val kind : t -> kind
  (** [kind e] is the kind of [e]. *)

  val message : t -> string
  (** [message e] is the sanitized message of [e]. *)

  val detail : t -> string option
  (** [detail e] is the result detail given to {!set_result}, if any. *)

  val pp : t Fmt.t
  (** [pp] formats an event for diagnostics, in an unspecified format. *)
end

(** {1:immutable_activity_projection Immutable activity projection} *)

module Activity : sig
  type progress = [ `Spin | `Count of int * int | `Bytes of int64 * int64 ]
  (** The type for task progress: indeterminate, an item count or a byte count,
      each as done and total. *)

  type item
  (** The type for running tasks. *)

  val id : item -> int
  (** [id i] is the numeric identity of the task of [i]. *)

  val key : item -> string option
  (** [key i] is the stable identity the caller gave the task, if any. *)

  val parent : item -> int option
  (** [parent i] is the numeric identity of the parent task, if any. *)

  val label : item -> string
  (** [label i] is the current label of the task. *)

  val progress : item -> progress
  (** [progress i] is the current progress of the task. *)

  val elapsed : item -> float
  (** [elapsed i] is the time in seconds since the task started. *)

  val logs : item -> Event.t list
  (** [logs i] are the [`Log] events of the latest log lines of the task, oldest
      first, the window lines of [~keep:false] included. *)

  type t
  (** The type for activity snapshots. *)

  val items : t -> item list
  (** [items a] are the running tasks of [a] in display order. *)

  val pp : t Fmt.t
  (** [pp] formats a snapshot for diagnostics, in an unspecified format. *)
end

(** {1:scoped_sessions Scoped sessions}

    Every function below that takes a display or a task raises
    [Invalid_argument] once that display has closed, except {!pp}, {!history},
    {!activity}, {!frame}, {!on_frame}, {!now}, {!wait} and {!tick}. *)

type t
(** The type for display sessions. Values are created only by {!run}. *)

val run :
  ctx:ctx ->
  ?mode:mode ->
  ?theme:Theme.t ->
  ?palette:Color.t list ->
  ?bar:Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  (t -> 'a) ->
  'a
(** [run ~ctx f] owns a display for the duration of [f]. Width and height are
    re-read from [ctx] unless explicitly pinned. The activity projection is
    bounded to the terminal height and every rendered line is bounded to its
    width.

    If [f] returns with running tasks, they are succeeded children-first. If [f]
    raises, they are cancelled children-first and the original exception is
    re-raised. The terminal is restored in both cases.

    One display runs at a time in a program.

    @raise Invalid_argument
      if [width] or [height] is not positive, or if a display is already
      running. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output display f] temporarily removes the activity projection and
    passes [f] the owned formatter, configured to the current terminal width. It
    then redraws the current snapshot. It is intended for prompts,
    formatter-based widgets and third-party commands that cannot emit through
    {!event} or {!log}. Calls may be nested. History mode simply calls [f].

    Prefer {!event} for ordinary messages: it records their semantics as well as
    printing them.

    @raise Invalid_argument
      if [display] is an activity display that does not own the terminal's
      activity region. *)

type task
(** The type for running or completed units of work owned by one display. *)

val task :
  ?id:string ->
  ?parent:task ->
  ?bar:Line.t ->
  ?color:Color.t ->
  ?log_lines:int ->
  ?total:int ->
  ?transient:bool ->
  ?group:bool ->
  t ->
  string ->
  task
(** [task display label] starts a task. With [~id], repeated calls return the
    same task, so event streams can use their native stable IDs without keeping
    a registry. The first call fixes its parent and rendering options; use
    {!set_label} when a later start event supplies a better label. [label] is
    sanitized: a newline or another control character becomes a space, so a task
    is always drawn on one row.

    {b The first parent wins.} A later call offering a different [parent] for a
    known [id] returns the existing task where it already sits, and does not
    move it: an event stream carries one id across whatever scopes its emitter
    passes through, and the scope the work began in is the truthful answer.
    Offering a parent is therefore always safe, on every event, so a caller
    never has to track which of them is allowed to name one.

    [total] starts determinate count progress at zero; without it, the task
    starts with an indeterminate spinner. [parent] must belong to [display]. A
    task first observed after its parent finished inherits success, or
    cancellation when the parent failed or was cancelled. [log_lines] defaults
    to eight and controls only the bounded activity tail; every log is still
    permanent.

    [transient] (defaults to [false]) makes a task that is drawn while it runs
    and leaves no line when it ends: the screen keeps what happened, not how the
    work got there. Its start, its ending and its logs are in {!history} like
    any task's, and a pipe, which draws nothing that runs, never sees it.

    [group] (defaults to [false]) makes a group row, the way a build draws one
    row per service with its steps beneath it. A row under an open group that
    ends stays drawn beneath the group, with its outcome and its frozen timer,
    and its permanent line waits for the group's: when the group settles, its
    line and then its rows' lines, each with its kept log lines, are written as
    one block, on the block's grid. A group settles when its last row does:
    {!succeed}, {!fail} or {!cancel} on a group with rows still open records
    that ending, and the group takes it, with its timer stopped then, when the
    last of those rows ends. A pipe writes every line as it comes.

    @raise Invalid_argument
      if the display is closed, [id] is empty, [log_lines] or [total] is
      negative, or [parent] belongs to another display. *)

val set_label : task -> string -> unit
(** [set_label task label] atomically changes the live label of [task] to
    [label], sanitized as in {!task}. *)

val set_count : task -> cur:int -> total:int -> unit
(** [set_count task ~cur ~total] records monotonic count progress. Estimates
    expand automatically when [cur] exceeds [total].

    @raise Invalid_argument
      if [cur] or [total] is negative, or [task] counts bytes. *)

val advance : ?by:int -> ?total:int -> ?label:string -> task -> unit
(** [advance task] increments count progress by one. [by] selects another
    non-negative increment, [label] updates the activity label in the same
    repaint, and [total] supplies a new estimate. The known total expands
    automatically if progress passes it. An indeterminate task becomes a count
    task on its first advance.

    @raise Invalid_argument
      if [by] or [total] is negative, or [task] counts bytes. *)

val set_bytes : task -> cur:int64 -> total:int64 -> unit
(** [set_bytes task ~cur ~total] is like {!set_count} for a task that counts
    bytes. Counts and byte counts must be non-negative, and a [cur] beyond
    [total] raises the total to [cur]. Progress is monotonic: an older update
    cannot move a task backwards or reduce its known total. Updates after the
    task finished do nothing. A task cannot switch between counting items and
    counting bytes.

    @raise Invalid_argument
      if [cur] or [total] is negative, or [task] counts items. *)

val log : ?level:Event.level -> ?keep:bool -> task -> string -> unit
(** [log task text] appends each line permanently to {!history}; the latest
    lines also appear in the task's bounded activity tail. Logs arriving after
    completion remain attached to the completed task in permanent history.

    With [~keep:false] (default [true]) a line is a window line: it is drawn in
    the task's tail while it is one of the last [log_lines], and it never
    reaches history or {!history}, neither when it leaves the tail nor when the
    task ends. Where there is no tail to draw it in (an appending display, a
    task with no [log_lines], a finished task) it is not written at all. This is
    how a live stream is watched in a bounded window, the way BuildKit's TTY
    progress shows the last lines of a running step and drops them when the step
    ends.

    A tail line that does not fit the terminal loses words from its end, and one
    whose first word does not fit folds under it, whole; the tail is [log_lines]
    lines high on screen whatever its lines fold into. *)

val event : ?level:Event.level -> t -> string -> unit
(** [event display text] appends permanent session-level messages, one per input
    line. Physical line breaks are preserved. Use this instead of writing
    directly while an activity projection owns the output. *)

val succeed : ?message:string -> task -> unit
(** [succeed ~message task] finishes [task] successfully, recording [message] in
    place of its label if given. A task finishes once: the first of {!succeed},
    {!fail} and {!cancel} wins and later calls do nothing. Its unfinished
    descendants finish first, children before parents: they succeed when [task]
    succeeds and are cancelled when it fails or is cancelled. A group with rows
    still open finishes when the last of them does instead (see {!task}). *)

val fail : ?message:string -> task -> unit
(** [fail ~message task] is like {!succeed} but finishes [task] as failed. *)

val cancel : ?message:string -> task -> unit
(** [cancel ~message task] is like {!succeed} but finishes [task] as cancelled.
*)

val with_task :
  ?id:string ->
  ?parent:task ->
  ?bar:Line.t ->
  ?color:Color.t ->
  ?log_lines:int ->
  ?total:int ->
  ?transient:bool ->
  ?group:bool ->
  t ->
  string ->
  (task -> 'a) ->
  'a
(** [with_task display label f] starts a task, calls [f], and succeeds the task
    when [f] returns. If [f] raises, the task fails and the exception is
    re-raised. An explicit outcome inside [f] wins, so callers can record a
    handled failure or cancellation without extra scope state. *)

val history : t -> Event.t list
(** [history display] is the permanent event history, oldest first. *)

val activity : t -> Activity.t
(** [activity display] is a snapshot of currently running tasks. *)

val set_result :
  ?detail:string -> t -> [ `Ok of string | `Fail of string ] -> unit
(** [set_result display result] settles remaining tasks, records the final
    verdict as a permanent semantic event, and emits it when {!run} closes.
    [`Ok] succeeds remaining work; [`Fail] cancels it. [detail], if given, is
    written dimmed above the verdict, one line per line of it. It is
    exactly-once; a later call is a harmless no-op. *)

val frame : t -> int
(** [frame display] is the number of times {!tick} has refreshed [display]'s
    activity projection: the frame an animation draws. It moves with the
    driver's ticks and not with the display's clock, so a display whose clock is
    frozen still animates. *)

val on_frame : t -> int -> (unit -> unit) -> unit
(** [on_frame display n k] calls [k ()] once, when the terminal holds frame [n]
    of [display] ({!val-frame}): from the paint that puts it there, which is the
    {!tick} that reaches [n] unless the region is lent out by {!with_output},
    and then the paint that gives it back. It calls [k] at once when that frame
    or a later one is already drawn, and when [display] draws no frames: an
    appending display, or one that has closed. A frame still to come when
    [display] closes calls [k] as it closes. [k] runs inside the paint, so it
    resolves or signals and returns; a driver such as [Console_eio] turns it
    into a promise a fiber awaits. *)

val set_scene : t -> (frame:int -> width:int -> Span.t list) -> unit
(** [set_scene display scene] draws [scene ~frame ~width] above the header of
    [display]'s activity projection at every paint, the frame being {!val-frame}
    and [width] the terminal's width, and replaces any scene set before. Each
    line is cut to the width. The scene is left out of a paint whose terminal is
    too short to hold it with the header and every running row, and an appending
    display never draws it, so it holds nothing that is not also said elsewhere.
*)

val now : t -> float
(** [now display] is the time on the clock of the context [display] was opened
    with. *)

val wait : t -> float -> unit
(** [wait display s] returns once the clock of the context [display] was opened
    with has advanced by [s] seconds. A driver's refresh and a renderer's
    periodic work wait here, so the display has one clock. *)

val tick : t -> unit
(** [tick display] refreshes an activity projection. Driver libraries call it
    periodically; it is a no-op after the session closes.

    A task update repaints the projection when the frame on screen is 100 ms old
    or older, and otherwise changes the snapshot and leaves the paint to the
    next update past that age or to [tick], so a producer reporting every few
    kilobytes writes the terminal at the eye's pace and not its own. Permanent
    events paint at once. *)

(** {1:fmt Formatting} *)

val pp_bytes : int64 Fmt.t
(** [pp_bytes] formats a byte count as a row draws it, in the largest binary
    unit that keeps it at least one: ["512 B"], ["1.50 KiB"], ["30.00 MiB"]. *)

val pp_span : float Fmt.t
(** [pp_span] formats seconds as a row draws its elapsed time: tenths of a
    second below a minute (["6.4s"]), then minutes and seconds (["1m05s"]), then
    hours and minutes (["1h02m"]). A negative span is ["0.0s"]. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics, in an unspecified format. *)

(**/**)

val restore : unit -> unit
(** [restore ()] gives the terminal back if a display is drawing on it: text
    attributes reset and the cursor shown. A driver calls it when a signal ends
    the program. *)

(**/**)
