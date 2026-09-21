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

type color = Color.t
(** A foreground colour. *)

val bytes : int64 -> string
(** [bytes n] is [n] bytes formatted in the largest suitable binary unit. *)

val span : float -> string
(** [span seconds] is [seconds] as a row draws its elapsed time: tenths of a
    second below a minute (["6.4s"]), then minutes and seconds (["1m05s"]), then
    hours and minutes (["1h02m"]). A negative [seconds] is ["0.0s"]. *)

(** {1 Task layout} *)

module Line : sig
  type piece
  (** One piece of a task's activity rendering. *)

  val spinner : piece
  (** Animated activity marker, replaced by the outcome marker on completion. *)

  val label : piece
  (** Flexible task label or completion message. *)

  val bar : int -> piece
  (** [bar width] is a progress bar occupying [width] terminal cells. *)

  val percent : piece
  (** Completion percentage for determinate progress. *)

  val count : piece
  (** Item count or byte-count pair, according to the task metric. *)

  val rate : piece
  (** Transfer rate for byte-count tasks. *)

  val elapsed : piece
  (** Time elapsed since the task started. *)

  val text : string -> piece
  (** [text s] is a sanitized literal separator. *)

  val accent : string -> piece
  (** [accent s] is sanitized literal text in the task accent colour. *)

  val spacer : piece
  (** Flexible space that pushes all following pieces to the right edge. *)

  type t
  (** A task-line layout and accent colour. *)

  val v : ?color:color -> piece list -> t
  (** [v pieces] is a layout assembled left to right.

      {b Raises.} [Invalid_argument] if [pieces] contains more than one label or
      spacer. *)

  val with_color : color -> t -> t
  (** [with_color color line] changes [line]'s accent colour. *)

  val default : t
  (** The default label, bar, count, rate and right-aligned elapsed layout. *)
end

(** {1 Rendering context} *)

type mode = [ `Auto | `Activity | `History ]
(** [`Activity] renders permanent history plus a bounded projection of running
    tasks. [`History] is strictly append-only. [`Auto] selects [`Activity] on a
    terminal and [`History] otherwise. *)

type ctx
(** A display-start capability. It is deliberately not obtainable from an active
    display; ordinary Eio applications let [Console_eio.run] construct it. *)

val ctx :
  ppf:Format.formatter ->
  now:(unit -> float) ->
  dimensions:(unit -> int * int) ->
  is_tty:bool ->
  unit ->
  ctx
(** [ctx] constructs a display context from explicit output, clock, geometry and
    terminal detection functions. Driver libraries normally do this. *)

val ctx_ppf : ctx -> Format.formatter
(** [ctx_ppf ctx] is the formatter [ctx] draws on. A wrapper that hands [ctx] to
    {!run} and then writes to the same surface itself -- the newline that closes
    a line the session left open, say -- reads it here rather than being told
    twice, which is the arrangement in which the two answers can differ. *)

val with_char_width : ctx -> (Uchar.t -> int) -> ctx
(** [with_char_width ctx width] overrides Unicode terminal-cell measurement. *)

val dumb : unit -> ctx
(** [dumb ()] is an append-only 80 by 24 context with a zero clock. *)

(** {1 Immutable event history} *)

module Event : sig
  type level = [ `Info | `Warning | `Error ]
  type outcome = [ `Succeeded | `Failed | `Cancelled ]
  type result = [ `Ok | `Fail ]

  type kind =
    [ `Message of level
    | `Started
    | `Log of level
    | `Finished of outcome
    | `Result of result ]

  val equal_outcome : outcome -> outcome -> bool
  (** [equal_outcome a b] is [true] when [a] and [b] are the same outcome. *)

  type t

  val elapsed : t -> float
  (** Seconds since the display session started. *)

  val task : t -> int option
  (** The numeric task identity, or [None] for a session-level event. *)

  val key : t -> string option
  (** The caller-supplied stable task ID, when there is one. *)

  val path : t -> string list
  (** Task labels from the root task through the event's owning task. *)

  val kind : t -> kind
  (** The typed event kind. *)

  val message : t -> string
  (** The sanitized event message. *)

  val detail : t -> string option
  (** Additional result detail, present only when supplied to {!set_result}. *)

  val pp : t Fmt.t
  (** Pretty-printer for diagnostics. *)
end

(** {1 Immutable activity projection} *)

module Activity : sig
  type progress = [ `Spin | `Count of int * int | `Bytes of int64 * int64 ]

  val equal_progress : progress -> progress -> bool
  (** [equal_progress a b] is [true] when [a] and [b] describe the same metric.
  *)

  type log

  val log_elapsed : log -> float
  (** Seconds from display start to this log entry. *)

  val log_level : log -> Event.level
  (** The log severity. *)

  val log_message : log -> string
  (** The sanitized log message. *)

  type item

  val id : item -> int
  (** Stable numeric task identity. *)

  val key : item -> string option
  (** Caller-provided stable identity, when present. *)

  val parent : item -> int option
  (** Numeric identity of the parent task, when present. *)

  val label : item -> string
  (** Current task label. *)

  val progress : item -> progress
  (** Current progress metric. *)

  val elapsed : item -> float
  (** Seconds elapsed since this task started. *)

  val logs : item -> log list
  (** Bounded live log tail, oldest first. *)

  type t

  val items : t -> item list
  (** Running task items in display order. *)

  val pp : t Fmt.t
  (** Pretty-printer for diagnostics. *)
end

(** {1 Scoped sessions} *)

type t
(** A display session. Values are created only by {!run}. *)

val pp : t Fmt.t
(** Pretty-printer for display diagnostics. *)

val run :
  ctx:ctx ->
  ?mode:mode ->
  ?theme:Theme.t ->
  ?palette:color list ->
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

    Calls compose: a nested [run] on the same domain receives the existing
    display, and only the outermost call applies configuration and teardown.
    Across domains an activity display remains exclusive. A history display may
    run concurrently only on a different formatter; sharing an owned formatter
    is rejected before output can corrupt the activity region. *)

val restore_terminal : unit -> unit
(** [restore_terminal ()] shows the cursor on the formatter of the open activity
    projection, which hid it, and flushes; without one it does nothing. {!run}
    gives the terminal back itself on return and on an exception. A signal
    handler that ends the process under the signal's default disposition is the
    one exit {!run} never sees, and calls this before it re-raises. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output display f] temporarily removes the activity projection and
    passes [f] the owned formatter, configured to the current terminal width. It
    then redraws the current snapshot. It is intended for prompts,
    formatter-based widgets and third-party commands that cannot emit through
    {!event} or {!log}. Calls may be nested. History mode simply calls [f].

    Prefer {!event} for ordinary messages: it records their semantics as well as
    printing them. The display must be open and must own the activity output. *)

type task
(** A running or completed unit of work owned by one display. *)

val task :
  ?id:string ->
  ?parent:task ->
  ?bar:Line.t ->
  ?color:color ->
  ?log_lines:int ->
  ?total:int ->
  ?transient:bool ->
  t ->
  string ->
  task
(** [task display label] starts a task. With [~id], repeated calls return the
    same task, so event streams can use their native stable IDs without keeping
    a registry. The first call fixes its parent and rendering options; use
    {!set_label} when a later start event supplies a better label.

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

    {b Raises.} [Invalid_argument] if the display is closed, [id] is empty,
    [log_lines] or [total] is negative, or [parent] belongs to another display.
*)

val set_label : task -> string -> unit
(** [set_label task label] atomically changes the live task label. *)

val set_count : task -> cur:int -> total:int -> unit
(** [set_count task ~cur ~total] records monotonic count progress. Estimates
    expand automatically when [cur] exceeds [total]. *)

val advance : ?by:int -> ?total:int -> ?label:string -> task -> unit
(** [advance task] increments count progress by one. [by] selects another
    non-negative increment, [label] updates the activity label in the same
    repaint, and [total] supplies a new estimate. The known total expands
    automatically if progress passes it. An indeterminate task becomes a count
    task on its first advance. *)

val set_bytes : task -> cur:int64 -> total:int64 -> unit
(** Counts and byte counts must be non-negative; a [cur] beyond a provisional
    [total] expands the total automatically. Progress is monotonic: an older
    update cannot move a task backwards or reduce its known total. Updates after
    completion are harmless no-ops. A task cannot switch between count and byte
    units. *)

val log : ?level:Event.level -> task -> string -> unit
(** [log task text] appends each line permanently to {!history}; the latest
    lines also appear in the task's bounded activity tail. Logs arriving after
    completion remain attached to the completed task in permanent history. *)

val event : ?level:Event.level -> t -> string -> unit
(** [event display text] appends permanent session-level messages, one per input
    line. Physical line breaks are preserved. Use this instead of writing
    directly while an activity projection owns the output. *)

val succeed : ?message:string -> task -> unit
(** Finish a task successfully, optionally replacing its committed label. *)

val fail : ?message:string -> task -> unit
(** Finish a task as failed, optionally replacing its committed label. *)

val cancel : ?message:string -> task -> unit
(** Outcomes are idempotent: the first outcome wins and duplicates are no-ops.
    Finishing a task first settles unfinished descendants children-first:
    success succeeds them, while failure or cancellation cancels them. *)

val with_task :
  ?id:string ->
  ?parent:task ->
  ?bar:Line.t ->
  ?color:color ->
  ?log_lines:int ->
  ?total:int ->
  ?transient:bool ->
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
  t -> ?detail:string -> [ `Ok of string | `Fail of string ] -> unit
(** [set_result display result] settles remaining tasks, records the final
    verdict as a permanent semantic event, and emits it when {!run} closes.
    [`Ok] succeeds remaining work; [`Fail] cancels it. It is exactly-once; a
    later call is a harmless no-op. *)

val tick : t -> unit
(** [tick display] refreshes an activity projection. Driver libraries call it
    periodically; it is a no-op after the session closes.

    A task update repaints the projection when the frame on screen is 100 ms old
    or older, and otherwise changes the snapshot and leaves the paint to the
    next update past that age or to [tick], so a producer reporting every few
    kilobytes writes the terminal at the eye's pace and not its own. Permanent
    events paint at once. *)
