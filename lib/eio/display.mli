(** Eio driver for a {!Console.Display}: an animation loop, a task tree, and log
    routing.

    {!run} owns a display for the duration of a function -- animating it,
    finalising it with a result banner, and tearing it down on failure -- while
    {!task} describes the tree of work beneath it. Log lines reach a task either
    explicitly with {!log}, through a {!Logs} reporter that routes by a {!tag},
    or by piping a stream ({!pipe}) or tailing a growing file ({!follow}) into
    its window. *)

val animate :
  sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> Console.Display.t -> unit
(** [animate ~sw ~clock d] forks a daemon fiber on [sw] that calls
    {!Console.Display.tick} on [d] every 120 ms, so timers and spinners keep
    moving with no new events. The daemon stops when [sw] finishes; ticking a
    torn-down display is a no-op. {!run} calls this for you. *)

type t
(** A running display: the underlying {!Console.Display.t}, the default
    log-window size for its tasks, and the result set during the run. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] writes a short representation of [t] to [ppf]. *)

type task = Console.Display.row
(** A node in the tree -- a {!Console.Display.row}. *)

val run :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  ?ppf:Format.formatter ->
  ?mode:Console.Display.mode ->
  ?log_lines:int ->
  ?theme:Console.Theme.t ->
  ?header:string ->
  (t -> 'a) ->
  'a
(** [run ~sw ~clock f] creates a display (forwarding [ppf], [mode], [theme] and
    [header] to {!Console.Display.v}), forks {!animate} on [sw], and runs [f t].
    [log_lines] (default [8]) is the default log-window size handed to {!task}.

    On normal return it finalises with {!Console.Display.finish_all}, adding the
    verdict banner set through {!set_result} if any, and returns [f]'s value. If
    [f] raises, it {!Console.Display.clear}s the live region and re-raises, so
    the caller's handler reports the failure; a {e handled} failure should call
    {!set_result} with [`Fail] and return so the banner shows it. *)

(** {1 The task tree} *)

val task : ?parent:task -> ?log_lines:int -> t -> string -> task
(** [task t label] adds a task to the tree and starts its timer. [parent] nests
    it as a sub-step; [log_lines] overrides the run's default window size. *)

val finish : ?live:bool -> ?error:bool -> ?message:string -> task -> unit
(** [finish task] is {!Console.Display.finish}: freeze the task's timer and
    commit it. [~live:true] keeps it visible under its parent, [~error:true]
    flips the final summary marker to a failure. *)

val set_result :
  t -> ?detail:string -> [ `Ok of string | `Fail of string ] -> unit
(** [set_result t result] records the verdict {!run} shows on completion: a
    green [`Ok msg] or red [`Fail msg] banner, with [detail] as a dimmed
    paragraph above it. The last call wins. *)

(** {1 Feeding logs to a task} *)

val log : task -> string -> unit
(** [log task s] appends [s] (split on newlines) to [task]'s log window. *)

val tag : task -> Logs.Tag.set
(** [tag task] is a {!Logs} tag set naming [task], so a log call routed through
    {!reporter} lands in [task]'s window -- pass it as the [~tags] of the log
    call, e.g. [Logs.app (fun m -> m ~tags:(tag step) "compiling foo.ml")]. *)

val add_tag : task -> Logs.Tag.set -> Logs.Tag.set
(** [add_tag task set] adds the {!tag} for [task] to an existing tag set. *)

val reporter : t -> Logs.reporter
(** [reporter t] is a {!Logs} reporter that appends each message to the window
    of the task named by its {!tag}, or to a top-level [logs] task when the
    message carries no task tag. Install it with [Logs.set_reporter]. *)

val pipe : sw:Eio.Switch.t -> task -> _ Eio.Flow.source -> unit
(** [pipe ~sw task src] forks a fiber on [sw] that reads [src] line by line into
    [task]'s window until [src] reaches end-of-file -- e.g. the standard output
    of a sub-process, or a socket. *)

val follow :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  ?poll:float ->
  task ->
  _ Eio.Path.t ->
  unit
(** [follow ~sw ~clock task path] forks a daemon fiber on [sw] that tails the
    file at [path] into [task]'s window, polling every [poll] seconds (default
    [0.2]) for bytes appended since the last read and emitting only whole lines.
    A file created or replaced after the call is picked up on the next poll. The
    daemon stops when [sw] finishes. Truncation or rotation of the file is not
    tracked. *)

val underlying : t -> Console.Display.t
(** [underlying t] is the wrapped display, for the lower-level controls -- byte
    counters, bar styles, row removal -- not surfaced here. *)
