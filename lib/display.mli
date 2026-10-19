(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Progress rows for work that takes time.

    A display shows one row for each task still running, with a spinner or a
    progress bar, and redraws those rows in place. When a task ends, or logs a
    line, that line is printed once above the rows and stays on the screen. When
    the output is not a terminal, such as a pipe or a CI log, nothing is
    redrawn: each of those lines is printed in turn, and the header line comes
    last, as a summary.

    [Console_eio.run] opens a display and passes it to a function; the display
    closes when the function returns.
    {b Output during a display goes through {!with_output}}, so that it is not
    mixed with the rows being redrawn.

    {[
    let build display =
      let step = Console.Display.task display "compile" in
      Console.Display.set_count step ~cur:1 ~total:2;
      Console.Display.log step "compiled parser.ml";
      Console.Display.succeed step
    ]}

    The lines a display prints start with a mark that says what they are:

    - [=>], [!!] and [--] for a task that succeeded, failed or was cancelled;
    - [#], [!] and [x] for an information, a warning and an error line;
    - in the header, [[-]] while tasks run, then [[+]], [[x]] or [[~]] when all
      succeeded, one failed or one was cancelled, followed by the count of tasks
      that ended out of those started, such as [(2/3)]. *)

(** {1:layout Row layout} *)

(** The pieces of a running task's row. *)
module Line : sig
  type piece
  (** The type for the pieces of a row. *)

  val spinner : piece
  (** [spinner] is the spinner, and the task's ending mark once it ends. *)

  val label : piece
  (** [label] is the task's label. It takes the room the other pieces leave, and
      loses words from its end when there is not enough. *)

  val bar : int -> piece
  (** [bar width] is a progress bar [width] cells wide.

      @raise Invalid_argument if [width] is negative. *)

  val percent : piece
  (** [percent] is the share of the work done, as a percentage. *)

  val count : piece
  (** [count] is the count of items or bytes done and the total, such as [3/10].
      The counts of all rows are right-aligned in one column. *)

  val rate : piece
  (** [rate] is the bytes per second of a task that counts bytes. *)

  val elapsed : piece
  (** [elapsed] is the time since the task started, such as [6.4s], [1m05s] or
      [1h02m]. *)

  val text : string -> piece
  (** [text s] is [s] as it is. *)

  val accent : string -> piece
  (** [accent s] is [s] in the row's colour. *)

  val spacer : piece
  (** [spacer] is the space that pushes the pieces after it to the right edge.
  *)

  type t
  (** The type for row layouts. *)

  val v : piece list -> t
  (** [v pieces] is the layout of [pieces], left to right.

      @raise Invalid_argument
        if [pieces] has more than one {!label} or {!spacer}. *)

  val default : t
  (** [default] is a spinner, the label, a bar, the count, the rate and the
      elapsed time. *)
end

(** {1:displays Displays} *)

type t
(** The type for displays. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output display f] takes the rows off the screen, calls [f] with the
    display's formatter, as wide as the terminal, and draws the rows again below
    what [f] printed. Ask questions ([Console_eio.Prompt]) and print widgets
    this way while the display runs. *)

val set_result :
  ?detail:string -> t -> [ `Ok of string | `Fail of string ] -> unit
(** [set_result display result] says how the work ended: when [display] closes,
    it prints [result]'s message after a success or failure mark, and [detail]
    (if any) dim above it. With [`Ok] the tasks still running succeed, with
    [`Fail] they are cancelled. A second call does nothing. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics, in an unspecified format. *)

(** {1:tasks Tasks}

    Every function below raises [Invalid_argument] when the display has closed.
*)

type task
(** The type for tasks. *)

val task :
  ?key:string ->
  ?parent:task ->
  ?log_lines:int ->
  ?transient:bool ->
  t ->
  string ->
  task
(** [task display label] starts a task named [label] (a line feed in it is a
    space). It shows a spinner until {!set_count} or {!set_bytes} says how much
    work it has.

    - [key], if given, names the task: a later call with the same [key] returns
      the same task, wherever it is called from.
    - [parent], if given, draws the task under [parent]. A task started under a
      parent that ended takes the parent's ending.
    - [log_lines] is how many of the task's latest {!log} lines are shown under
      its row while it runs (defaults to [8]).
    - [transient] (defaults to [false]) makes a task that leaves no line when it
      ends: it shows while it runs, and that is all.

    @raise Invalid_argument
      if [key] is empty, [log_lines] is negative, or [parent] belongs to another
      display. *)

val set_label : task -> string -> unit
(** [set_label task label] renames [task] to [label]. *)

val set_count : task -> cur:int -> total:int -> unit
(** [set_count task ~cur ~total] says that [cur] items of [total] are done. A
    count never goes back: a smaller [cur] or [total] than before is ignored,
    and a [cur] above [total] raises the total.

    @raise Invalid_argument
      if [cur] or [total] is negative, or [task] counts bytes. *)

val set_bytes : task -> cur:int64 -> total:int64 -> unit
(** [set_bytes task ~cur ~total] is like {!set_count} for a task that counts
    bytes, whose row also shows the bytes per second.

    @raise Invalid_argument
      if [cur] or [total] is negative, or [task] counts items. *)

val log :
  ?level:[ `Info | `Warning | `Error ] -> ?keep:bool -> task -> string -> unit
(** [log task text] prints each line of [text] as a line of [task], marked by
    [level] (defaults to [`Info]). Its latest lines show under the task's row
    while the task runs, and each one is printed above the rows once it leaves
    them. A line that does not fit the terminal under the row loses words from
    its end.

    With [~keep:false] (defaults to [true]) a line only shows under the row and
    is never printed above it, for output that is only worth watching while it
    comes. *)

val succeed : ?message:string -> task -> unit
(** [succeed task] ends [task] as a success, with [message] in place of its
    label if given. A task ends once: the first of {!succeed}, {!fail} and
    {!cancel} counts and the others do nothing. The tasks under it that still
    run end first: they succeed with it, and are cancelled when it fails or is
    cancelled. *)

val fail : ?message:string -> task -> unit
(** [fail task] is like {!succeed} but ends [task] as a failure. *)

val cancel : ?message:string -> task -> unit
(** [cancel task] is like {!succeed} but ends [task] as cancelled. *)

(**/**)

type ctx
(** The type for where a display draws. *)

val ctx :
  ?depth:Color.depth ->
  ppf:Format.formatter ->
  now:(unit -> float) ->
  wait:(float -> unit) ->
  dimensions:(unit -> int * int) ->
  live:bool ->
  unit ->
  ctx
(** [ctx ~ppf ~now ~wait ~dimensions ~live ()] is where a display draws: on
    [ppf], with the clock [now] and [wait], in a terminal of [dimensions], with
    rows redrawn in place iff [live], in colours of [depth] (defaults to
    [`True_color]). A driver such as [Console_eio], and the tests, make it. *)

val run :
  ctx:ctx ->
  ?theme:Theme.t ->
  ?layout:Line.t ->
  ?header:string ->
  (t -> 'a) ->
  'a
(** [run ~ctx f] is [f display] for a display on [ctx]. When [f] returns, the
    tasks still running succeed; when it raises, they are cancelled and the
    exception is raised again.

    @raise Invalid_argument if a display is already running. *)

val event : ?level:[ `Info | `Warning | `Error ] -> t -> string -> unit
(** [event display text] prints each line of [text] above the rows. *)

val tick : t -> unit
(** [tick display] redraws the rows. *)

val now : t -> float
(** [now display] is the time on [display]'s clock. *)

val wait : t -> float -> unit
(** [wait display s] returns once [display]'s clock has moved by [s]. *)

val restore : unit -> unit
(** [restore ()] shows the cursor again if a display hid it. *)

module Event : sig
  type level = [ `Info | `Warning | `Error ]
  (** The type for levels. *)

  type outcome = [ `Succeeded | `Failed | `Cancelled ]
  (** The type for task endings. *)

  type result = [ `Ok | `Fail ]
  (** The type for results. *)

  type kind =
    [ `Message of level
    | `Started
    | `Log of level
    | `Finished of outcome
    | `Result of result ]
  (** The type for what an event says. *)

  type t
  (** The type for events. *)

  val elapsed : t -> float
  (** [elapsed e] is the seconds from the display's start to [e]. *)

  val task : t -> int option
  (** [task e] is the number of the task of [e], if any. *)

  val key : t -> string option
  (** [key e] is the key of the task of [e], if any. *)

  val path : t -> string list
  (** [path e] are the labels of the task of [e] and of those above it. *)

  val kind : t -> kind
  (** [kind e] is what [e] says. *)

  val message : t -> string
  (** [message e] is the text of [e]. *)

  val detail : t -> string option
  (** [detail e] is the detail of a result, if any. *)

  val pp : t Fmt.t
  (** [pp] formats an event for diagnostics. *)
end

module Activity : sig
  type progress = [ `Spin | `Count of int * int | `Bytes of int64 * int64 ]
  (** The type for how far a task is. *)

  type item
  (** The type for running tasks. *)

  val id : item -> int
  (** [id i] is the number of [i]. *)

  val key : item -> string option
  (** [key i] is the key of [i], if any. *)

  val parent : item -> int option
  (** [parent i] is the number of the task above [i], if any. *)

  val label : item -> string
  (** [label i] is the label of [i]. *)

  val progress : item -> progress
  (** [progress i] is how far [i] is. *)

  val elapsed : item -> float
  (** [elapsed i] is the seconds since [i] started. *)

  val logs : item -> Event.t list
  (** [logs i] are the lines shown under [i], oldest first. *)

  type t
  (** The type for the running tasks of a display. *)

  val items : t -> item list
  (** [items a] are the tasks of [a], in the order they started. *)

  val pp : t Fmt.t
  (** [pp] formats running tasks for diagnostics. *)
end

val history : t -> Event.t list
(** [history display] is every event of [display], oldest first. *)

val activity : t -> Activity.t
(** [activity display] is the tasks of [display] still running. *)

(**/**)
