(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Progress rows for work that takes time.

    A display shows one row for each task still running, with a spinner or a
    progress bar, and redraws those rows in place: they are its {e live region},
    at the bottom of the terminal's normal output. When a task ends, or logs a
    line, that line is printed once above the region and stays in the
    scrollback. When the output is not a terminal, such as a pipe or a CI log,
    nothing is redrawn: each of those lines is printed in turn, and the header
    comes last, as a summary. Each starts a line of its own: when what the
    program wrote on the same formatter did not end its line, a line feed comes
    first.

    [Console_eio.run] opens a display, passes it to a function and closes it
    when the function returns; most programs need nothing else.

    {[
    let build display =
      let step = Console.Display.task display "compile" in
      Console.Display.set_count step ~cur:1 ~total:2;
      Console.Display.log step "compiled parser.ml";
      Console.Display.succeed step
    ]}

    The lines a display prints start with a mark ({!Theme.val-mark}) that says
    what they are: how a task ended, the level of a log line, and in the header
    whether work is still running and how it ended.

    {1:guarantees Guarantees}

    On a terminal of [w] columns and [h] rows:
    - no line the display draws in its region is wider than [w], so none wraps,
      and the region is never taller than [h - 1] lines;
    - the region starts at the first column of a row of its own, below a line
      the program left unended before the display started;
    - each change is written in one piece, as one synchronized update (mode
      2026) that a terminal supporting it shows whole: the region is never shown
      half erased or half drawn, and only the region's rows change between two
      of its frames; on a pipe no such escape is written;
    - a redraw that would change nothing writes nothing;
    - a label cut to fit is cut between two characters, never inside an escape
      code, ends with an ellipsis and turns its style off;
    - the counts, rates and timers of all rows sit in the same columns, whatever
      the width of the labels: at the right edge of the first [min w 100]
      columns, so that on a wide terminal they stay near their labels and do not
      move when it is resized wider;
    - after a resize the old frame is erased, counting the rows a terminal that
      reflows its lines gives it;
    - the cursor is hidden while the display runs and shown again when it
      closes, and every line the display prints ends with ["\r\n"], so a tty
      with output processing off shows the same screen as one with it on. *)

(** {1:layout Row layout} *)

(** The pieces of a running task's row. *)
module Line : sig
  type piece
  (** The type for the pieces of a row. *)

  val spinner : piece
  (** [spinner] is the spinner. *)

  val label : piece
  (** [label] is the task's label. It takes the room the other pieces leave, and
      is cut to it with an ellipsis. *)

  val bar : int -> piece
  (** [bar width] is a progress bar [width] cells wide ({!Bar}).

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
      [1h02m], right-aligned in a column of its own. *)

  val text : string -> piece
  (** [text s] is [s] on one line. *)

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
  (** [default] is a spinner, the label, a spacer, a bar, the count, the rate
      and the elapsed time. *)
end

(** {1:displays Displays} *)

type t
(** The type for displays. *)

val v :
  ?layout:Line.t ->
  ?header:string ->
  ?size:(unit -> int * int) ->
  now:(unit -> float) ->
  Format.formatter ->
  t
(** [v ~now ppf] is a display writing on [ppf] and timed by the clock [now], in
    seconds.

    - [size], if given, says that [ppf] is a terminal and gives its width and
      height, read again before each redraw: the display hides the cursor and
      redraws its rows in place. Without it the display prints lines in turn.
    - [header], if given, names the work on a line above the rows.
    - [layout] (defaults to {!Line.default}) gives the pieces of each row.

    The display is drawn in [ppf]'s theme ({!Theme.get}), read again each time
    it draws, and in colour iff [ppf]'s style renderer is [`Ansi_tty], at
    [ppf]'s {!Color.val-depth}. *)

val redraw : t -> unit
(** [redraw display] draws the region again, so that the spinners turn and the
    timers run. A driver calls it a few times a second; the display redraws
    itself when its tasks change, at most ten times a second. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output display f] takes the region off the screen, calls [f] with the
    display's formatter, as wide as the terminal, and draws the region again
    below what [f] printed, also when [f] raises. Output that [f] does not end
    with a line feed is ended. *)

val message : ?level:[ `Info | `Warning | `Error ] -> t -> string -> unit
(** [message display text] prints each line of [text] above the region, after
    the mark of [level] unless [level] is [`Info] (the default). *)

val set_scene : t -> (height:int -> Format.formatter -> unit) -> unit
(** [set_scene display scene] draws what [scene ~height ppf] prints above the
    header each time the region is drawn, in place of the scene set before.
    [ppf] is in the theme and the UTF-8 of [display]'s formatter, and its margin
    is the width of the rows, so that a {!Table.pp} or a {!Panel.pp} printed on
    it fits. A span printed on [ppf] keeps its styles, and other text is plain.
    [height] is the number of lines the header and the rows leave: the lines
    after them are not drawn, and a line wider than the rows is cut.

    While [display] draws in colour, a frame drawn in a theme with a
    {!Theme.val-motion} moves, by the time since [display] started; on a
    terminal without colour it stands still. Without a terminal the scene is not
    drawn, and once [display] closes it is gone: it shows what the program also
    prints, such as a table it prints when the work is done.

    {[
    let watch display results =
      Console.Display.set_scene display (fun ~height:_ ppf ->
          Console.Table.pp ppf (results ()))
    ]} *)

val set_result :
  ?detail:string -> t -> [ `Ok of string | `Fail of string ] -> unit
(** [set_result display result] says how the work ended: when [display] closes,
    it prints [result]'s message after a success or failure mark, and [detail]
    (if any) in the muted colour above it. With [`Ok] the tasks still running
    succeed, with [`Fail] they are cancelled. A second call does nothing. *)

val close : ?running:[ `Succeed | `Cancel ] -> t -> unit
(** [close display] ends the tasks still running, as [running] says (defaults to
    [`Cancel]: a task nobody ended did not finish; {!set_result} with [`Ok] has
    them succeed first), takes the region off the screen, shows the cursor and
    prints the header, then the result of {!set_result}. Closing a closed
    display does nothing. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics, in an unspecified format. *)

(** {1:tasks Tasks}

    Every function below, and {!redraw}, {!with_output}, {!message},
    {!set_scene} and {!set_result}, raises [Invalid_argument] when the display
    has closed. A task that ended ignores what it is told, but its {!log}. *)

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

    - [key], if given, names the task: while it runs, a later call with the same
      [key] returns the same task, wherever it is called from.
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
(** [log task text] prints each line of [text] as a line of [task], after the
    mark of [level] (defaults to [`Info]). Its latest lines show under the
    task's row while the task runs, and each one is printed above the region
    once it leaves it. Without a terminal, each line is printed at once, after
    the label of its task. A line that does not fit the terminal under the row
    is cut with an ellipsis.

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
