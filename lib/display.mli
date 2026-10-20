(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Progress rows for work that takes time.

    A display shows one row for each task still running, with a spinner or a
    progress bar, and redraws those rows in place: they are its {e live region},
    at the bottom of the terminal's normal output. When a task ends, its line is
    printed once above the region and stays in the scrollback. When the output
    is not a terminal, such as a pipe or a CI log, nothing is redrawn: a task
    prints a line when it starts and one when it ends, and the header comes
    last, as a summary. Each starts a line of its own: when what the program
    wrote on the same formatter did not end its line, a line feed comes first.

    [Console_eio.run] opens a display, passes it to a function and closes it
    when the function returns; most programs need nothing else.

    {[
    let build display =
      Console.Display.with_task display "compile" (fun step ->
          Console.Display.set_count step ~cur:1 ~total:2;
          Console.Display.log step "ocamlopt parser.ml";
          Console.Display.set_count step ~cur:2 ~total:2)
    ]}

    {1:output Where text goes}

    A task's output shows under it while it runs, and stays if the task fails.

    - Output outside any task, or within {!root}, goes above the rows.
    - On a pipe, each line is printed at once, after its task's name.
    - {!within} makes a task current again, from another fiber say, and {!log}
      gives it a line from where it is not.

    Output is what the program prints while a task is current ({!tasks}): its
    {!log} lines, its [Logs] messages and, under [Console_eio.run], the lines it
    prints with [Fmt.pr] or [Fmt.epr] on the display's terminal, without their
    escape codes. A task that succeeds leaves its line alone; one that fails or
    is cancelled prints its output under its line, up to 1 MiB, its oldest lines
    dropped first and counted on a line of their own. A line too wide for the
    terminal is cut under a running task and nowhere else. [Console_eio.run]
    sees standard output through {!Fmt.stdout} and [Logs] messages through its
    reporter: what [print_string], [Printf] or a child process writes on the
    terminal is not seen and garbles the rows, so print it with {!with_output}.
    Without a terminal, console flushes {!Fmt.stdout} before each line it
    writes, so that [2>&1] keeps the order of the two outputs. A write error on
    the display's output, such as a reader that closed its pipe in
    [prog 2>&1 | head -1], ends the display's output and nothing else. One on
    standard output is the program's: console stops flushing it, and the
    program's own writes meet the error as they would without console.

    What the program prints outside a task goes out as it is, escape codes
    included: console does not read it, so that the terminal shows what a pipe
    receives, colours, tabs and hyperlinks too. Bytes that move the cursor or
    erase, such as a carriage return, a move up or a clear, act on the lines
    above the region, which console does not draw again; the region is drawn
    below wherever they leave the cursor, from the first column, on rows it
    erases first. A mode they set, such as a scroll region, the alternate screen
    or a shown cursor, stays set.

    The lines a display prints start with a mark ({!Theme.val-mark}) that says
    what they are: how a task ended, a line of its output, the level of a [Logs]
    message, and in the header whether work is still running and how it ended.

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
module Layout : sig
  type piece = Display_layout.piece
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

  type t = Display_layout.t
  (** The type for row layouts. *)

  val v : piece list -> t
  (** [v pieces] is the layout of [pieces], left to right.

      @raise Invalid_argument
        if [pieces] has more than one {!label} or {!spacer}. *)

  val default : t
  (** [default] is a spinner, the label, a spacer, a bar, the count, the rate
      and the elapsed time. *)
end

(** {1:displays Displays}

    A display belongs to the domain that runs it: call its functions, and those
    of its tasks, from that domain alone. [Console_eio.run] passes it the [Logs]
    messages logged in the other domains Eio runs, such as those of an
    [Eio.Executor_pool], and they go above the rows. *)

type t = Display_state.t
(** The type for displays. *)

val v :
  ?layout:Layout.t ->
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
    - [layout] (defaults to {!Layout.default}) gives the pieces of each row.

    The current task is that of the innermost {!with_task} that runs; under
    [Console_eio.run], that of the fiber that asks or of the fibers it was
    forked from.

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
    with a line feed is ended, and the region is drawn from the first column of
    the row where [f] left the cursor, on rows it erases first. This prints what
    console does not see, such as the text of [print_string] or of a child
    process. *)

val set_scene : t -> (height:int -> Format.formatter -> unit) -> unit
(** [set_scene display scene] draws what [scene ~height ppf] prints above the
    header each time the region is drawn, in place of the scene set before.
    [ppf] is in the theme and the UTF-8 of [display]'s formatter, and its margin
    is the width of the rows, so that a {!Table.pp} or a {!Panel.pp} printed on
    it fits. A span printed on [ppf] keeps its styles, and other text is plain.
    [height] is the number of lines the header and the rows leave: the lines
    after them are not drawn, and a line wider than the rows is cut. A [scene]
    that raises is dropped: the call that was drawing it, such as {!redraw} or
    {!set_count}, draws the region without it, then raises the exception again.

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

val set_result : ?detail:string -> t -> (string, string) result -> unit
(** [set_result display result] says how the work ended: when [display] closes,
    it prints [result]'s message after a success or failure mark, and [detail]
    (if any) in the muted colour above it. With [Ok] the tasks still running
    succeed, with [Error] they are cancelled. A second call does nothing. *)

val close : t -> unit
(** [close display] cancels the tasks still running, for a task nobody ended did
    not finish ({!set_result} with [Ok] has them succeed first), takes the
    region off the screen, shows the cursor and prints the header, then the
    result of {!set_result}.

    A closed display ignores what it is told, so that a fiber that outlives it
    does not fail: closing it again, {!redraw}, {!set_scene} and {!set_result}
    do nothing, and [with_output display f] is [f ppf], [ppf] being the
    formatter of {!v}. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics, in an unspecified format. *)

(** {1:tasks Tasks}

    A task is {e current} in the function given to {!with_task} and, with the
    scope of [Console_eio.run], in the fibers that function starts: a task
    started there goes under it, and what the program prints there is its output
    ({!output}).

    A task that ended ignores what it is told. Once its display closed, every
    task has ended, and so has one {!val-task} starts then. *)

type task = Display_state.row
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
    work it has. Without a terminal it prints a line as it starts.

    - [key], if given, names the task: while it runs, a later call with the same
      [key] returns the same task, wherever it is called from, and the [label],
      [parent], [log_lines] and [transient] of that call are ignored.
    - [parent] (defaults to the current task, if any) draws the task under
      [parent]. A task started under a parent that ended takes the parent's
      ending.
    - [log_lines] is how many of the task's latest lines its window shows under
      its row while it runs (defaults to [8]).
    - [transient] (defaults to [false]) makes a task that leaves no line of its
      own when it ends, nor any of its output: it shows while it runs, and that
      is all.

    @raise Invalid_argument
      if [key] is empty, [log_lines] is negative, or [parent] belongs to another
      display. *)

val with_task :
  ?key:string ->
  ?parent:task ->
  ?log_lines:int ->
  ?transient:bool ->
  t ->
  string ->
  (task -> 'a) ->
  'a
(** [with_task display label f] is [f task], for a [task] started as {!val-task}
    starts it and current while [f] runs. The task succeeds when [f] returns and
    fails when [f] raises, and the exception is raised again; if [f] ended the
    task, that ending counts. When [key] names a task that runs, [with_task] is
    [within task (fun () -> f task)] for that task: it ends nothing, and the
    task ends as the call that started it says. *)

val within : task -> (unit -> 'a) -> 'a
(** [within task f] is [f ()] with [task] current while [f] runs, and with the
    scope of [Console_eio.run] in the fibers [f] starts: what [f] prints is
    [task]'s output, and a task [f] starts goes under [task]. Where {!with_task}
    starts a task, [within] enters one that exists, and ends nothing, also when
    [f] raises: [task] ends as it is told. *)

val root : t -> task
(** [root display] is the top task of [display], current where no other is: its
    output goes above the rows at once and stays, and a task started within it
    has no parent. [within (root display) f] prints what [f] prints above the
    rows, from inside a task too. The root never ends: {!succeed}, {!fail},
    {!cancel}, {!set_label}, {!set_count} and {!set_bytes} do nothing on it, as
    on a task that ended, so that code given a task needs no case for it. *)

val iter : t -> string -> ('a -> unit) -> 'a list -> unit
(** [iter display label f l] applies [f] to the elements of [l] in turn, inside
    a {!with_task} named [label] whose count is the elements done out of the
    length of [l]. *)

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

val log : task -> string -> unit
(** [log task text] gives each line of [text] to [task] as its output
    ({!output}), such as the lines of a command it runs: the latest show in
    [task]'s window while it runs, and all of them under its line if it fails or
    is cancelled. It is the one-line form of {!within}, for code where [task] is
    not current. Without a terminal each line is printed at once, after [task]'s
    label, and under the line of a failure the lines of the window are printed
    again. *)

val succeed : ?message:string -> task -> unit
(** [succeed task] ends [task] as a success, with [message] in place of its
    label if given. A task ends once: the first of {!succeed}, {!fail} and
    {!cancel} counts and the others do nothing. The tasks under it that still
    run end first, each with a line of its own: they succeed with it, and are
    cancelled when it fails or is cancelled. *)

val fail : ?message:string -> task -> unit
(** [fail task] is like {!succeed} but ends [task] as a failure. *)

val cancel : ?message:string -> task -> unit
(** [cancel task] is like {!succeed} but ends [task] as cancelled. *)
