(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Progress rows for work that takes time.

    A display shows the tree of the tasks still running, one row each, with a
    spinner or a progress bar, above a summary line. It redraws them in place,
    at the bottom of the terminal's normal output: they are its {e live region}
    ({!design}). When a top task ends, its tree is printed once above the region
    and stays in the scrollback.

    When the output is not a terminal, such as a pipe or a CI log, nothing is
    redrawn, unless the [`Tty] {!Progress} mode asks for the rows. Instead:
    - a task prints a line when it starts;
    - it prints a line when it ends, with [failed] or [cancelled] at the end
      when it did not succeed;
    - the summary comes last.

    Each of these lines starts on a line of its own, after a line feed when what
    the program wrote on the same formatter did not end its line.

    [Console_eio.run] opens a display, passes it to a function and closes it
    when the function returns, which is all that most programs need:

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
    - On a pipe, each line is printed at once, after its task's path
      ({!design}).
    - {!within} makes a task current again, for example from another fiber, and
      {!log} gives it a line from code where it is not current.

    Output is what the program prints while a task is current
    ({!section-tasks}), without its escape codes:
    - its {!log} lines;
    - under [Console_eio.run], its [Logs] messages and the lines it prints with
      [Fmt.pr] or [Fmt.epr] that [Console_eio.run] takes.

    A task that succeeds leaves its line alone, while one that fails or is
    cancelled prints up to 1 MiB of its output under its line. Beyond that, the
    oldest lines are dropped first and a line of their own counts them. A line
    too wide for the terminal is cut under a running task, and nowhere else. A
    line a task keeps for its window or its failure, and a task's label or
    message, keeps its first 4 KiB, ended with an ellipsis, so that the bounds
    above hold for each task however long its text.

    [Console_eio.run] sees standard output through {!Fmt.stdout} and [Logs]
    messages through its reporter, so what [print_string], [Printf] or a child
    process writes on the terminal goes unseen and garbles the rows. Print such
    text with {!with_output}.

    Without a terminal, console flushes {!Fmt.stdout} before each line it
    writes, so that [2>&1] keeps the order of the two outputs. A write error on
    the display's output ends the display's output and nothing else, as when a
    reader closes its pipe in [prog 2>&1 | head -1]. A write error on standard
    output belongs to the program: console stops flushing it, and the program's
    own writes meet the error as they would without console.

    Console does not read what the program prints outside a task, so it goes out
    as it is, escape codes included, and the terminal shows what a pipe would
    receive, colours, tabs and hyperlinks too.
    - Bytes that move the cursor or erase, such as a carriage return, a move up
      or a clear, act on the lines above the region, which console does not draw
      again.
    - The region is drawn below wherever such bytes leave the cursor, from the
      first column, on rows it erases first.
    - A mode they set, such as a scroll region, the alternate screen or a shown
      cursor, stays set.

    The lines a display prints start with a mark ({!Theme.val-mark}) that says
    what they are: how a task ended, a line of a task's output, or the level of
    a [Logs] message. In the summary, the mark says whether work is still
    running and how it ended.

    {1:design Design}

    On a terminal, a display writes in two places: the {e scrollback}, where a
    line is printed once and never rewritten, and the {e region} below it, which
    it redraws in place. The region is a {e view} ({!views}): a span the program
    may put together from the display's parts, a tree of tasks ({!tree}) and the
    summary line ({!summary}), and any other span, such as a {!Table.to_span}.
    The default view is the tree of every top task, then the summary.

    - The summary is the last line of the default view, so the line that says
      how far the work is stays where the eye left it, below the rows, and it is
      the last line printed when the display closes, after all it counts.
    - A task that ends stays in the region, marked as it ended, under its
      parent, while its top task runs.
    - When a top task ends, it moves to the scrollback with every task under it,
      as its tree: each task once, under its parent, in the order they started,
      and a task that failed or was cancelled followed by its output. Work moves
      by top task so that a task never lands above its parent and the tasks of
      one top task stay together, however their endings interleave.
    - With [keep] ({!v}), no task moves: the tasks that ended stay in the region
      until the display closes, as in a view of trees side by side, where a
      column should not empty while the others run.
    - Output outside any task goes to the scrollback at once, above the region
      ({!output}).
    - In the region, only running rows, their windows and the summary change: a
      task that ended keeps its line as it ended until it leaves the region, or
      folds to fit.
    - The region is never taller than the terminal less one row, so a redraw
      never scrolls the terminal: a view taller than that loses its first lines,
      and a summary at its bottom stays. A widget, such as a table or a panel,
      that this would cut in its height loses all its lines, so that no fragment
      of its frame shows, in a {!Span.hcat} too.
    - A tree too tall for its height first folds the tasks that ended under each
      task into one line, such as [12 done, 2 failed] after the mark of the
      worst ending among them, then the lines of its windows give way, the
      oldest of them first, whichever task's window holds it, then only its
      latest running tasks show, after a line such as [… 3 more running].
    - At close, the tasks still running are cancelled and move as above, then
      the view's last frame is printed once, still, and stays ({!type-view}):
      the summary alone in the default view.
    - The tasks that wait in the region under a task are bounded: past 1 MiB,
      the oldest are dropped, and a line counts them. Each weighs at most its
      output, 1 MiB, and three texts of 4 KiB ({!output}).

    Without a terminal there is no region. The tasks print the lines of
    {!Progress}: one line when a task starts and one when it ends, in the order
    they happen, and at close the view's last frame comes last, once: the
    summary alone in the default view, since every task has printed its lines. A
    terminal's scrollback keeps one line for each task instead, in the order of
    its tree, with the tree's guides.

    The lines of tasks that run at once interleave on a pipe, so no line is
    placed by its indentation: each line a task prints, as it starts, as it ends
    and before each line of its output, names the task by its {e path}, the
    labels from its top task down to it, such as [linux > compile] with a single
    right-pointing angle quotation mark, U+203A, in UTF-8. The lines of tasks
    start at the first column, and a line of output two columns in. With
    [`Quiet], the line of a failure names its path too, and its output follows
    it, two columns in.

    {1:guarantees Guarantees}

    On a terminal of [w] columns and [h] rows:
    - No line the display draws in its region is wider than [w], so none wraps.
      The region is never taller than [h - 1] lines.
    - The region starts at the first column of a row of its own, below any line
      the program left unended before the display started.
    - Each change is written in one piece, as one synchronized update (mode
      2026), which a terminal that supports it shows whole. The region is never
      shown half erased or half drawn, and only its rows change between two of
      its frames. On a pipe no such escape is written.
    - A redraw that would change nothing writes nothing.
    - A label cut to fit is cut between two characters, never inside an escape
      code, and it ends with an ellipsis that turns its style off.
    - While no piece gives way ({!Layout}), the counts, rates and timers of all
      rows sit in the same columns, whatever the width of the labels: at the
      right edge of the first [min w 100] columns. On a wide terminal they stay
      near their labels, and they do not move when the terminal is resized
      wider.
    - After a resize the old frame is erased, counting the rows that a terminal
      which reflows its lines gives it. On one that does not reflow, such as
      xterm or GNU screen, a narrowing below the width of the old lines also
      erases the lines above the region that a reflow would have filled.
    - The cursor is hidden while the display runs, and shown again when it
      closes.
    - Every line the display prints ends with ["\r\n"], so a tty with output
      processing off shows the same screen as one with it on. *)

(** {1:layout Row layout} *)

(** The pieces of a running task's row.

    A layout draws the rows of running tasks in a tree ({!tree}). The other
    lines have pieces of their own, whatever the layout, and no bar, rate or
    percentage shows on them.
    - The line of a task that ended is its mark, its label, its count and its
      elapsed time. Without a terminal, [failed] or [cancelled] follows when the
      task did not succeed.
    - Without a terminal, the line a task prints when it starts is its mark and
      its path ({!design}), and the line it prints when it ends has its path in
      place of its label.

    With the [`Quiet] {!Progress}, only a task that fails or is cancelled prints
    its line. With [`Json] no task does: JSON events take their place.

    {b What gives way.} The label is what tells two rows apart, so it gives way
    last. When a row does not fit its width, its pieces give way in this order:
    + the rate;
    + the bar and the percentage;
    + the count, which leaves the column it shares with the other rows and, for
      bytes, gets shorter, such as [256M/2.0G];
    + the elapsed time.

    Until all of them have, the label keeps 12 cells, or all of it if it is
    shorter. Only then is it cut, with an ellipsis. The line of a task that
    ended gives way in the same order: its count gets shorter, then its time
    goes. In the last frame a label is never cut ({!tree}). *)
module Layout : sig
  type piece = Display_layout.piece
  (** The type for the pieces of a row. *)

  val spinner : piece
  (** [spinner] is the spinner. *)

  val label : piece
  (** [label] is the task's label. It takes the room the other pieces leave,
      once they have given way ({!Layout}), and is cut to it with an ellipsis.
  *)

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
  (** [accent s] is [s] in bold, in the row's colour: the colour of the
      {!Theme.palette} the row takes. *)

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
  (** [default] is a spinner, the label, a spacer, a bar 12 cells wide, the
      count, the rate and the elapsed time, with a space after the spinner and
      between each two of the last four. *)
end

(** {1:displays Displays}

    A display belongs to the domain that runs it, so call its functions, and
    those of its tasks, from that domain alone. [Console_eio.run] passes it the
    [Logs] messages logged in the other domains that Eio runs, such as those of
    an [Eio.Executor_pool], and they go above the rows. *)

type t = Display_state.t
(** The type for displays. *)

type view = t -> width:int -> height:int -> Span.t
(** The type for views: [view display ~width ~height] is the region of [display]
    on a terminal [width] columns wide, at most [height] lines tall ({!views}).

    The {e last frame} is the view drawn once more when the display closes, with
    [height] [max_int], since the scrollback has room for all of it, and without
    a terminal with [width] [80]: a view does not make a line for each line of
    its height. It is printed on a terminal and with [`Plain] alike, and never
    with [`Quiet] or [`Json], which print no view. *)

val v :
  ?layout:Layout.t ->
  ?summary:string ->
  ?view:view ->
  ?keep:bool ->
  ?size:(unit -> int * int) ->
  ?progress:Progress.t ->
  now:(unit -> float) ->
  Format.formatter ->
  t
(** [v ~progress ~now ppf] is a display writing on [ppf] and timed by the clock
    [now], in seconds.

    - [size], if given, says that [ppf] is a terminal and gives its width and
      height. The display reads it again before each redraw, hides the cursor
      and redraws its rows in place. A width or a height is taken in
      \[[1];[4096]\]: a larger one is [4096], and a smaller one [1]. Without
      [size], with [`Auto], the display prints lines in turn.
    - [progress] says how progress shows (defaults to [`Auto]). [`Auto] is as
      [size] says. [`Tty] redraws the rows, on 80 columns by 24 rows without
      [size]. [`Plain], [`Quiet] and [`Json] print lines, whatever [size] says.
      The display takes none of what the program prints on [ppf]: with [`Quiet]
      it shows, and with [`Json] it breaks the lines of events. {!log} gives
      such a line to a task, and [Console_eio.run] takes the standard outputs.
    - [summary], if given, names the work on the summary line ({!val-summary}).
    - [view] (defaults to {!default_view}) is the region on a terminal, and the
      last frame printed when the display closes ({!type-view}, {!set_view}).
    - [keep] (defaults to [false]) keeps the tasks that ended in the region
      until the display closes, where they otherwise move to the scrollback with
      their top task ({!design}).
    - [layout] (defaults to {!Layout.default}) gives the pieces of each row
      redrawn on a terminal; the lines printed in turn have their own
      ({!Layout}).

    The current task is that of the innermost {!with_task} that runs. Under
    [Console_eio.run], it is that of the fiber that asks, or of the fibers it
    was forked from.

    The display is drawn in [ppf]'s theme ({!Theme.get}), read again each time
    it draws. It is in colour iff [ppf]'s style renderer is [`Ansi_tty], at
    [ppf]'s {!Color.val-depth}. *)

val redraw : t -> unit
(** [redraw display] draws the region again, so that the spinners turn and the
    timers run. [Console_eio.run] calls it ten times a second. A program that
    runs a display without [Console_eio.run] calls it a few times a second. The
    display also redraws itself when its tasks change, at most ten times a
    second. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output display f] prints what console does not see, such as the text
    of [print_string] or of a child process. It takes the region off the screen,
    calls [f] with the display's formatter, as wide as the terminal, and then
    draws the region again below what [f] printed, also when [f] raises.

    Output that [f] does not end with a line feed is ended, and the region is
    drawn from the first column of the row where [f] left the cursor, on rows it
    erases first. *)

val set_result : ?detail:string -> t -> (string, string) result -> unit
(** [set_result display result] says how the work ended. When [display] closes,
    it prints [result]'s message after a success or failure mark, and [detail]
    (if any) in the muted colour above it. With [Ok] the tasks still running
    succeed, and with [Error] they are cancelled. A second call does nothing. *)

val close : t -> unit
(** [close display] cancels the tasks still running, unless {!set_result} with
    [Ok] had them succeed. It takes the region off the screen, shows the cursor
    and prints the view's last frame ({!type-view}), then the result of
    {!set_result}. A view that raises then is dropped, and the default view's
    last frame printed in its place.

    A closed display ignores what it is told, so that a fiber that outlives it
    does not fail. Closing it again, {!redraw}, {!set_view} and {!set_result} do
    nothing, and [with_output display f] is [f ppf], with [ppf] the formatter of
    {!v}. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics, in an unspecified format. *)

(** {1:tasks Tasks}

    A task is {e current} in the function given to {!with_task} and, under
    [Console_eio.run], in the fibers that function starts. A task started where
    another is current goes under it, and what the program prints there is the
    current task's output ({!output}).

    A task that ended ignores what it is told. Once its display closed, every
    task has ended, and so has one that {!val-task} starts then. *)

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

    - [key], if given, names the task. While it runs, a later call with the same
      [key] returns the same task, wherever it is called from. The [label],
      [parent], [log_lines] and [transient] of that call are ignored.
    - [parent] (defaults to the current task, if any) draws the task under
      [parent]. A task started under a parent that ended takes the parent's
      ending.
    - [log_lines] is how many of the task's latest lines its window shows under
      its row while it runs (defaults to [8]).
    - [transient] (defaults to [false]) makes a task that shows only while it
      runs, and leaves neither a line of its own nor any of its output when it
      ends. With [`Quiet], where nothing else would show it, one that fails or
      is cancelled prints its line and its output as any other does.

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
    fails when [f] raises, with the message
    [label ^ ": " ^ Printexc.to_string e] for the exception [e] ({!fail}), which
    is raised again; but if [f] ended the task itself, that ending counts.

    When [key] names a task that runs, [with_task] is
    [within task (fun () -> f task)] for that task. It ends nothing, and the
    task ends as the call that started it says. *)

val within : task -> (unit -> 'a) -> 'a
(** [within task f] is [f ()] with [task] current while [f] runs and, under
    [Console_eio.run], in the fibers [f] starts. What [f] prints is [task]'s
    output, and a task [f] starts goes under [task].

    Where {!with_task} starts a task, [within] enters one that exists, so it
    ends nothing, even when [f] raises: [task] ends as it is told. *)

val root : t -> task
(** [root display] is the top task of [display], current where no other is. Its
    output goes above the rows at once and stays, and a task started within it
    has no parent, so [within (root display) f] prints what [f] prints above the
    rows, from inside a task too.

    The root never ends: {!succeed}, {!fail}, {!cancel}, {!set_label},
    {!set_count} and {!set_bytes} do nothing on it, as on a task that ended, so
    code given a task needs no case for the root. *)

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
    ({!output}), such as the lines of a command it runs. It is the one-line form
    of {!within}, for code where [task] is not current.

    - A tab goes to the next stop of eight columns from the start of its line.
    - The latest lines show in [task]'s window while it runs. All of them show
      under its line if it fails or is cancelled.
    - Without a terminal, each line is printed once, as it comes, after [task]'s
      label, and not again when [task] fails. With [`Quiet], where none is
      printed as it comes, all of them are printed under the line of a failure
      or a cancellation. *)

val succeed : ?message:string -> task -> unit
(** [succeed task] ends [task] as a success, with [message] in place of its
    label if given. A task ends once: the first of {!succeed}, {!fail} and
    {!cancel} counts, and the others do nothing. A task's mark is what the
    program says of it: a task that failed under [task] does not fail it.

    The tasks under [task] that still run end first, each with a line of its
    own: they succeed with it, and are cancelled when it fails or is cancelled.
*)

val fail : ?message:string -> task -> unit
(** [fail task] is like {!succeed} but ends [task] as a failure. *)

val cancel : ?message:string -> task -> unit
(** [cancel task] is like {!succeed} but ends [task] as cancelled. *)

(** {1:views Views}

    A view is the region of a display on a terminal, as a span ({!Span}). The
    display calls it each time it draws, with the terminal's width and the
    number of lines the region may take, draws the last lines that fit, each cut
    to the width, and redraws only those that changed. Any span goes in a view:
    a {!Table.to_span} above the tasks, trees side by side with {!Span.hcat},
    one under the other with {!Span.vcat}.

    {[
    let view display ~width ~height =
      let half = (min width 100 - 2) / 2 in
      let tree task =
        Console.Display.tree ~width:half ~height:(height - 1) task
      in
      match Console.Display.tasks display with
      | [ a; b ] ->
          Console.Span.(
            vcat [ hcat [ tree a; tree b ]; Console.Display.summary display ])
      | _ -> Console.Display.default_view display ~width ~height
    ]}

    - The view is drawn in [display]'s theme. A span keeps its styles, and a
      widget's span is laid out in that theme.
    - While [display] draws in colour, a frame drawn in a theme with a
      {!Theme.val-motion} moves, by the time since [display] started, and on a
      terminal without colour it stands still.
    - A view that raises is dropped. The call that was drawing it, such as
      {!redraw} or {!set_count}, draws the default view, then raises the
      exception again.
    - Without a terminal the view is drawn once, as its last frame, with
      [`Plain] alone ({!type-view}). *)

val set_view : t -> view -> unit
(** [set_view display view] draws the region of [display] with [view] from now
    on, in place of the view set before, and draws it at once. *)

val default_view : view
(** [default_view display ~width ~height] is
    [Span.vcat [tree ~width:w ~height:(height - Span.height s) (root display);
     s]] with [w = min width 100] and [s = summary ~width:w display]: the tree
    of every top task, then the summary line, at most [w] cells wide. *)

val tasks : t -> task list
(** [tasks display] are the top tasks of [display] in its region, in the order
    they started: those that run, and with [keep] those that ended. *)

val tree : ?width:int -> ?height:int -> task -> Span.t
(** [tree task] is the tree of [task] in the region: the row of [task], then the
    rows of the tasks under it, each under its parent with the guides of a
    {!Tree}, in the order they started. The tree of {!root} is those of the top
    tasks.

    - A running task's row is drawn by the {!Layout} of the display, its window
      under it; a task that ended is its line ({!Layout}), and a transient one
      leaves its place to the tasks under it.
    - [width] (defaults to the terminal's width, at most 100) is the width of
      each line, and [height] (defaults to no limit) the most lines it takes.
      When the tree does not fit, it folds as {!design} says.
    - In the last frame, once the display closes, the tree holds every task,
      none folded, each failure or cancellation followed by its output, and a
      line too wide goes on over the next ones. A label is never cut there: one
      too wide goes on over the next lines at the full width its guides and mark
      leave, and never in fewer than 12 cells, and its count and time go at the
      end of its last line, or give way as {!Layout} says. They are never
      squeezed.
    - The tree of a task that moved to the scrollback is {!Span.empty}. *)

val summary : ?width:int -> t -> Span.t
(** [summary ~width display] is the summary line of [display], if it has a name
    ({!v}), and {!Span.empty} otherwise: the mark of its state, its name, the
    time since it started and the count of the tasks that ended out of those
    started. On a terminal it is at most [width] cells (defaults to the
    terminal's width, at most 100), the time and the count giving way before the
    name. A view that wants more says it next to it:

    {[
    let view display ~width ~height =
      let open Console in
      let tree = Display.tree ~width ~height:(height - 1) (Display.root display)
      and summary = Display.summary ~width:(width - 12) display in
      Span.(vcat [ tree; summary ++ text "  3 warnings" ])
    ]} *)
