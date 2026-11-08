(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** How tasks show: rows, lines, failures alone or JSON.

    The tasks of a {!Task.root} show as rows redrawn in place, as lines printed
    in turn, as only what fails, or as JSON events, the choices that the
    [--progress] option of [docker build] gives. The rows are ordinary views:
    {!row} is the row of a task, {!tree} the tree of rows of a task and those
    under it, drawn with {!Tree.view}, {!summary} the line that says how far the
    work is, and {!view} all of them. {!show} connects a root to a {!Display} in
    a mode, and [Console_eio.run] does it for a program.

    The {{!design}design} of the rows and the {{!guarantees}guarantees} they
    keep close this page. *)

type t =
  [ `Auto
    (** The rows are redrawn in place on a terminal, and lines are printed in
        turn otherwise. *)
  | `Tty
    (** The rows are redrawn in place on any output, such as a pipe that a
        terminal recorder reads. *)
  | `Plain
    (** Lines are printed in turn, as on a pipe. A task prints a line when it
        starts, its output at once and a line when it ends, each naming the task
        by its path ({!design}). The view comes last, once: by default the
        summary line. The lines are in colour if the output has colour. *)
  | `Quiet
    (** Only what fails is printed: a task that fails or is cancelled prints its
        line, which names its path, then all its output under it. A task that
        succeeds prints nothing, and there are no rows and no summary. The
        output of the root is printed as it comes, and the message the work
        ended with last. *)
  | `Json
    (** Each event ({!Task.Event}) is one JSON object on a line of its own, and
        nothing else is printed: what the program prints on the display's
        formatter becomes [output] events with no task. *) ]
(** The type for progress modes, how the tasks of a root show. Constructors may
    be added, so a match on a progress mode needs a case for the others. *)

val all : (string * t) list
(** [all] is every progress mode under its name, [`Auto] first, which are the
    choices of a [--progress] option as [Cmdliner.Arg.enum all] reads them:
    [[("auto", `Auto); ("tty", `Tty); ("plain", `Plain); ("quiet", `Quiet);
     ("json", `Json)]]. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the progress mode of {!all} named [s]. The error message
    lists the names. *)

val to_string : t -> string
(** [to_string p] is the name of [p] in {!all}. *)

val pp : t Fmt.t
(** [pp] formats a progress mode as {!to_string} does. *)

(** {1:layout Row layout} *)

(** What a running task's row shows, left to right.

    A layout is pieces put together with {!Layout.(++)} and {!Layout.list}. The
    lines of the tasks that ended have pieces of their own, whatever the layout:
    those of {!Layout.default}, with the mark of how the task ended in place of
    the spinner, in the same columns, and the bar as far as the task went.

    {b Packed from the left.} {!Layout.default} packs a row from the left: the
    label, in a column as wide as the widest label; then the bar, which grows to
    fill the room, up to 40 cells; then the count, the rate and the elapsed
    time. The rest of the line stays empty. The bars of all rows start in one
    column and are as wide, one cell after the label column. A task under
    another is indented under it, behind the guides of its tree, and its label
    column ends where the others do, so its columns are those of every row. The
    spinners and the marks take as many cells as the widest of them, so the
    labels start in one column too.

    {b What gives way.} The count and the elapsed time say how far a task is, so
    they keep their columns until the label is cut as far as it can be. When a
    row does not fit its room, its bar first shrinks, down to 10 cells; then its
    pieces give way stage by stage, each piece at the stage {!Layout.give_way}
    sets, by default:
    + the rate and the time left;
    + the bar and the percentage;
    + the label, cut with an ellipsis to the room the others leave, down to 12
      cells;
    + the count of bytes, which gets shorter, such as [256M/2.0G], in a column
      of its own;
    + the elapsed time.

    Only once all of them have given way is the label cut further, with an
    ellipsis. From the third stage the label column takes the room the other
    pieces leave, so the count and the time stay right-aligned at the end of the
    row. A label wider than its column first takes the cells of the pieces after
    it that show nothing, such as the count of a task that counts nothing. A
    piece that gives way, or that shows nothing, takes with it the
    {{!Layout.const}separator} before it, or the one after it. The rows of a
    region share their columns, so they give way together; a task under another,
    whose label column is narrower by the guides of its tree, keeps as many
    fewer cells of label.

    {b Columns.} The label, the bar, the count and the rate each take a column
    that the rows of a root share, and the elapsed time a column of six cells; a
    row without a bar, a count or a rate leaves its column blank. The bar, the
    count and the rate columns are as wide as the kinds of progress seen so far
    need, and only widen; the label column is as wide as the labels the rows
    show:
    - The label column is as wide as the widest label drawn, behind the guides
      of its depth: that of each task that ended, its message if it has one, and
      the widest label each running task has drawn, which a task renamed at each
      step keeps while it runs, so that its row moves nothing sideways. A
      running row whose layout holds no label ({!Layout.choose}) does not count.
      A wider label widens the column in the first frame after its task starts,
      is renamed or ends. Once a running task ends, its labels count no more, so
      a label it was renamed from widens no row after it: the column narrows if
      that label was the widest. A top task that leaves for the scrollback keeps
      the label column as wide as it was printed, so that the rows printed after
      it line up with it.
    - The bar column opens with the first task that counts.
    - A count of items takes the cells its total needs, at least [999/999], so a
      display of items is as dense as its counts allow. A task of items with a
      larger total widens the count column when it starts counting.
    - The first task that counts bytes widens the count column at once to the
      widest count of bytes, [1023.99 KiB / 1023.99 TiB], and opens the rate
      column at the widest rate, [1023.99 MiB/s]. Later tasks of bytes, larger
      totals and first rates widen nothing.
    - No column but the label column narrows while the display is open.

    So a column widens at most once for each new kind of progress, for a count
    of items with more digits, or for a wider label: a display of bytes alone,
    or one whose tasks of bytes count from their start, never widens after its
    first frame of bytes except for a label, and one where a task of bytes comes
    among rows of items widens once. While no piece gives way and the label
    column keeps its width, each piece sits in the same columns on every row and
    in every frame.

    A program that wants the count, the rate and the time against the right edge
    puts a {!Layout.spacer} before them, which takes the room the bar leaves:

    {[
    let right_aligned =
      let space = Console.Progress.Layout.const (Console.View.text " ") in
      Console.Progress.Layout.(
        list
          [
            spinner;
            space;
            label ();
            spacer;
            bar ();
            space;
            count;
            space;
            rate;
            space;
            elapsed;
          ])
    ]} *)
module Layout : sig
  type t = Progress_layout.t
  (** The type for row layouts: at most one {!label} and one {!spacer}. *)

  val spinner : t
  (** [spinner] is the spinner of the row's theme ({!Spinner.view}), in the
      row's colour: the colour of the {!Theme.palette} the row takes. *)

  val label : ?style:Style.t -> unit -> t
  (** [label ()] is the task's label, in [style] (defaults to {!Style.bold}),
      padded to the label column. Short of room, it is cut with an ellipsis at
      the third stage, once the pieces of the first two have given way. *)

  val bar : ?width:int -> unit -> t
  (** [bar ()] is a progress bar ({!Bar.horizontal}) [width] cells wide or,
      without [width], growing to fill the room up to 40 cells and leaving the
      rest to a {!spacer}. No bar shows before the task has a count.

      @raise Invalid_argument if [width] is negative. *)

  val percent : t
  (** [percent] is the share of the work done, as a percentage. *)

  val count : t
  (** [count] is the count of items or bytes done and the total, such as [3/10],
      right-aligned in the column of the counts. *)

  val rate : t
  (** [rate] is the bytes per second of a task that counts bytes, right-aligned
      in the column of the rates. The rate weighs the past by the time since it
      was measured, not by how often the count is given, so a program that
      reports often and one that reports seldom show the same rate for the same
      work. *)

  val eta : t
  (** [eta] is the time the task has left at its rate, such as [ETA 1m05s], once
      it has a rate. *)

  val elapsed : t
  (** [elapsed] is the time since the task started, rounded, such as [6.4s],
      [1m05s] or [1h02m], right-aligned in a column of its own. *)

  val spacer : t
  (** [spacer] is the room the other pieces leave, at least one cell: the pieces
      after it sit against the right edge. *)

  val const : View.t -> t
  (** [const v] is [v] on every row, on one line. A [v] whose text is only
      blanks is a separator. It goes with the piece before or after it when that
      piece gives way or shows nothing, so a row never shows two gaps side by
      side. *)

  val custom : (Task.t -> View.t) -> t
  (** [custom f] is [f task] on the row of [task], on one line, drawn again at
      every frame. *)

  val choose : (Task.t -> t) -> t
  (** [choose f] is the layout [f task] on the row of [task], chosen again at
      every frame, so that tasks of different kinds show different pieces. A
      label of [f task] counts towards the one label of the layout it is put in,
      and so does a spacer: a row that would hold two raises [Invalid_argument]
      where it is drawn ({!Display}). *)

  val give_way : int -> t -> t
  (** [give_way n l] is [l] whose pieces give way at stage [n] when the row is
      short of room ({!Layout}). The label is cut at stage 3: a piece of stage 1
      or 2 gives way before it is, one of stage 3 as it is, and one of a later
      stage once it is cut to 12 cells.

      @raise Invalid_argument if [n] is less than [1]. *)

  val ( ++ ) : t -> t -> t
  (** [l0 ++ l1] is [l0] then [l1].

      @raise Invalid_argument if they hold two {!label}s or two {!spacer}s. *)

  val list : ?sep:t -> t list -> t
  (** [list ~sep ls] is [ls] in turn, [sep] between each two.

      @raise Invalid_argument if they hold two {!label}s or two {!spacer}s. *)

  val default : t
  (** [default] is a spinner, the label, a bar that grows, the count, the rate
      and the elapsed time, a space between each two, packed from the left
      ({!Layout}). *)
end

val pp_bytes : int64 Fmt.t
(** [pp_bytes] formats a count of bytes as {!Layout.count} and {!Layout.rate}
    show it: in bytes below 1 KiB, such as [512 B], and above in the largest
    binary unit that keeps it below 1024 once rounded to two decimals, such as
    [1.50 MiB]. A negative count is [0 B]. *)

val pp_duration : float Fmt.t
(** [pp_duration] formats a duration in seconds as {!Layout.elapsed} and
    {!summary} show it, rounded to its last digit: in tenths of a second below a
    minute, such as [6.4s], then in minutes and seconds, such as [1m05s], then
    in hours and minutes, such as [1h02m]. A negative or NaN duration is [0.0s].
*)

(** {1:views Task views} *)

val row : ?layout:Layout.t -> Task.t -> View.t
(** [row task] is the row of [task] in its room ({!View.fit}): drawn by [layout]
    (defaults to {!Layout.default}) while it runs, its pieces giving way to fit,
    and its line once it ended: its mark, then its label, bar, count, rate and
    time in the columns of the rows ({!Layout}). *)

val tree : ?layout:Layout.t -> Task.t -> View.t
(** [tree task] is the tree of [task] in its room ({!View.fit}): the row of
    [task], its window of output under it, then the trees of the tasks under it,
    each under its parent with the guides of a {!Tree.view} two cells in, in the
    order they started. The tree of a root is those of its top tasks.

    - In a room of a height, a tree too tall folds as {!design} says, and a task
      that ended is its line.
    - Without a height, as when the display closes or a task moves to the
      scrollback, the tree is whole: every task, none folded, each failure or
      cancellation followed by its output, and a label never cut: one too wide
      goes on over the next lines, never in fewer than 12 cells, its count and
      time at the end of its last line, or giving way as {!Layout} says.
    - A transient task that succeeded is replaced by the tasks under it, and a
      task forgotten once it moved to the scrollback is {!View.empty}. *)

val summary : Task.t -> View.t
(** [summary root] is the summary line of the work of [root], if its root has a
    label ({!Task.root}), and {!View.empty} otherwise: the mark of the state of
    the work, its label, the time since it started and the count of the tasks
    that ended out of those started. In a room, the time and the count give way
    before the label. The mark is the running mark until the root ends, then the
    worst of how the root and its tasks ended. *)

val view : ?layout:Layout.t -> Task.t -> View.t
(** [view root] is the tree of [root] above its summary, and the message the
    work ended with under it, if any, at most 100 cells wide: what a display
    shows by default. *)

(** {1:showing Showing tasks} *)

val show :
  ?progress:t ->
  ?layout:Layout.t ->
  ?view:View.t ->
  ?keep_ended:bool ->
  ?commit:(Task.t -> View.t option) ->
  ?plain:(Task.Event.t -> View.t option) ->
  ?delay:float ->
  ?size:(unit -> int * int) ->
  Format.formatter ->
  Task.t ->
  Display.t
(** [show ppf root] shows the tasks of [root] on [ppf] and is the display that
    does it. Closing it ends the work as cancelled if [root] still runs.

    - [size], if given, says that [ppf] is a terminal and gives its width and
      height, as {!Display.v} takes them.
    - [progress] (defaults to [`Auto]) is the mode: [`Auto] redraws the rows on
      a terminal, as [size] says, [`Tty] on 80 columns by 24 rows without
      [size], and [`Plain], [`Quiet] and [`Json] print lines, whatever [size]
      says. [`Plain] prints its last frame as wide as the terminal [size] gives,
      and whole without [size].
    - [view] (defaults to {!val-view}[ root]) is the region, redrawn in place,
      and the last frame printed when the display closes, with [`Plain] too. A
      view that raises is replaced by the default one, and the display raises
      its exception as {!Display} says.
    - [commit] (defaults to the {!tree} of the task) is what a top task that
      ended leaves in the scrollback, given the task as it ended, with the
      output it kept ({!Task.output}) even when it succeeded, and laid out when
      it moves there ({!design}); [None] leaves nothing.
    - [plain], if given, prints the program's own lines in place of those of the
      tasks when they print as lines, with [`Plain] or with [`Auto] off a
      terminal: [plain e] for each event [e] of a task, as wide as [size] says
      or whole, and nothing for [None]. The lines that belong to no task and the
      view's last frame print as they do without [plain]. [plain] is called only
      when the rows do not show, so a program learns from it whether they do.
    - [delay] (defaults to [0.]) is the seconds before the rows first show, as
      {!Display.v} takes it: work that ends sooner shows no rows, only its top
      tasks in the scrollback and the last frame.
    - [keep_ended] (defaults to [false]) keeps the tasks that ended in the
      region until the display closes, instead of moving them to the scrollback
      with their top task ({!design}). With [`Plain] it keeps them too, for the
      view's last frame and {!Task.children}, so the default view then ends with
      their whole tree.
    - [layout] (defaults to {!Layout.default}) draws the rows of the default
      [view] and of the default [commit]; a [view] or a [commit] given draws its
      own.

    The display captures only what the program prints on [ppf] through
    {!Display.with_output}. Other text printed on [ppf] shows as it is with
    [`Quiet], and breaks the lines of events with [`Json]. [Console_eio.run]
    captures the standard outputs. What {!Display.print} and
    {!Display.with_output} print belongs to no task: it goes above the rows, in
    turn with the lines, and with [`Json] it is an [output] event for each of
    its lines, without escape codes. *)

(** {1:design Design}

    On a terminal, the rows are the region of a {!Display}, and what leaves the
    region goes to its scrollback.

    - The summary is the last line of the default view, so the line that says
      how far the work is stays where the eye left it, below the rows.
    - A task that ends stays in the region, marked as it ended, under its
      parent, while its top task runs.
    - A top task that ended moves to the scrollback with every task under it, as
      its tree: each task once, under its parent, in the order they started, and
      a task that failed or was cancelled followed by its output. Tasks move by
      top task so that a task never lands above its parent and the tasks of one
      top task stay together, however their ends interleave. The tree is laid
      out, then the task forgotten, then the tree printed with the region drawn
      again below it, so it never shows twice.
    - A top task that succeeded waits in the region, marked, until anything else
      is printed to the scrollback, the region has no room left for it, or the
      display closes. The tasks that wait then move together, in the order they
      started, so that steps run one after another share their columns. A tree
      that holds a failure or a cancellation moves as its top task ends, after
      the tasks that wait.
    - With [keep_ended] ({!show}), no task moves: the tasks that ended stay in
      the region until the display closes, as in a view of trees side by side,
      where a column should not empty while the others run.
    - The output of the root, and a [Logs] message or a printed line from a
      fiber whose task ended, go to the scrollback at once. {!Task.log} drops a
      line given to a task that ended. A line wider than the terminal goes on
      over the next rows as the terminal wraps it, mid-word, as it would without
      the display, while a line after a mark, such as a warning, goes on under
      its text, broken between words.
    - Only running rows, their windows and the summary change: a task that ended
      keeps its line as it ended until it leaves the region, or folds to fit.
    - A change to the tasks is drawn at once if the last frame is a tenth of a
      second old, and with the next redraw otherwise, while what is printed to
      the scrollback is written at once, with the region as it is then. So a
      line printed just before a task starts may show for a tenth of a second
      above rows that lack the task.
    - Any change to the tasks may draw a frame, such as the end of a task that
      {!Task.run} ends before it returns. A view that reads the program's own
      state, such as a count of the steps done, shows it as it is then: change
      it inside the function given to {!Task.run}, before the task ends, so that
      no frame shows the task ended and the count not yet raised.
    - A tree too tall for its room first folds the tasks that ended under each
      task into one line, such as [12 done, 2 failed] after the mark of the
      worst ending among them, then the lines of its windows give way, the
      oldest of them first, whichever task's window holds it, then only its
      latest running tasks show, after a line such as [3 more running].
    - At close, the root ends cancelled if it runs, its tasks move as above,
      then the view's last frame is printed once, still, and stays: the summary
      alone in the default view, unless [keep_ended] kept the tasks.
    - The ended tasks kept in the region under a task are bounded: past about 1
      MiB, the oldest are dropped, and a line counts them.

    Without a terminal there is no region. The tasks print the lines of
    [`Plain]: one line when a task starts and one when it ends, in the order
    they happen, and at close the view's last frame comes once: the summary
    alone in the default view, since every task has printed its lines (unless
    [keep_ended] kept them), then the message the work ended with.

    The lines of tasks that run at once interleave on a pipe, so no line is
    placed by its indentation: each line a task prints, as it starts, as it ends
    and before each line of its output, names the task by its {e path}, the
    labels from its top task down to it, such as [linux > compile]. On a UTF-8
    formatter the separator is a small black right-pointing triangle, U+25B8, a
    glyph that no mark takes and that a label seldom holds; on another it is
    [>]. The lines of tasks start at the first column, and a line of output two
    columns in. A task's line when it ends says [failed] or [cancelled] when it
    did not succeed, since the output may have no colour. With [`Quiet], the
    line of a failure names its path too, and its output follows it, two columns
    in.

    A line of output that comes a second or more after the task's previous line,
    or after the task started, ends with that delay, muted, such as [+3.0s], so
    that a pause in the work shows where it happened.

    {1:guarantees Guarantees}

    Those of {!Display}, and on a terminal of [w] columns:
    - While no piece gives way ({!Layout}), the bars, counts, rates and timers
      of all rows sit in the same columns, packed from the left: the bars start
      one cell after the widest label, are as wide, at most 40 cells, and the
      rest of the line stays empty, within the first [min w 100] columns.
      Nothing in the region moves sideways unless the terminal width changes, a
      column widens, at most once for each new kind of progress or for a wider
      label, or the label column narrows as the task with the widest label ends
      ({!Layout}).
    - Short of room, the counts and the timers stay right-aligned in their
      columns: the rate, the bar and the percentage give way first, then the
      label is cut, down to 12 cells, and only then does the count of bytes get
      shorter and the timer go.
    - The rows of the top tasks that move to the scrollback together line up.
      Rows printed after them line up with them as far as the labels known then
      allow: a label column, once printed, never narrows.
    - A label cut to fit is cut between two characters and ends with an ellipsis
      that turns its style off. *)
