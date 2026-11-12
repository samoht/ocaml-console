(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Show tasks as rows, as lines, as failures only, or as JSON.

    The tasks of a {!Task.root} show in one of four ways: as rows redrawn in
    place, as lines printed one after another, as failures only, or as JSON
    events. These are the choices of the [--progress] option of [docker build].
    The rows are ordinary views. {!row} is the row of one task. {!tree} is the
    tree of rows of a task and its subtasks, drawn with {!Tree.view}. {!summary}
    is the line that says how far the work is. {!view} puts them together.
    {!show} connects a root to a {!Display} in a given mode, and
    [Console_eio.run] does this for a program.

    The page ends with the {{!design}design} of the rows and the
    {{!guarantees}guarantees} they keep. *)

type t =
  [ `Auto
    (** Rows redrawn in place on a terminal. Lines printed one after another
        otherwise. *)
  | `Tty
    (** Rows redrawn in place on any output, for example a pipe read by a
        terminal recorder. *)
  | `Plain
    (** Lines printed one after another, as on a pipe. A task prints a line when
        it starts, then its output as it comes, then a line when it ends. Each
        line names the task by its path ({!design}). The view is printed once,
        at the end: by default, the summary line. The lines are in colour if the
        output has colour. *)
  | `Quiet
    (** Only failures are printed. A task that fails or is cancelled prints its
        line, which names its path, followed by all its output. A task that
        succeeds prints nothing. There are no rows and no summary. The output of
        the root is printed as it comes, and the final message of the work is
        printed last. *)
  | `Json
    (** Each event ({!Task.Event}) is one JSON object on its own line, and
        nothing else is printed. What the program prints on the display's
        formatter becomes [output] events with no task. *) ]
(** The type for progress modes: how the tasks of a root show. More constructors
    may be added, so a match on a progress mode needs a catch-all case. *)

val all : (string * t) list
(** [all] is every progress mode with its name, [`Auto] first. These are the
    choices of a [--progress] option, as [Cmdliner.Arg.enum all] reads them:
    [[("auto", `Auto); ("tty", `Tty); ("plain", `Plain); ("quiet", `Quiet);
     ("json", `Json)]]. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the progress mode of {!all} named [s]. The error message
    lists the valid names. *)

val to_string : t -> string
(** [to_string p] is the name of [p] in {!all}. *)

val pp : t Fmt.t
(** [pp] formats a progress mode as {!to_string} does. *)

(** {1:layout Row layout} *)

(** The pieces of a running task's row, from left to right.

    A layout is built from pieces with {!Layout.(++)} and {!Layout.list}. The
    line of a task that ended does not use the layout. It uses the pieces of
    {!Layout.default}, in the same columns, with the mark of how the task ended
    in place of the spinner, and the bar where the task stopped.

    {b Packed from the left.} {!Layout.default} packs a row from the left. First
    comes the label, in a column as wide as the widest label. Then comes the
    bar, which grows to fill the room, up to 40 cells. Then come the count, the
    rate and the elapsed time. The rest of the line stays empty. The bars of all
    rows start in the same column, one cell after the label column, and have the
    same width. A subtask is indented under its parent, behind the guides of its
    tree. Its label column ends at the same place as the others, so all rows
    share the same columns. The spinners and the marks all take the width of the
    widest one, so the labels also start in the same column.

    {b What gives way.} The count and the elapsed time show how far a task is,
    so they keep their columns until the label is cut as far as possible. When a
    row does not fit, its bar first shrinks, down to 10 cells. Then its pieces
    give way in stages. {!Layout.give_way} sets the stage of each piece. The
    default stages are:
    + the rate and the time left;
    + the bar and the percentage;
    + the label, cut with an ellipsis to the room the other pieces leave, down
      to 12 cells;
    + the count of bytes, which switches to a shorter form such as [256M/2.0G],
      in a column of its own;
    + the elapsed time.

    Only when all of them have given way is the label cut further, with an
    ellipsis. From the third stage on, the label column takes the room the other
    pieces leave, so the count and the time stay right-aligned at the end of the
    row. A label wider than its column first uses the cells of the later pieces
    that show nothing, such as the count of a task that counts nothing. When a
    piece gives way or shows nothing, the {{!Layout.const}separator} before it
    (or after it) goes too. The rows of a region share their columns, so they
    give way together. The label column of a subtask is narrower by the width of
    its tree guides, so its label has that many fewer cells.

    {b Columns.} The label, the bar, the count and the rate each have a column
    shared by all the rows of a root. The elapsed time has a column of six
    cells. A row without a bar, a count or a rate leaves that column blank. The
    bar, count and rate columns are as wide as the kinds of progress seen so far
    need, and they only widen. The label column is as wide as the labels the
    rows show:
    - The label column is as wide as the widest label drawn, plus the guides of
      its depth. For a task that ended, its message counts if it has one. The
      column never narrows while the display is open. A running row whose layout
      has no label ({!Layout.choose}) does not count. A wider label widens the
      column in the first frame after its task starts, is renamed or ends. If a
      task is renamed at each step to names too long for the column to stay
      fixed, give it a label of fixed width ({!Layout.label}).
    - The bar column opens with the first task that counts.
    - A count of items takes the cells its total needs, at least [999/999], so a
      display of items is as compact as its counts allow. A task of items with a
      larger total widens the count column when it starts counting.
    - The first task that counts bytes widens the count column at once to the
      widest count of bytes, [1023.99 KiB / 1023.99 TiB]. It also opens the rate
      column at the widest rate, [1023.99 MiB/s]. Later tasks of bytes, larger
      totals and first rates widen nothing.
    - No column narrows while the display is open.

    So a column widens at most once for each new kind of progress, for a count
    of items with more digits, or for a wider label. A display with only bytes,
    or whose tasks of bytes count from their start, never widens after its first
    frame of bytes, except for a label. A display where a task of bytes appears
    among rows of items widens once. While no piece gives way and the label
    column keeps its width, each piece stays in the same columns on every row
    and in every frame.

    To put the count, the rate and the time against the right edge, place a
    {!Layout.spacer} before them. It takes the room the bar leaves:

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
  (** The type for row layouts. A layout holds at most one {!label} and one
      {!spacer}. *)

  val spinner : t
  (** [spinner] is the spinner of the row's theme ({!Spinner.view}), in the
      row's colour: the colour of the {!Theme.palette} the row uses. *)

  val label : ?style:Style.t -> ?width:int -> unit -> t
  (** [label ()] is the task's label in [style] (defaults to {!Style.bold}),
      padded to the label column. The column is as wide as the widest label
      shown, and never narrows while the display is open. With [width], the
      column is exactly [width] cells, so nothing moves whatever the labels are.
      A longer label is then cut with an ellipsis, except in the scrollback,
      where it extends past the column on its own row. When the row is short of
      room, the label is cut with an ellipsis at the third stage, after the
      pieces of the first two stages have given way.

      @raise Invalid_argument if [width] is below 1. *)

  val bar : ?width:int -> unit -> t
  (** [bar ()] is a progress bar ({!Bar.horizontal}) of [width] cells. Without
      [width], the bar grows to fill the room, up to 40 cells, and leaves the
      rest to a {!spacer}. No bar shows before the task has a count.

      @raise Invalid_argument if [width] is negative. *)

  val percent : t
  (** [percent] is the share of the work done, as a percentage. *)

  val count : t
  (** [count] is the count of items or bytes done and the total, such as [3/10],
      right-aligned in the count column. *)

  val rate : t
  (** [rate] is the bytes per second of a task that counts bytes, right-aligned
      in the rate column. The rate weights past samples by their age, not by how
      often the count is reported. So a program that reports often and one that
      reports rarely show the same rate for the same work. *)

  val eta : t
  (** [eta] is the time left at the task's rate, such as [ETA 1m05s]. It shows
      once the task has a rate. *)

  val elapsed : t
  (** [elapsed] is the time since the task started, rounded, such as [6.4s],
      [1m05s] or [1h02m], right-aligned in a column of its own. *)

  val spacer : t
  (** [spacer] fills the room the other pieces leave, at least one cell. The
      pieces after it sit against the right edge. *)

  val const : View.t -> t
  (** [const v] is [v] on every row, on one line. If the text of [v] is only
      blanks, [v] is a separator. A separator goes away with the piece before or
      after it when that piece gives way or shows nothing. So a row never shows
      two gaps side by side. *)

  val custom : (Task.t -> View.t) -> t
  (** [custom f] is [f task] on the row of [task], on one line, computed again
      at every frame. *)

  val choose : (Task.t -> t) -> t
  (** [choose f] is the layout [f task] on the row of [task], chosen again at
      every frame, so tasks of different kinds can show different pieces. A
      label in [f task] counts towards the one label allowed in the enclosing
      layout, and so does a spacer. A row that would hold two of either raises
      [Invalid_argument] when it is drawn ({!Display}). *)

  val give_way : int -> t -> t
  (** [give_way n l] is [l] with its pieces giving way at stage [n] when the row
      is short of room ({!Layout}). The label is cut at stage 3. A piece of
      stage 1 or 2 gives way before the label is cut. A piece of stage 3 gives
      way when the label is cut. A piece of a later stage gives way after the
      label is cut to 12 cells.

      @raise Invalid_argument if [n] is less than [1]. *)

  val ( ++ ) : t -> t -> t
  (** [l0 ++ l1] is [l0] then [l1].

      @raise Invalid_argument if they hold two {!label}s or two {!spacer}s. *)

  val list : ?sep:t -> t list -> t
  (** [list ~sep ls] is the layouts [ls] in order, with [sep] between each two.

      @raise Invalid_argument if they hold two {!label}s or two {!spacer}s. *)

  val default : t
  (** [default] is a spinner, the label, a growing bar, the count, the rate and
      the elapsed time, with a space between each two, packed from the left
      ({!Layout}). *)
end

val pp_bytes : int64 Fmt.t
(** [pp_bytes] formats a count of bytes as {!Layout.count} and {!Layout.rate}
    show it. Below 1 KiB it is in bytes, such as [512 B]. Above, it uses the
    largest binary unit that keeps the value below 1024 after rounding to two
    decimals, such as [1.50 MiB]. A negative count is [0 B]. *)

val pp_duration : float Fmt.t
(** [pp_duration] formats a duration in seconds as {!Layout.elapsed} and
    {!summary} show it, rounded to its last digit. Below a minute it is in
    tenths of a second, such as [6.4s]. Then it is in minutes and seconds, such
    as [1m05s], then in hours and minutes, such as [1h02m]. A negative or NaN
    duration is [0.0s]. *)

(** {1:views Task views} *)

val row : ?layout:Layout.t -> Task.t -> View.t
(** [row task] is the row of [task], fitted to its room ({!View.fit}). While
    [task] runs, [layout] (defaults to {!Layout.default}) draws it, and its
    pieces give way to fit. Once [task] ended, the row is its line: its mark,
    then its label, bar, count, rate and time, in the columns of the rows
    ({!Layout}). The bar of a task that ended is muted if the task succeeded. It
    has the failure colour if the task failed, and the warning colour if it was
    cancelled. *)

val tree : ?layout:Layout.t -> Task.t -> View.t
(** [tree task] is the tree of [task], fitted to its room ({!View.fit}). It
    shows the row of [task], the window of its output, then the trees of its
    subtasks in the order they started. Each subtree sits under its parent, two
    cells in, with the guides of a {!Tree.view}. The tree of a root is the trees
    of its top tasks.

    - In a room with a height, a tree that is too tall folds as {!design}
      describes, and a task that ended shows as its line.
    - Without a height, the tree is whole. This is the case when the display
      closes, or when a task moves to the scrollback. Every task shows, none is
      folded, and each failure or cancellation is followed by its output. A
      label is never cut. A label too wide continues on the next lines, in at
      least 12 cells. Its count and time go at the end of its last line, or give
      way as {!Layout} describes.
    - A transient task that succeeded is replaced by its subtasks. A task that
      was forgotten after it moved to the scrollback is {!View.empty}. *)

val summary : Task.t -> View.t
(** [summary root] is the summary line of the work of [root] if its root has a
    label ({!Task.root}), and {!View.empty} otherwise. The line shows a mark for
    the state of the work, its label, the time since it started, and the number
    of tasks that ended out of those started. When room is short, the time and
    the count give way before the label. The mark is the running mark until the
    root ends. After that, it is the worst outcome among the root and its tasks.
*)

val view : ?layout:Layout.t -> Task.t -> View.t
(** [view root] is the tree of [root] above its summary, then the final message
    of the work, if any. The view is at most 100 cells wide. A display shows it
    by default. *)

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
    does it. Closing the display ends the work as cancelled if [root] still
    runs.

    - [size], if given, says that [ppf] is a terminal and gives its width and
      height, as {!Display.v} takes them.
    - [progress] (defaults to [`Auto]) is the mode. [`Auto] redraws the rows on
      a terminal, as [size] tells. [`Tty] redraws them, on 80 columns by 24 rows
      if [size] is not given. [`Plain], [`Quiet] and [`Json] print lines,
      whatever [size] says. [`Plain] prints its last frame at the terminal width
      that [size] gives, and whole without [size].
    - [view] (defaults to {!val-view}[ root]) is the region redrawn in place. It
      is also the last frame printed when the display closes, with [`Plain] too.
      If [view] raises, the default view replaces it, and the display raises the
      exception as {!Display} describes.
    - [commit] (defaults to the {!tree} of the task) is what a top task that
      ended leaves in the scrollback. It receives the task as it ended, with the
      output it kept ({!Task.output}), even if it succeeded. It is laid out when
      the task moves there ({!design}). [None] leaves nothing.
    - [plain], if given, prints the program's own lines in place of the lines of
      the tasks, when tasks print as lines: with [`Plain], or with [`Auto] off a
      terminal. For each event [e] of a task it prints [plain e], at the width
      [size] gives or whole, and nothing for [None]. Lines that belong to no
      task, and the view's last frame, print as they do without [plain]. [plain]
      is called only when the rows do not show, so a program can learn from it
      whether they show.
    - [delay] (defaults to [0.]) is the number of seconds before the rows first
      show, as {!Display.v} takes it. Work that ends sooner shows no rows: only
      its top tasks in the scrollback, and the last frame.
    - [keep_ended] (defaults to [false]) keeps the tasks that ended in the
      region until the display closes. Without it, they move to the scrollback
      with their top task ({!design}). With [`Plain], [keep_ended] also keeps
      them, for the view's last frame and for {!Task.children}. So the default
      view then ends with their whole tree.
    - [layout] (defaults to {!Layout.default}) draws the rows of the default
      [view] and of the default [commit]. A [view] or a [commit] given by the
      caller draws its own rows.

    The display captures only what the program prints on [ppf] through
    {!Display.with_output}. Other text printed on [ppf] shows unchanged with
    [`Quiet], and breaks the event lines with [`Json]. [Console_eio.run]
    captures the standard outputs. What {!Display.print} and
    {!Display.with_output} print belongs to no task. It goes above the rows,
    between the lines of the tasks. With [`Json], each of its lines is an
    [output] event, without escape codes. *)

(** {1:design Design}

    On a terminal, the rows are the region of a {!Display}. What leaves the
    region goes to its scrollback.

    - The summary is the last line of the default view. So the line that shows
      how far the work is stays in the same place, below the rows.
    - A task that ends stays in the region under its parent, marked with how it
      ended, while its top task runs.
    - A top task that ended moves to the scrollback with all its subtasks, as
      its tree. Each task appears once, under its parent, in the order they
      started. Each task that failed or was cancelled is followed by its output.
      Tasks move by top task, so a task never lands above its parent, and the
      tasks of one top task stay together, even when their ends interleave. The
      tree is laid out first, then the task is forgotten, then the tree is
      printed and the region is drawn again below it. So the tree never shows
      twice.
    - A top task that ended waits in the region, marked, until one of these
      happens: something else is printed to the scrollback, the region has no
      room left for it, or the display closes. The waiting tasks then move
      together, in the order they started, so steps that ran one after another
      share their columns. A tree that holds a failure or a cancellation with
      output moves as soon as its top task ends, after the waiting tasks, so its
      output shows at once.
    - With [keep_ended] ({!show}), no task moves. Tasks that ended stay in the
      region until the display closes. This suits a view of trees side by side,
      where one column should not empty while the others run.
    - The output of the root goes to the scrollback at once. So does a [Logs]
      message or a printed line from a fiber whose task ended. {!Task.log} drops
      a line given to a task that ended. A line wider than the terminal wraps
      onto the next rows mid-word, as the terminal wraps it without the display.
      A line after a mark, such as a warning, wraps under its own text, broken
      between words.
    - Only running rows, their windows and the summary change. A task that ended
      keeps its line until it leaves the region or folds to fit.
    - A change to the tasks is drawn at once if the last frame is a tenth of a
      second old, and with the next redraw otherwise. What is printed to the
      scrollback is written at once, with the region as it is at that moment. So
      a line printed just before a task starts may show for a tenth of a second
      above rows that do not have the task yet.
    - Any change to the tasks may draw a frame. For example, {!Task.run} ends
      the task before it returns, and that end may draw a frame. A view that
      reads the program's own state, such as a count of the steps done, shows
      that state as it is at the frame. So change that state inside the function
      given to {!Task.run}, before the task ends. Then no frame shows the task
      as ended with the count not yet updated.
    - A tree too tall for its room folds in three steps. First, the tasks that
      ended under each task fold into one line, such as [12 done, 2 failed],
      after the mark of the worst outcome among them. Next, the lines of the
      output windows give way, oldest first, whichever task's window holds them.
      Last, only the latest running tasks show, after a line such as
      [3 more running].
    - At close, the root ends as cancelled if it still runs, and its tasks move
      as above. Then the view's last frame is printed once and stays. In the
      default view this is the summary alone, unless [keep_ended] kept the
      tasks.
    - The tasks that ended and are kept in the region under a task are bounded.
      Past about 1 MiB, the oldest are dropped, and a line counts them.

    Without a terminal there is no region. The tasks print the lines of
    [`Plain]: one line when a task starts and one when it ends, in the order
    these happen. At close, the view's last frame comes once, then the final
    message of the work. In the default view, the last frame is the summary
    alone, since every task has already printed its lines (unless [keep_ended]
    kept them).

    The lines of tasks that run at the same time interleave on a pipe, so the
    indentation cannot show which task a line belongs to. Instead, every line a
    task prints names the task by its {e path}: the line when it starts, the
    line when it ends, and each line of its output. The path is the labels from
    its top task down to the task, such as [linux > compile]. On a UTF-8
    formatter the separator is a small black right-pointing triangle, U+25B8. No
    mark uses this glyph, and labels rarely contain it. On other formatters the
    separator is [>]. The lines of tasks start at the first column, and the
    lines of output two columns in. When a task did not succeed, its last line
    says [failed] or [cancelled], since the output may have no colour. With
    [`Quiet], the line of a failure also names its path, and its output follows
    it, two columns in.

    A line of output that comes a second or more after the task's previous line,
    or after the task started, ends with that delay, muted, such as [+3.0s].
    This shows where the work paused.

    {1:guarantees Guarantees}

    Those of {!Display}, plus the following, on a terminal of [w] columns:
    - While no piece gives way ({!Layout}), the bars, counts, rates and timers
      of all rows sit in the same columns, packed from the left. The bars start
      one cell after the widest label and have the same width, at most 40 cells.
      The rest of the line stays empty. All of this fits in the first
      [min w 100] columns. Nothing in the region moves sideways unless the
      terminal width changes or a column widens. A column widens at most once
      for each new kind of progress or for a wider label ({!Layout}), and never
      if the label column has a fixed width.
    - When room is short, the counts and the timers stay right-aligned in their
      columns. The rate, the bar and the percentage give way first. Then the
      label is cut, down to 12 cells. Only then does the count of bytes get
      shorter and the timer go.
    - The rows of top tasks that move to the scrollback together line up. Rows
      printed after them line up with them as far as the labels known at that
      time allow.
    - A label cut to fit is cut between two characters. It ends with an ellipsis
      that turns its style off. *)
