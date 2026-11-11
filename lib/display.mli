(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A view redrawn in place: the live region.

    A display draws a {!View.t} at the bottom of the normal output of the
    terminal. This part of the screen is its {e region}. The display redraws the
    region in place at each frame, in the room of the terminal and at the time
    of the frame ({!Room}). So its spinners turn, and the frames of a theme with
    motion move. Above the region is the {e scrollback}, where {!print} writes
    what is printed once. A display knows nothing about tasks: {!Progress.show}
    makes the display of the tasks of a root, and [Console_eio.run] runs it.

    The frame of the matrix theme, moving in a display with no task:

    {[
    let matrix ~now ~size =
      let table =
        Console.Table.view
          [ Console.Table.column (Console.View.text "Name") ]
          [ [ Console.View.text "Alice" ] ]
      in
      Console.Theme.set Fmt.stderr Console.Theme.matrix;
      let d = Console.Display.v ~size ~now Fmt.stderr table in
      Console.Display.redraw d;
      Console.Display.close d
    ]}

    {1:guarantees Guarantees}

    On a terminal of [w] columns and [h] rows:
    - No line that the display draws in its region is wider than [w], so no line
      wraps.
    - The region is never taller than [h - 1] lines, so a redraw never scrolls
      the terminal. A view taller than that loses its first lines. If this would
      cut a table or a panel, the table or panel loses all its lines, also
      inside a {!View.hcat}, so that no part of its frame shows. A tree loses
      its lines one by one.
    - The region starts at the first column of its own row, below any unfinished
      line that the program left before the display started.
    - Each change is written in one piece, as one synchronized update (mode
      2026). A terminal that supports this mode shows the update whole. The
      region is never shown half erased or half drawn, and only its rows change
      between two of its frames. On a pipe, no such escape is written.
    - A redraw that would change nothing writes nothing. A redraw where nothing
      that the view reads has changed computes nothing. A view that reads
      neither the time ({!Room.time}) nor the program's state is drawn again
      only when it is set, when something is printed above it, or when the size
      of the terminal changes. A view that reads the time is also drawn again
      when the time moves. A view of the program's state ({!View.fit}),
      including the tasks, is drawn again at every redraw, unless the display is
      told of each change to that state. The display of {!Progress.show} is told
      of each change to its tasks.
    - A line cut to fit is cut between two characters, never inside an escape
      code. It ends with an ellipsis that turns its style off.
    - After a resize, the old frame is erased. Its rows are counted the way a
      terminal that reflows its lines wraps them. On a terminal that does not
      reflow, such as xterm or GNU screen, narrowing below the width of the old
      lines also erases the lines above the region that a reflow would have
      filled.
    - The cursor is hidden while the display runs, and shown again when it
      closes. A display installs no signal handler. So if a program is
      interrupted before it closes its display, the cursor stays hidden.
      [Console_eio.run] closes its display on SIGINT and SIGTERM. A program that
      runs a display itself closes it from its own handler.
    - Every line that the display prints ends with ["\r\n"], so a tty with
      output processing off shows the same screen as one with it on.
    - The display is drawn in the theme of its formatter ({!Theme.get}), read
      again each time it draws. It is in colour iff the style renderer of the
      formatter is [`Ansi_tty], at its {!Color.val-depth}. The frames of a theme
      move only in colour, and stand still in the scrollback.

    Without a terminal there is no region. {!print} writes at once, and the view
    is printed once, as its last frame, when the display closes. Each of these
    lines starts on a line of its own: a line feed comes first when the last
    write of the program on the same formatter did not end its line. A write
    error on the output of the display ends that output and nothing else, as
    when a reader closes its pipe in [prog 2>&1 | head -1]. Without a terminal,
    console flushes {!Fmt.stdout} before each line it writes, so that [2>&1]
    keeps the order of the two outputs. A write error on standard output belongs
    to the program: the program's own writes meet it, as they would without
    console.

    A view that raises is dropped, and nothing is drawn in its place. The next
    call to {!redraw}, {!set_view} or {!close} raises its exception again. So
    drawing a frame, when a task changes or a line is printed, never raises in
    the other calls of the program. An exception from the size function, the
    output or a {!Progress.show} hook is kept the same way, and the view stays.
    {!close} raises it after it closes the display and restores the terminal.

    [Sys.Break], [Out_of_memory] and [Stack_overflow] are not kept. They
    propagate from the call that was drawing, including a {!Task} report, and
    the view stays.

    When one of these exceptions or a cancellation cuts an update short, and the
    output may have taken part of it, the update is not written again. The lines
    it was going to print go out whole with the next update, so part of them may
    show twice, on a terminal too. On a terminal, the region starts again on the
    row below the row where the cut left the cursor, and the screen is erased
    from that row down. What the cut update drew above that row stays on the
    screen, including rows of the region. Nothing that was above the region is
    erased.

    Any domain can call the functions of a display. The domain that made the
    display runs it. A call from another domain returns at once, and the owning
    domain does what it asks at its next call or redraw. So nothing shows while
    the owning domain does neither. In another domain, {!with_output} gives its
    function a buffer, and the text of the buffer goes out when the display next
    runs. {!close} does not wait for the close to happen. *)

type t = Display_core.t
(** The type for displays. *)

val v :
  ?size:(unit -> int * int) ->
  ?delay:float ->
  now:(unit -> float) ->
  Format.formatter ->
  View.t ->
  t
(** [v ~size ~now ppf view] draws [view] on [ppf], timed by the clock [now], in
    seconds.

    - With [size], [ppf] is a terminal of that width and height. The display
      reads the size again before each redraw. [view] is redrawn in place, in a
      room of at most [h - 1] lines of [w] cells. A width or a height is clamped
      to \[[1];[4096]\]. The region appears once [delay] seconds (defaults to
      [0.]) have passed on [now], so work that ends sooner draws no region.
      {!print} still writes at once, and {!close} prints the last frame. A
      negative [delay] is [0.]. With [nan] or [infinity], the region never
      shows, and only {!close} draws the view.
    - Without [size], [view] is printed once, still and whole, when the display
      closes. *)

val set_view : t -> View.t -> unit
(** [set_view d view] makes [d] draw [view] from now on, starting at once. *)

val print : t -> View.t -> unit
(** [print d v] prints [v] once, above the region, in the scrollback. [v] is
    laid out still, as wide as the terminal, with no height bound. It is written
    at once, with the region drawn again below it, in one update. Without a
    terminal, [v] is written at once, whole, in order with the other lines of
    the display. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output d f] prints output that console does not see, such as the text
    of [print_string] or of a child process. It takes the region off the screen
    and calls [f] with the formatter of the display, as wide as the terminal.
    Then it draws the region again below what [f] printed, also when [f] raises.

    If [f] does not end its output with a line feed, the output is ended. The
    region is then drawn from the first column of the row where [f] left the
    cursor, on rows that it erases first.

    The cursor stays hidden while [f] runs, as it is while the region shows. So
    a child process that [f] runs on the terminal, and that reads from it, shows
    no cursor unless it shows one itself. *)

val redraw : t -> unit
(** [redraw d] draws the view again, at the time of the call, so a view that
    reads the time ({!Room.time}) moves. [Console_eio.run] calls it ten times a
    second. A program that runs a display without [Console_eio.run] calls it a
    few times a second. It raises the exception of a view that raised since the
    last call ({!guarantees}). *)

val close : t -> unit
(** [close d] takes the region off the screen, shows the cursor, and prints the
    view once more as its last frame, which the scrollback keeps. This frame is
    still, as wide as the terminal (or whole without a terminal), with no height
    bound. A closed display ignores what it is told: closing it again,
    {!redraw}, {!set_view} and {!print} do nothing, and [with_output d f] is
    [f ppf], where [ppf] is the formatter given to {!v}. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics, in an unspecified format. *)
